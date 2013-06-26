--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.LLVM.SExprParser
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Parses a file of instruction patterns formatted as S-expressions. The
-- instruction patterns consists of a set of constraints and one or more
-- patterns expressed as LLVM IR code.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.LLVM.SExprParser (
      parsePatterns
    ) where

import Language.InstructionSelection.Patterns.LLVM.Base
import Language.InstructionSelection.OpTypes (CompareOp (..))
import Language.InstructionSelection.Utils (Range (..))
import Text.ParserCombinators.Parsec
import Data.String.Utils
import Debug.Trace



parsePatterns :: String -> Either ParseError [Instruction]
parsePatterns input = parse pInstructions "" input

pInstructions :: GenParser Char st [Instruction]
pInstructions =
  do instructions <- many pInstruction
     eof
     return instructions

pInstruction :: GenParser Char st Instruction
pInstruction = labeledData "instruction" pInstruction'

pInstruction' :: GenParser Char st Instruction
pInstruction' =
  do pWhitespace
     assemblyStr <- pAssemblyStr
     patterns    <- many pPattern
     return (Instruction assemblyStr patterns)

pAssemblyStr :: GenParser Char st AssemblyString
pAssemblyStr =
  do str <- many1 (noneOf $ whitespace ++ "(")
     return (AssemblyString str)

pPattern :: GenParser Char st Pattern
pPattern = labeledData "pattern" pPattern'

pPattern' :: GenParser Char st Pattern
pPattern' =
  do constraints <- labeledData "constraints" pAllConstraints
     statements  <- labeledData "code" pAllStatements
     return (Pattern statements constraints)

pAllConstraints :: GenParser Char st [Constraint]
pAllConstraints = many pConstraint

pConstraint :: GenParser Char st Constraint
pConstraint = pParens pConstraint'

pConstraint' :: GenParser Char st Constraint
pConstraint' =
      try pAllocConstraint
  <|> try pImmConstraint
  <|> try pZimmConstraint
  <|> try pAliasConstraint
  <|> try pRelAddressConstraint
  <|> try pAbsAddressConstraint
  <|> try pAssertConstraint

pAllocConstraint :: GenParser Char st Constraint
pAllocConstraint =
  do string "allocate-in"
     pWhitespace
     storage <- pAnyStorage
     pWhitespace
     space <- pAnyStorageSpace
     pWhitespace
     return (AllocateIn storage space)

pImmConstraint :: GenParser Char st Constraint
pImmConstraint =
  do string "imm"
     pWhitespace
     lower <- pInt
     pWhitespace
     upper <- pInt
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (ImmediateRange imm (Range lower upper))

pZimmConstraint :: GenParser Char st Constraint
pZimmConstraint =
  do string "zimm"
     pWhitespace
     lower <- pInt
     pWhitespace
     upper <- pInt
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (ImmediateRangeNoZero imm (Range lower upper))

pAliasConstraint :: GenParser Char st Constraint
pAliasConstraint =
  do string "alias"
     pWhitespace
     temp <- pTemporary
     pWhitespace
     reg <-      try (do pNoValue
                         return Nothing)
            <|> (do rg <- pRegister
                    return (Just rg))
     pWhitespace
     return (Alias temp reg)

pRelAddressConstraint :: GenParser Char st Constraint
pRelAddressConstraint =
  do string "rel-address"
     pWhitespace
     lower <- pInt
     pWhitespace
     upper <- pInt
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (RelAddressConstraint imm (MemoryClass "local" (Range lower upper)))

pAbsAddressConstraint :: GenParser Char st Constraint
pAbsAddressConstraint =
  do string "abs-address"
     pWhitespace
     lower <- pInt
     pWhitespace
     upper <- pInt
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (AbsAddressConstraint imm (MemoryClass "local" (Range lower upper)))

pAssertConstraint :: GenParser Char st Constraint
pAssertConstraint =
  do string "assert"
     expr <- pAssertExpression
     return (Assert expr)

pAssertExpression :: GenParser Char st AssertExpression
pAssertExpression =
      try pContainsAssertExpr
  <|> try pCompareAssertExpr
  <|> try pRegFlagAssertExpr
  <|> try pNotAssertExpr
  <|> try pTrueFalseAssertExpr
  <|> try pImmediateAssertExpr

pContainsAssertExpr :: GenParser Char st AssertExpression
pContainsAssertExpr = pParens pContainsAssertExpr'

pContainsAssertExpr' :: GenParser Char st AssertExpression
pContainsAssertExpr' =
  do string "contains?"
     regclass <- pParens pPrefixedRegisterClass
     reg      <- pRegister
     return (ContainsExpr reg regclass)

pCompareAssertExpr :: GenParser Char st AssertExpression
pCompareAssertExpr = pParens pCompareAssertExpr'

pCompareAssertExpr' :: GenParser Char st AssertExpression
pCompareAssertExpr' =
  do string "icmp"
     pWhitespace
     cmpOp <- pCompareOp
     pWhitespace
     _ <- pSymbolWidth
     pWhitespace
     data1 <- pAnyData
     data2 <- pAnyData
     return (CompareExpr cmpOp data1 data2)

pRegFlagAssertExpr :: GenParser Char st AssertExpression
pRegFlagAssertExpr = pParens pRegFlagAssertExpr'

pRegFlagAssertExpr' :: GenParser Char st AssertExpression
pRegFlagAssertExpr' =
  do regFlag <- pRegisterFlag'
     return (RegFlagExpr regFlag)

pNotAssertExpr :: GenParser Char st AssertExpression
pNotAssertExpr = pParens pNotAssertExpr'

pNotAssertExpr' :: GenParser Char st AssertExpression
pNotAssertExpr' =
  do string "not"
     expr <- pAssertExpression
     return (NotExpr expr)

pTrueFalseAssertExpr :: GenParser Char st AssertExpression
pTrueFalseAssertExpr =
  do int <- pConstant
     if int == (ConstIntValue 0)
        then return FalseExpr
        else return TrueExpr

pImmediateAssertExpr :: GenParser Char st AssertExpression
pImmediateAssertExpr =
  do pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (ImmediateExpr imm)

pAllStatements :: GenParser Char st [Statement]
pAllStatements = many pStatement

pStatement :: GenParser Char st Statement
pStatement =
  do pParens pData -- TODO: fix
     return DummyStmt

pSymbolWidth :: GenParser Char st Integer
pSymbolWidth =
  do (ConstIntValue int) <- pConstant
     return int

pConstant :: GenParser Char st ConstantValue
pConstant = pParens pConstant'

pConstant' :: GenParser Char st ConstantValue
pConstant' =
  do string "constant"
     pWhitespace
     string "?"
     pWhitespace
     int <- pInt
     return (ConstIntValue int)

pTemporary :: GenParser Char st Temporary
pTemporary = labeledData "tmp" temporary'

temporary' :: GenParser Char st Temporary
temporary' =
  do int <- many1 digit
     return (Temporary (read int))

pSymbol :: GenParser Char st String
pSymbol = many1 alphaNum

pInt :: GenParser Char st Integer
pInt =
  do     try (do string "-"
                 num <- pInt'
                 return (num * (-1)))
     <|> pInt'

pInt' :: GenParser Char st Integer
pInt' =
  do int <- many1 digit
     return (read int)

pImmediateSymbol :: GenParser Char st ImmediateSymbol
pImmediateSymbol =
  do symbol <- pSymbol
     return (ImmediateSymbol symbol)

pRegisterFlag :: GenParser Char st RegisterFlag
pRegisterFlag = pParens pRegisterFlag'

pRegisterFlag' :: GenParser Char st RegisterFlag
pRegisterFlag' =
  do string "reg-flag"
     pWhitespace
     flag <- pRegisterFlagSymbol
     pWhitespace
     reg <- pRegister
     return (RegisterFlag flag reg)

pRegisterFlagSymbol :: GenParser Char st RegisterFlagSymbol
pRegisterFlagSymbol =
  do symbol <- pSymbol
     return (RegisterFlagSymbol symbol)

pRegister :: GenParser Char st Register
pRegister =
      try (do symbol <- pRegisterSymbol
              return (RegBySymbol symbol))
  <|> try (do symbol <- pPrefixedRegisterSymbol
              return (RegBySymbol symbol))
  <|> try (do temp <- pTemporary
              return (RegByTemporary temp))

pRegisterSymbol :: GenParser Char st RegisterSymbol
pRegisterSymbol =
  do symbol <- pSymbol
     return (RegisterSymbol symbol)

pPrefixedRegisterSymbol :: GenParser Char st RegisterSymbol
pPrefixedRegisterSymbol = pParens pPrefixedRegisterSymbol'

pPrefixedRegisterSymbol' :: GenParser Char st RegisterSymbol
pPrefixedRegisterSymbol' =
  do string "register"
     pWhitespace
     pRegisterSymbol

pDataSpace :: GenParser Char st DataSpace
pDataSpace =
      try (do regClass <- pRegisterClass
              return (DSRegisterClass regClass))
     -- TODO: add memory class

pRegisterClass :: GenParser Char st RegisterClass
pRegisterClass =
  do reg <- many1 (try alphaNum <|> try (char '_'))
     return (RegisterClass reg)

pPrefixedRegisterClass :: GenParser Char st RegisterClass
pPrefixedRegisterClass =
  do string "register-class"
     pWhitespace
     pRegisterClass

pAnyData :: GenParser Char st AnyData
pAnyData =
      try (do const <- pConstant
              return (ADConstant const))
  <|> try (do temp <- pTemporary
              return (ADTemporary temp))
  <|> try (do reg <- pRegister
              return (ADRegister reg))
  <|> try (do flag <- pRegisterFlag
              return (ADRegisterFlag flag))
  <|> try (do imm <- pImmediateSymbol
              return (ADImmediate imm))
  <|> try (do pNoValue
              return (ADNoValue))

pNoValue :: GenParser Char st ()
pNoValue =
  do pWhitespace
     string "no-value"
     pWhitespace
     return ()

pAnyStorage :: GenParser Char st AnyStorage
pAnyStorage =
      try (do temp <- pTemporary
              return (ASTemporary temp))
  <|> try (do reg <- pRegister
              return (ASRegister reg))
  <|> try (do flag <- pRegisterFlag
              return (ASRegisterFlag flag))

pAnyStorageSpace :: GenParser Char st AnyStorageSpace
pAnyStorageSpace =
      try (do space <- pDataSpace
              return (ASSDataSpace space))
  <|> try (do flag <- pRegisterFlag
              return (ASSRegisterFlag flag))


pCompareOp :: GenParser Char st CompareOp
pCompareOp =
      try (do string "slt"
              return ISCmpLT)
  <|> try (do string "eq"
              return ICmpEq)

labeledData :: String -> GenParser Char st a -> GenParser Char st a
labeledData str p = pParens (labeledData' str p)

labeledData' :: String -> GenParser Char st a -> GenParser Char st a
labeledData' str p =
  do string str
     pWhitespace
     result <- p
     return result

pData :: GenParser Char st String
pData =
  do list <- many1 morePData
     return $ istrip $ Prelude.foldr (++) [] list

morePData :: GenParser Char st String
morePData =
      try (do nested <- pParens pData
              return $ ['('] ++ nested ++ [')'])
  <|> do pWhitespace
         many1 (noneOf "()")

pWhitespace :: GenParser Char st String
pWhitespace = many (oneOf whitespace)

pParens :: GenParser Char st a -> GenParser Char st a
pParens c =
  do pWhitespace
     result <- between (char '(') (char ')') (pInnerParens c)
     pWhitespace
     return result

pInnerParens :: GenParser Char st a -> GenParser Char st a
pInnerParens c =
  do pWhitespace
     result <- c
     pWhitespace
     return result

istrip :: String -> String
istrip = join " " . filter (\x -> x /= "") . split " "

whitespace = " \r\n\t"
