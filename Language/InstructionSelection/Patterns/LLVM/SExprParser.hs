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
  do pWhitespace
     (    try pAllocConstraint
      <|> try pImmConstraint
      <|> try pZimmConstraint
      <|> try pAliasConstraint
      <|> try pAssertConstraint)

pAllocConstraint :: GenParser Char st Constraint
pAllocConstraint =
  do string "allocate-in"
     pWhitespace
     symbol <- pRegisterSymbol
     pWhitespace
     regclass <- pRegisterClass
     pWhitespace
     return (AllocateIn symbol regclass)

pImmConstraint :: GenParser Char st Constraint
pImmConstraint =
  do string "imm"
     pWhitespace
     lower <- many1 digit
     pWhitespace
     upper <- many1 digit
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (ImmediateRange imm (Range (read lower) (read upper)))

pZimmConstraint :: GenParser Char st Constraint
pZimmConstraint =
  do string "zimm"
     pWhitespace
     lower <- many1 digit
     pWhitespace
     upper <- many1 digit
     pWhitespace
     imm <- pImmediateSymbol
     pWhitespace
     return (ImmediateRangeNoZero imm (Range (read lower) (read upper)))

pAliasConstraint :: GenParser Char st Constraint
pAliasConstraint =
  do string "alias"
     pWhitespace
     temp1 <- pTemporary
     pWhitespace
     temp2 <- pTemporary
     pWhitespace
     return (Alias temp1 temp2)

pAssertConstraint :: GenParser Char st Constraint
pAssertConstraint =
  do string "assert"
     expr <- pParens pAssertExpression
     return (Assert expr)

pAssertExpression :: GenParser Char st AssertExpression
pAssertExpression =
      try pContainsAssert
  <|> try pCompareAssert
  <|> try pRegFlagAssert
  <|> try pNotAssert

pContainsAssert :: GenParser Char st AssertExpression
pContainsAssert =
  do string "contains?"
     regclass <- pParens pPrefixedRegisterClass
     reg      <- pRegisterSymbol
     return (ContainsExpr reg regclass)

pCompareAssert :: GenParser Char st AssertExpression
pCompareAssert =
  do string "icmp"
     pWhitespace
     cmpOp <- pCompareOp
     pWhitespace
     _ <- pSymbolWidth
     pWhitespace
     data1 <- pAnyData
     data2 <- pAnyData
     return (CompareExpr cmpOp data1 data2)

pRegFlagAssert :: GenParser Char st AssertExpression
pRegFlagAssert =
  do regFlag <- pRegisterFlag
     return (RegFlagExpr regFlag)

pNotAssert :: GenParser Char st AssertExpression
pNotAssert =
  do string "not"
     expr <- pParens pAssertExpression
     return (NotExpr expr)

pAllStatements :: GenParser Char st [Statement]
pAllStatements = many pStatement

pStatement :: GenParser Char st Statement
pStatement = pParens pData -- TODO: fix

pSymbolWidth :: GenParser Char st Integer
pSymbolWidth = pConstant

pConstant :: GenParser Char st Integer
pConstant = pParens pConstant'

pConstant' :: GenParser Char st Integer
pConstant' =
  do string "constant"
     pWhitespace
     string "?"
     pWhitespace
     int <- many1 digit
     return (read int)

--variable :: GenParser Char st Variable
--variable =
--  do (    try (do temp <- temporary
--                  return (VTemporary temp))
--      <|> try (do param <- parameter
--                  return (VParameter param)))

pTemporary :: GenParser Char st Temporary
pTemporary = labeledData "tmp" temporary'

temporary' :: GenParser Char st Temporary
temporary' =
  do int <- many1 digit
     return (Temporary (read int))

pSymbol :: GenParser Char st String
pSymbol = many1 alphaNum

pImmediateSymbol :: GenParser Char st ImmediateSymbol
pImmediateSymbol =
  do symbol <- pSymbol
     return (ImmediateSymbol symbol)

pRegisterFlag :: GenParser Char st RegisterFlag
pRegisterFlag =
  do string "reg-flag"
     pWhitespace
     flag <- pRegisterFlagSymbol
     pWhitespace
     reg <- (    try pRegisterSymbol
             <|> try pPrefixedRegisterSymbol)
     return (RegisterFlag flag reg)

pRegisterFlagSymbol :: GenParser Char st RegisterFlagSymbol
pRegisterFlagSymbol =
  do symbol <- pSymbol
     return (RegisterFlagSymbol symbol)

pRegisterSymbol :: GenParser Char st RegisterSymbol
pRegisterSymbol =
  do symbol <- pSymbol
     return (RegisterSymbol symbol)

pPrefixedRegisterSymbol :: GenParser Char st RegisterSymbol
pPrefixedRegisterSymbol =
  do string "register"
     pWhitespace
     pRegisterSymbol

pRegisterClass :: GenParser Char st DataSpace
pRegisterClass =
  do reg <- many1 (try alphaNum <|> try (char '_'))
     return (RegisterClass reg)

pPrefixedRegisterClass :: GenParser Char st DataSpace
pPrefixedRegisterClass =
  do string "register-class"
     pWhitespace
     pRegisterClass

pAnyData :: GenParser Char st AnyData
pAnyData =
      try (do const <- pConstant
              return (ADConstant (ConstIntValue const)))
  <|> try (do temp <- pTemporary
              return (ADTemporary temp))
  <|> try (do reg <- pRegisterSymbol
              return (ADRegister reg))
  <|> try (do reg <- pPrefixedRegisterSymbol
              return (ADRegister reg))
  <|> try (do flag <- pRegisterFlagSymbol
              return (ADRegisterFlag flag))
  <|> try (do imm <- pImmediateSymbol
              return (ADImmediate imm))

pCompareOp :: GenParser Char st CompareOp
pCompareOp =
  do op <- (    try (do string "slt"
                        return ISCmpLT)
            <|> try (do string "eq"
                        return ICmpEq)
           )
     pWhitespace
     _ <- pConstant
     return op

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
              return $ ['('] ++ nested ++ [')']
          )
  <|> do pWhitespace
         many1 (noneOf "()")

pWhitespace :: GenParser Char st String
pWhitespace = many (oneOf whitespace)

pParens :: GenParser Char st a -> GenParser Char st a
pParens c =
  do pWhitespace
     result <- between (char '(') (char ')') c
     pWhitespace
     return result

istrip :: String -> String
istrip = join " " . filter (\x -> x /= "") . split " "

whitespace = " \r\n\t"
