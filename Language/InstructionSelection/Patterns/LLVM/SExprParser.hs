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
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.Utils (Range (..), Natural, toNatural)
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
     pWhitespace
     reg      <- pRegister
     return (AssertContainsExpr reg regclass)

pCompareAssertExpr :: GenParser Char st AssertExpression
pCompareAssertExpr = pParens pCompareAssertExpr'

pCompareAssertExpr' :: GenParser Char st AssertExpression
pCompareAssertExpr' =
  do op <- pCompareAssertOp
     pWhitespace
     data1 <- pAnyData
     pWhitespace
     data2 <- pAnyData
     return (AssertCompareExpr op data1 data2)

pRegFlagAssertExpr :: GenParser Char st AssertExpression
pRegFlagAssertExpr = pParens pRegFlagAssertExpr'

pRegFlagAssertExpr' :: GenParser Char st AssertExpression
pRegFlagAssertExpr' =
  do regFlag <- pRegisterFlag'
     return (AssertRegFlagExpr regFlag)

pNotAssertExpr :: GenParser Char st AssertExpression
pNotAssertExpr = pParens pNotAssertExpr'

pNotAssertExpr' :: GenParser Char st AssertExpression
pNotAssertExpr' =
  do string "not"
     expr <- pAssertExpression
     return (AssertNotExpr expr)

pTrueFalseAssertExpr :: GenParser Char st AssertExpression
pTrueFalseAssertExpr =
  do int <- pConstant
     if int == (ConstIntValue 0)
        then return AssertFalseExpr
        else return AssertTrueExpr

pImmediateAssertExpr :: GenParser Char st AssertExpression
pImmediateAssertExpr =
  do pWhitespace
     imm <- pImmediateSymbol
     return (AssertImmediateExpr imm)

pAllStatements :: GenParser Char st [Statement]
pAllStatements = many pStatement

pStatement :: GenParser Char st Statement
pStatement =
      try pAssignmentStmt
  <|> try pSetRegStmt
  <|> try pUncondBranchStmt
  <|> try pCondBranchStmt
  <|> try pLabelStmt

pAssignmentStmt :: GenParser Char st Statement
pAssignmentStmt = pParens pAssignmentStmt'

pAssignmentStmt' :: GenParser Char st Statement
pAssignmentStmt' =
  do string "="
     pWhitespace
     temp <- pTemporary
     pWhitespace
     expr <- pStmtExpression
     return (AssignmentStmt temp expr)

pSetRegStmt :: GenParser Char st Statement
pSetRegStmt = pParens pSetRegStmt'

pSetRegStmt' :: GenParser Char st Statement
pSetRegStmt' =
  do string "set-reg"
     pWhitespace
     reg <- pRegister
     pWhitespace
     expr <- pStmtExpression
     return (SetRegStmt reg expr)

pStmtExpression :: GenParser Char st StmtExpression
pStmtExpression =
      try pUnaryOpStmtExpr
  <|> try pBinaryOpStmtExpr
  <|> try pDataStmtExpr
  <|> try pRegRangeStmtExpr
  <|> try pPhiStmtExpr

pPhiStmtExpr :: GenParser Char st StmtExpression
pPhiStmtExpr = pParens pPhiStmtExpr'

pPhiStmtExpr' :: GenParser Char st StmtExpression
pPhiStmtExpr' =
  do string "phi"
     pWhitespace
     elems <- pParens pAllPhiElements
     return (PhiStmtExpr elems)

pAllPhiElements :: GenParser Char st [PhiElement]
pAllPhiElements = many1 (pPhiElement)

pPhiElement :: GenParser Char st PhiElement
pPhiElement = pParens pPhiElement'

pPhiElement' :: GenParser Char st PhiElement
pPhiElement' =
  do expr <- pStmtExpression
     pWhitespace
     string "."
     pWhitespace
     label <- pLabel
     return (PhiElement expr label)

pRegRangeStmtExpr :: GenParser Char st StmtExpression
pRegRangeStmtExpr = pParens pRegRangeStmtExpr'

pRegRangeStmtExpr' :: GenParser Char st StmtExpression
pRegRangeStmtExpr' =
  do string "reg-range"
     pWhitespace
     reg <- pRegister
     pWhitespace
     (ConstIntValue lower) <- pConstant
     pWhitespace
     (ConstIntValue upper) <- pConstant
     return (RegRangeStmtExpr reg (Range (toNatural lower) (toNatural upper)))

pUnaryOpStmtExpr :: GenParser Char st StmtExpression
pUnaryOpStmtExpr = pParens pUnaryOpStmtExpr'

pUnaryOpStmtExpr' :: GenParser Char st StmtExpression
pUnaryOpStmtExpr' =
  do op <- pUnaryStmtOp
     pWhitespace
     expr <- pStmtExpression
     return (UnaryOpStmtExpr op expr)

pBinaryOpStmtExpr :: GenParser Char st StmtExpression
pBinaryOpStmtExpr = pParens pBinaryOpStmtExpr'

pBinaryOpStmtExpr' :: GenParser Char st StmtExpression
pBinaryOpStmtExpr' =
  do op <- pBinaryStmtOp
     pWhitespace
     expr1 <- pStmtExpression
     pWhitespace
     expr2 <- pStmtExpression
     return (BinaryOpStmtExpr op expr1 expr2)

pDataStmtExpr :: GenParser Char st StmtExpression
pDataStmtExpr =
  do pdata <- pProgramData
     return (DataStmtExpr pdata)

pProgramData :: GenParser Char st ProgramData
pProgramData =
      try (do pNoValue
              return PDNoValue)
  <|> try (do const <- pConstant
              return (PDConstant const))
  <|> try (do imm <- pImmediateSymbol
              return (PDImmediate imm))
  <|> try (do temp <- pTemporary
              return (PDTemporary temp))
  <|> try (do reg <- pRegister
              return (PDRegister reg))

pUncondBranchStmt :: GenParser Char st Statement
pUncondBranchStmt = pParens pUncondBranchStmt'

pUncondBranchStmt' :: GenParser Char st Statement
pUncondBranchStmt' =
  do string "br"
     pWhitespace
     label <- pLabel
     return (UncondBranchStmt label)

pCondBranchStmt :: GenParser Char st Statement
pCondBranchStmt = pParens pCondBranchStmt'

pCondBranchStmt' :: GenParser Char st Statement
pCondBranchStmt' =
  do string "br"
     pWhitespace
     reg <- pRegister
     pWhitespace
     falseLabel <- pLabel
     pWhitespace
     trueLabel <- pLabel
     return (CondBranchStmt reg falseLabel trueLabel)

pLabelStmt :: GenParser Char st Statement
pLabelStmt = pParens pLabelStmt'

pLabelStmt' :: GenParser Char st Statement
pLabelStmt' =
  do string "label"
     pWhitespace
     label <- pLabel
     return (LabelStmt label)

pLabel :: GenParser Char st Label
pLabel =
  do label <- many1 (try alphaNum <|> try (char '_'))
     return (Label label)

pSymbolWidth :: GenParser Char st Integer
pSymbolWidth =
  do (ConstIntValue int) <- pConstant
     return int

pOpWidth :: GenParser Char st Integer
pOpWidth =
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
      try (do pNoValue
              return (ADNoValue))
  <|> try (do const <- pConstant
              return (ADConstant const))
  <|> try (do temp <- pTemporary
              return (ADTemporary temp))
  <|> try (do reg <- pRegister
              return (ADRegister reg))
  <|> try (do flag <- pRegisterFlag
              return (ADRegisterFlag flag))
  <|> try (do imm <- pImmediateSymbol
              return (ADImmediate imm))

pNoValue :: GenParser Char st ()
pNoValue =
  do string "no-value"
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

pUnaryStmtOp :: GenParser Char st UnaryOp
pUnaryStmtOp =
     try (do string "fixpointsqrt"
             return FixPointSqrt)
 <|> try (do string "bit_not"
             pWhitespace
             _ <- pOpWidth
             return FixPointSqrt)

pBinaryStmtOp :: GenParser Char st BinaryOp
pBinaryStmtOp =
      try (do op <- pCompareStmtOp
              return (BinCompareOp op))
  <|> try (do op <- pArithmeticStmtOp
              return (BinArithmeticOp op))

pCompareAssertOp :: GenParser Char st CompareOp
pCompareAssertOp =
  do string "icmp"
     pWhitespace
     op <- pIntCompareOp
     pWhitespace
     _ <- pOpWidth
     return op
  -- TODO: handle floats

pCompareStmtOp :: GenParser Char st CompareOp
pCompareStmtOp =
  do string "icmp"
     pWhitespace
     _ <- pOpWidth
     pWhitespace
     op <- pIntCompareOp
     return op
  -- TODO: handle floats

pIntCompareOp :: GenParser Char st CompareOp
pIntCompareOp =
      try (do string "eq"
              return ICmpEq)
  <|> try (do string "ult"
              return IUCmpLT)
  <|> try (do string "slt"
              return ISCmpLT)
  <|> try (do string "ugt"
              return IUCmpGT)
  <|> try (do string "sgt"
              return ISCmpGT)
  -- TOOD: add missing operations

pArithmeticStmtOp :: GenParser Char st ArithmeticOp
pArithmeticStmtOp =
  do op <- pArithmeticStmtOpType
     pWhitespace
     tmp <- pOpWidth
     return op

pArithmeticStmtOpType :: GenParser Char st ArithmeticOp
pArithmeticStmtOpType =
      try (do string "satadd"
              return ISatAdd)
  <|> try (do string "bit_and"
              return And)
  <|> try (do string "shl"
              return Shl)
  <|> try (do string "lshr"
              return LShr)
  <|> try (do string "sub"
              return ISub)
  <|> try (do string "zext"
              return ZExt)
  -- TOOD: add missing operations

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
