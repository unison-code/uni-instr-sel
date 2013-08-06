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

-- TODO: Fix so that "(register ...)" gets its own data type
-- TODO: Fix handling of "zext" and other conversion operations

module Language.InstructionSelection.Patterns.LLVM.SExprParser (
      parse
    ) where

import Language.InstructionSelection.Patterns.LLVM.Base
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.Utils (Range (..), Natural, toNatural)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Data.String.Utils
import Debug.Trace



--------------------------------------------------
-- Type classes
--------------------------------------------------

-- | A class for providing a way of checking if an operation is expecting a
-- result size or not.

class Operation a where

  -- | Checks whether an operation requires a result size.

  hasResultSize :: a -> Bool

--------------------------------------------------
-- Operation instances
--------------------------------------------------

instance Operation UnaryOp where
  hasResultSize op
    | op `elem` [USqrt, FixPointSqrt] = False
    | otherwise = True

instance Operation BinaryOp where
  hasResultSize (BinArithmeticOp op) = hasResultSize op
  hasResultSize (BinCompareOp op) = hasResultSize op

instance Operation ArithmeticOp where
  hasResultSize op
    | op `elem` [Plus, Minus, IUDiv, ISDiv, FDiv, IURem, ISRem,
                 FixPointDiv] = False
    | otherwise = True

instance Operation CompareOp where
  hasResultSize _ = True



parse :: String -> Either ParseError [Instruction]
parse input = Parsec.parse pInstructions "" input

pInstructions :: GenParser Char st [Instruction]
pInstructions =
  do instructions <- many pInstruction
     eof
     return instructions

pInstruction :: GenParser Char st Instruction
pInstruction = pLabeledData "instruction" pInstruction'

pInstruction' :: GenParser Char st Instruction
pInstruction' =
  do assemblyStr <- pAssemblyStr
     patterns    <- many pPattern
     return (Instruction assemblyStr patterns)

pAssemblyStr :: GenParser Char st AssemblyString
pAssemblyStr =
  do str <- many1 (noneOf $ whitespace ++ "(")
     return (AssemblyString str)

pPattern :: GenParser Char st Pattern
pPattern = pLabeledData "pattern" pPattern'

pPattern' :: GenParser Char st Pattern
pPattern' =
  do constraints <- pLabeledData "constraints" pAllConstraints
     statements  <- pLabeledData "code" pAllStatements
     return (Pattern statements constraints)

pAllConstraints :: GenParser Char st [Constraint]
pAllConstraints = many pConstraint

pConstraint :: GenParser Char st Constraint
pConstraint =
      try pAllocConstraint
  <|> try pImmConstraint
  <|> try pZimmConstraint
  <|> try pAliasConstraint
  <|> try pRelAddressConstraint
  <|> try pAbsAddressConstraint

pAllocConstraint :: GenParser Char st Constraint
pAllocConstraint = pLabeledData "allocate-in" pAllocConstraint'

pAllocConstraint' :: GenParser Char st Constraint
pAllocConstraint' =
  do storage <- pAnyStorage
     pWhitespace
     space <- pAnyStorageSpace
     return (AllocateIn storage space)

pImmConstraint :: GenParser Char st Constraint
pImmConstraint = pLabeledData "imm" pImmConstraint'

pImmConstraint' :: GenParser Char st Constraint
pImmConstraint' =
  do (range, imm) <- pImmRangeAndSymbol
     return (ImmediateRange imm range)

pZimmConstraint :: GenParser Char st Constraint
pZimmConstraint = pLabeledData "zimm" pZimmConstraint'

pZimmConstraint' :: GenParser Char st Constraint
pZimmConstraint' =
  do (range, imm) <- pImmRangeAndSymbol
     return (ImmediateRangeNoZero imm range)

pImmRangeAndSymbol :: GenParser Char st (Range Integer, ImmediateSymbol)
pImmRangeAndSymbol =
  do range <- pIntRange
     pWhitespace
     imm <- pImmediateSymbol
     return (range, imm)

pIntRange :: GenParser Char st (Range Integer)
pIntRange =
  do lower <- pInt
     pWhitespace
     upper <- pInt
     return (Range lower upper)

pAliasConstraint :: GenParser Char st Constraint
pAliasConstraint = pLabeledData "alias" pAliasConstraint'

pAliasConstraint' :: GenParser Char st Constraint
pAliasConstraint' =
  do temp <- pTemporary
     pWhitespace
     reg <-      try (do pNoValue
                         return Nothing)
            <|> (do rg <- pRegister
                    return (Just rg))
     return (Alias temp reg)

pRelAddressConstraint :: GenParser Char st Constraint
pRelAddressConstraint = pLabeledData "rel-address" pRelAddressConstraint'

pRelAddressConstraint' :: GenParser Char st Constraint
pRelAddressConstraint' =
  do (range, imm) <- pImmRangeAndSymbol
     return (RelAddressConstraint imm (MemoryClass "local" range))

pAbsAddressConstraint :: GenParser Char st Constraint
pAbsAddressConstraint = pLabeledData "abs-address" pAbsAddressConstraint'

pAbsAddressConstraint' :: GenParser Char st Constraint
pAbsAddressConstraint' =
  do (range, imm) <- pImmRangeAndSymbol
     return (AbsAddressConstraint imm (MemoryClass "local" range))

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
pAssignmentStmt = pLabeledData "=" pAssignmentStmt'

pAssignmentStmt' :: GenParser Char st Statement
pAssignmentStmt' =
  do temp <- pTemporary
     pWhitespace
     expr <- pStmtExpression
     return (AssignmentStmt temp expr)

pSetRegStmt :: GenParser Char st Statement
pSetRegStmt = pLabeledData "set-reg" pSetRegStmt'

pSetRegStmt' :: GenParser Char st Statement
pSetRegStmt' =
  do reg <- pRegister
     pWhitespace
     expr <- pStmtExpression
     return (SetRegStmt reg expr)

pStmtExpression :: GenParser Char st StmtExpression
pStmtExpression =
      try pUnaryOpStmtExpr
  <|> try pBinaryOpStmtExpr
  <|> try pDataStmtExpr
  <|> try pRegRangeStmtExpr
  <|> try pSizeStmtExpr
  <|> try pPhiStmtExpr

pSizeStmtExpr :: GenParser Char st StmtExpression
pSizeStmtExpr =
  do reg <- pRegSizeExpr
     return (SizeStmtExpr reg)

pPhiStmtExpr :: GenParser Char st StmtExpression
pPhiStmtExpr = pLabeledData "phi" pPhiStmtExpr'

pPhiStmtExpr' :: GenParser Char st StmtExpression
pPhiStmtExpr' =
  do elems <- pParens pAllPhiElements
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
     pWhitespace1
     label <- pLabel
     return (PhiElement expr label)

pRegRangeStmtExpr :: GenParser Char st StmtExpression
pRegRangeStmtExpr = pLabeledData "reg-range" pRegRangeStmtExpr'

pRegRangeStmtExpr' :: GenParser Char st StmtExpression
pRegRangeStmtExpr' =
  do reg <- pRegister
     pWhitespace
     lower <- pConstProgramData
     pWhitespace
     upper <- pConstProgramData
     return (RegRangeStmtExpr reg (Range lower upper))

pUnaryOpStmtExpr :: GenParser Char st StmtExpression
pUnaryOpStmtExpr = pParens pUnaryOpStmtExpr'

pUnaryOpStmtExpr' :: GenParser Char st StmtExpression
pUnaryOpStmtExpr' =
  do (op, size) <- pUnaryStmtOp
     pWhitespace
     expr <- pStmtExpression
     return (UnaryOpStmtExpr op size expr)

pBinaryOpStmtExpr :: GenParser Char st StmtExpression
pBinaryOpStmtExpr = pParens pBinaryOpStmtExpr'

pBinaryOpStmtExpr' :: GenParser Char st StmtExpression
pBinaryOpStmtExpr' =
  do (op, size) <- pBinaryStmtOp
     pWhitespace
     expr1 <- pStmtExpression
     pWhitespace
     expr2 <- pStmtExpression
     return (BinaryOpStmtExpr op size expr1 expr2)

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
pUncondBranchStmt = pLabeledData "br" pUncondBranchStmt'

pUncondBranchStmt' :: GenParser Char st Statement
pUncondBranchStmt' =
  do label <- pLabel
     return (UncondBranchStmt label)

pCondBranchStmt :: GenParser Char st Statement
pCondBranchStmt = pLabeledData "br" pCondBranchStmt'

pCondBranchStmt' :: GenParser Char st Statement
pCondBranchStmt' =
  do reg <- pRegister
     pWhitespace
     falseLabel <- pLabel
     pWhitespace
     trueLabel <- pLabel
     return (CondBranchStmt reg falseLabel trueLabel)

pLabelStmt :: GenParser Char st Statement
pLabelStmt = pLabeledData "label" pLabelStmt'

pLabelStmt' :: GenParser Char st Statement
pLabelStmt' =
  do label <- pLabel
     return (LabelStmt label)

pLabel :: GenParser Char st Label
pLabel =
  do label <- many1 (try alphaNum <|> try (char '_'))
     return (Label label)

pSymbolWidth :: GenParser Char st Integer
pSymbolWidth =
  do (ConstIntValue int) <- pConstant
     return int

pExprResultSize :: GenParser Char st ExprResultSize
pExprResultSize =
      try (do const <- pConstant
              return (ERSConstValue const))
  <|> try (do reg <- pRegSizeExpr
              return (ERSRegSize reg))
  <|> try (do temp <- pTemporary
              return (ERSConstTemporary temp))

pRegSizeExpr :: GenParser Char st Register
pRegSizeExpr = pLabeledData "size" pRegister

pConstProgramData :: GenParser Char st ConstProgramData
pConstProgramData =
      try (do const <- pConstant
              return (CPDConstant const))
  <|> try (do imm <- pImmediateSymbol
              return (CPDImmediate imm))

pConstant :: GenParser Char st ConstantValue
pConstant = pLabeledData "constant" pConstant'

pConstant' :: GenParser Char st ConstantValue
pConstant' =
  do string "?"
     pWhitespace1
     int <- pInt
     return (ConstIntValue int)

pTemporary :: GenParser Char st Temporary
pTemporary = pLabeledData "tmp" pTemporary'

pTemporary' :: GenParser Char st Temporary
pTemporary' =
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
pRegisterFlag = pLabeledData "reg-flag" pRegisterFlag'

pRegisterFlag' :: GenParser Char st RegisterFlag
pRegisterFlag' =
  do flag <- pRegisterFlagSymbol
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
pPrefixedRegisterSymbol = pLabeledData "register" pRegisterSymbol

pDataSpace :: GenParser Char st DataSpace
pDataSpace =
      try (do regClass <- pRegisterClass
              return (DSRegisterClass regClass))
     -- TODO: add memory class

pRegisterClass :: GenParser Char st RegisterClass
pRegisterClass = pParens pRegisterClass'

pRegisterClass' :: GenParser Char st RegisterClass
pRegisterClass' =
  do regs <- many1 pRegisterSymbolEatWhitespace
     return (RegisterClass regs)

pRegisterSymbolEatWhitespace :: GenParser Char st RegisterSymbol
pRegisterSymbolEatWhitespace =
  do pWhitespace
     reg <- pRegisterSymbol
     pWhitespace
     return reg

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

pUnaryStmtOp :: GenParser Char st (UnaryOp, Maybe ExprResultSize)
pUnaryStmtOp =
  do op <- pUnaryStmtOpType
     pWhitespace1
     if (hasResultSize op)
        then (do size' <- pExprResultSize
                 let size = Just size'
                 return (op, size))
        else return (op, Nothing)

pUnaryStmtOpType :: GenParser Char st UnaryOp
pUnaryStmtOpType =
      try (do string "usqrt"
              return USqrt)
  <|> try (do string "fixpointsqrt"
              return FixPointSqrt)
  <|> try (do string "bit_not"
              return Not)

pBinaryStmtOp :: GenParser Char st (BinaryOp, Maybe ExprResultSize)
pBinaryStmtOp =
      try (do (op, size) <- pCompareStmtOp
              return (BinCompareOp op, size))
  <|> try (do (op, size) <- pArithmeticStmtOp
              return (BinArithmeticOp op, size))

pCompareStmtOp :: GenParser Char st (CompareOp, Maybe ExprResultSize)
pCompareStmtOp =
  do string "icmp"
     pWhitespace1
     op <- pIntCompareOp
     pWhitespace1
     size' <- pExprResultSize
     let size = Just size'
     return (op, size)
  -- TODO: handle floats

pIntCompareOp :: GenParser Char st CompareOp
pIntCompareOp =
      try (do string "eq"
              return ICmpEq)
  <|> try (do string "ult"
              return IUCmpLT)
  <|> try (do string "slt"
              return ISCmpLT)
  <|> try (do string "gt"
              return IUCmpGT)
  <|> try (do string "ugt"
              return IUCmpGT)
  <|> try (do string "sgt"
              return ISCmpGT)
  -- TOOD: add missing operations

pArithmeticStmtOp :: GenParser Char st (ArithmeticOp, Maybe ExprResultSize)
pArithmeticStmtOp =
  do op <- pArithmeticStmtOpType
     pWhitespace1
     if (hasResultSize op)
        then (do size' <- pExprResultSize
                 let size = Just size'
                 return (op, size))
        else return (op, Nothing)

pArithmeticStmtOpType :: GenParser Char st ArithmeticOp
pArithmeticStmtOpType =
      try (do string "add"
              return IAdd)
  <|> try (do string "+"
              return Plus)
  <|> try (do string "satadd"
              return ISatAdd)
  <|> try (do string "-"
              return Minus)
  <|> try (do string "sub"
              return ISub)
  <|> try (do string "satsub"
              return ISatSub)
  <|> try (do string "mul"
              return IMul)
  <|> try (do string "satmul"
              return ISatMul)
  <|> try (do string "bit_and"
              return And)
  <|> try (do string "bit_or"
              return Or)
  <|> try (do string "bit_xor"
              return Xor)
  <|> try (do string "shl"
              return Shl)
  <|> try (do string "lshr"
              return LShr)
  <|> try (do string "ashr"
              return AShr)
  <|> try (do string "udiv"
              return IUDiv)
  <|> try (do string "sdiv"
              return ISDiv)
  <|> try (do string "fixpointdiv"
              return FixPointDiv)
  <|> try (do string "fdiv"
              return FDiv)
  <|> try (do string "urem"
              return IURem)
  <|> try (do string "srem"
              return ISRem)
  <|> try (do string "zext"
              return ZExt)
  <|> try (do string "sext"
              return SExt)
  -- TOOD: add missing operations

pLabeledData :: String -> GenParser Char st a -> GenParser Char st a
pLabeledData str p = pParens (pLabeledData' str p)

pLabeledData' :: String -> GenParser Char st a -> GenParser Char st a
pLabeledData' str p =
  do string str
     pWhitespace1
     result <- p
     return result

pWhitespace :: GenParser Char st String
pWhitespace = many (oneOf whitespace)

pWhitespace1 :: GenParser Char st String
pWhitespace1 = many1 (oneOf whitespace)

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

whitespace = " \r\n\t"
