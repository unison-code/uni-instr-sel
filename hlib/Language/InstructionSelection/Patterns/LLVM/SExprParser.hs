--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Patterns.LLVM.SExprParser
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Parses a file of instruction patterns formatted as S-expressions. The
-- instruction patterns consists of a set of constraints and one or more
-- patterns expressed as LLVM IR code.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.LLVM.SExprParser (
  parse
, ParseError
) where

import Language.InstructionSelection.Patterns.LLVM.Base
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.Utils (Range (..))
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)



--------------------------------------------------
-- Type classes
--------------------------------------------------

-- | A class for providing a way of checking if an operation is expecting a
-- result size or not.

class HasResultSize a where
  hasResultSize :: a -> Bool

instance HasResultSize UnaryOp where
  hasResultSize op
    | op `elem` [USqrt, Sqrt, FixPointSqrt] = False
    | otherwise = True

instance HasResultSize BinaryOp where
  hasResultSize (BinArithmeticOp op) = hasResultSize op
  hasResultSize (BinCompareOp op) = hasResultSize op

instance HasResultSize ArithmeticOp where
  hasResultSize op
    | op `elem` [Plus, Minus, FixPointDiv] = False
    | otherwise = True

instance HasResultSize CompareOp where
  hasResultSize _ = True



--------------------------------------------------
-- Public functions
--------------------------------------------------

parse :: String -> Either ParseError [Instruction]
parse input = Parsec.parse pInstructions "" input



--------------------------------------------------
-- Parsec functions
--------------------------------------------------

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
  do constraints <- pLabeledDataList "constraints" pConstraint
     statements  <- pLabeledDataList "code" pStatement
     return (Pattern statements constraints)

pConstraint :: GenParser Char st Constraint
pConstraint =
      try pAllocConstraint
  <|> try pImmConstraint
  <|> try pAliasesConstraint
  <|> try pRelAddressConstraint
  <|> try pAbsAddressConstraint
  <|> try pRegFlagConstraint

pAllocConstraint :: GenParser Char st Constraint
pAllocConstraint = pLabeledData "allocate-in" pAllocConstraint'

pAllocConstraint' :: GenParser Char st Constraint
pAllocConstraint' =
  do sto <- pTemporaryOrSymbol
     pWhitespace
     sp <- pDataSpace
     return (AllocateInConstraint sto sp)

pTemporaryOrSymbol :: GenParser Char st (Either Temporary Symbol)
pTemporaryOrSymbol =
      try (do temp <- pTemporary
              return (Left temp)
          )
  <|> try (do sym <- pSymbol
              return (Right sym)
          )

pImmConstraint :: GenParser Char st Constraint
pImmConstraint = pLabeledData "immediate" pImmConstraint'

pImmConstraint' :: GenParser Char st Constraint
pImmConstraint' =
  do imm <- pSymbol
     pWhitespace
     ranges <- pParens (many1 pIntRange)
     return (ImmediateConstraint imm ranges)

pIntRange :: GenParser Char st (Range Integer)
pIntRange = pParens pIntRange'

pIntRange' :: GenParser Char st (Range Integer)
pIntRange' =
  do lo <- pInt
     pWhitespace1
     _ <- string "."
     pWhitespace1
     up <- pInt
     return (Range lo up)

pAliasesConstraint :: GenParser Char st Constraint
pAliasesConstraint = pLabeledData "aliases" pAliasesConstraint'

pAliasesConstraint' :: GenParser Char st Constraint
pAliasesConstraint' =
  do aliases <- many1 pAliases
     return (AliasesConstraint aliases)

pAliases :: GenParser Char st [AliasValue]
pAliases = pParens (many1 pAliasValue)

pAliasValue :: GenParser Char st AliasValue
pAliasValue =
      try (do pNoValue
              return AVNoValue)
  <|> try (do temp <- pTemporary
              return (AVTemporary temp))
  <|> try (do reg <- pSymbol
              return (AVSymbol reg))

pRelAddressConstraint :: GenParser Char st Constraint
pRelAddressConstraint = pLabeledData "rel-address" pRelAddressConstraint'

pRelAddressConstraint' :: GenParser Char st Constraint
pRelAddressConstraint' =
  do imm <- pSymbol
     pWhitespace1
     range <- pAddressRange
     return (RelAddressConstraint imm (MemoryClass "local" range))

pAbsAddressConstraint :: GenParser Char st Constraint
pAbsAddressConstraint = pLabeledData "abs-address" pAbsAddressConstraint'

pAbsAddressConstraint' :: GenParser Char st Constraint
pAbsAddressConstraint' =
  do imm <- pSymbol
     pWhitespace1
     range <- pAddressRange
     return (AbsAddressConstraint imm (MemoryClass "local" range))

pAddressRange :: GenParser Char st (Range Integer)
pAddressRange =
  do lo <- pInt
     pWhitespace1
     up <- pInt
     return (Range lo up)

pRegFlagConstraint :: GenParser Char st Constraint
pRegFlagConstraint = pLabeledData "reg-flag" pRegFlagConstraint'

pRegFlagConstraint' :: GenParser Char st Constraint
pRegFlagConstraint' =
  do flag <- pRegisterFlag'
     pWhitespace
     ranges <- pParens (many1 pIntRange)
     return (RegFlagConstraint flag ranges)

pStatement :: GenParser Char st Statement
pStatement =
      try pAssignmentStmt
  <|> try pSetRegStmt
  <|> try pStoreStmt
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
  do storage <- pSetRegDestination
     pWhitespace
     expr <- pStmtExpression
     return (SetRegStmt storage expr)

pSetRegDestination :: GenParser Char st SetRegDestination
pSetRegDestination =
      try (do reg <- pPrefixedRegister
              return (SRDRegister reg)
          )
  <|> try (do flag <- pRegisterFlag
              return (SRDRegisterFlag flag)
          )
  <|> try (do temp <- pTemporary
              return (SRDTemporary temp)
          )
  <|> try (do reg <- pSymbol
              return (SRDSymbol reg)
          )

pStoreStmt :: GenParser Char st Statement
pStoreStmt = pLabeledData "store" pStoreStmt'

pStoreStmt' :: GenParser Char st Statement
pStoreStmt' =
  do size <- pExprResultSize
     pWhitespace
     area <- pString
     pWhitespace1
     dst <- pStmtExpression
     pWhitespace
     value <- pStmtExpression
     return (StoreStmt dst area size value)

pStmtExpression :: GenParser Char st StmtExpression
pStmtExpression =
      try pUnaryOpStmtExpr
  <|> try pBinaryOpStmtExpr
  <|> try pDataStmtExpr
  <|> try pRegRangeStmtExpr
  <|> try pSizeStmtExpr
  <|> try pPhiStmtExpr
  <|> try pLoadStmtExpr
  <|> try pFP2IStmtExpr
  <|> try pTypeConvStmtExpr

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
     _ <- string "."
     pWhitespace1
     l <- pLabel
     return (PhiElement expr l)

pRegRangeStmtExpr :: GenParser Char st StmtExpression
pRegRangeStmtExpr = pLabeledData "reg-range" pRegRangeStmtExpr'

pRegRangeStmtExpr' :: GenParser Char st StmtExpression
pRegRangeStmtExpr' =
  do sto <- pProgramStorage
     pWhitespace
     up <- pProgramData
     pWhitespace
     lo <- pProgramData
     return (RegRangeStmtExpr sto (Range lo up))

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

pLoadStmtExpr :: GenParser Char st StmtExpression
pLoadStmtExpr = pLabeledData "load-mem" pLoadStmtExpr'

pLoadStmtExpr' :: GenParser Char st StmtExpression
pLoadStmtExpr' =
  do size <- pExprResultSize
     pWhitespace
     area <- pString
     pWhitespace1
     src <- pStmtExpression
     return (LoadStmtExpr area size src)

pFP2IStmtExpr :: GenParser Char st StmtExpression
pFP2IStmtExpr = pLabeledData "fptosi" pFP2IStmtExpr'

pFP2IStmtExpr' :: GenParser Char st StmtExpression
pFP2IStmtExpr' =
  do size_src <- pExprResultSize
     pWhitespace
     expr <- pStmtExpression
     pWhitespace
     size_dst <- pExprResultSize
     return (FP2IStmtExpr size_src expr size_dst)

pTypeConvStmtExpr :: GenParser Char st StmtExpression
pTypeConvStmtExpr = pParens pTypeConvStmtExpr'

pTypeConvStmtExpr' :: GenParser Char st StmtExpression
pTypeConvStmtExpr' =
  do op <- pTypeConvOp
     pWhitespace1
     size_src <- pExprResultSize
     pWhitespace
     expr <- pStmtExpression
     pWhitespace
     size_dst <- pExprResultSize
     return (TypeConvStmtExpr op size_dst size_src expr)

pProgramData :: GenParser Char st ProgramData
pProgramData =
      try (do pNoValue
              return PDNoValue)
  <|> try (do c <- pConstant
              return (PDConstant c))
  <|> try (do sym <- pSymbol
              return (PDSymbol sym))
  <|> try (do temp <- pTemporary
              return (PDTemporary temp))
  <|> try (do reg <- pPrefixedRegister
              return (PDRegister reg))
  <|> try (do reg <- pRegister
              return (PDRegister reg))

pProgramStorage :: GenParser Char st ProgramStorage
pProgramStorage =
      try (do sym <- pSymbol
              return (PSSymbol sym))
  <|> try (do temp <- pTemporary
              return (PSTemporary temp))
  <|> try (do reg <- pPrefixedRegister
              return (PSRegister reg))

pUncondBranchStmt :: GenParser Char st Statement
pUncondBranchStmt = pLabeledData "br" pUncondBranchStmt'

pUncondBranchStmt' :: GenParser Char st Statement
pUncondBranchStmt' =
  do l <- pLabel
     return (UncondBranchStmt l)

pCondBranchStmt :: GenParser Char st Statement
pCondBranchStmt = pLabeledData "br" pCondBranchStmt'

pCondBranchStmt' :: GenParser Char st Statement
pCondBranchStmt' =
  do d <- pProgramData
     pWhitespace
     fl <- pLabel
     pWhitespace
     tl <- pLabel
     return (CondBranchStmt d fl tl)

pLabelStmt :: GenParser Char st Statement
pLabelStmt = pLabeledData "label" pLabelStmt'

pLabelStmt' :: GenParser Char st Statement
pLabelStmt' =
  do l <- pLabel
     return (LabelStmt l)

pLabel :: GenParser Char st Label
pLabel =
  do l <- many1 (try alphaNum <|> try (char '-') <|> try (char '_'))
     return (Label l)

pExprResultSize :: GenParser Char st ExprResultSize
pExprResultSize =
      try (do c <- pConstant
              return (ERSConstValue c))
  <|> try (do reg <- pRegSizeExpr
              return (ERSRegSize reg))
  <|> try (do temp <- pTemporary
              return (ERSConstTemporary temp))
  <|> try (do imm <- pSymbol
              return (ERSConstSymbol imm))

pRegSizeExpr :: GenParser Char st Register
pRegSizeExpr = pLabeledData "size" pRegister

pConstant :: GenParser Char st ConstantValue
pConstant = pLabeledData "constant" pConstant'

pConstant' :: GenParser Char st ConstantValue
pConstant' =
  do _ <- string "?"
     pWhitespace1
     int <- pInt
     return (ConstIntValue int)

pTemporary :: GenParser Char st Temporary
pTemporary = pLabeledData "tmp" pTemporary'

pTemporary' :: GenParser Char st Temporary
pTemporary' =
  do int <- many1 digit
     return (Temporary (read int))

pSymbol :: GenParser Char st Symbol
pSymbol =
  do str <- many1 (try alphaNum <|> try (char '-') <|> try (char '_'))
     return (Symbol str)

pString :: GenParser Char st String
pString = many1 (try alphaNum <|> try (char '-') <|> try (char '_'))

pInt :: GenParser Char st Integer
pInt =
  do     try (do _ <- string "-"
                 num <- pInt'
                 return (num * (-1)))
     <|> pInt'

pInt' :: GenParser Char st Integer
pInt' =
  do int <- many1 digit
     return (read int)

pRegisterFlag :: GenParser Char st RegisterFlag
pRegisterFlag = pLabeledData "reg-flag" pRegisterFlag'

pRegisterFlag' :: GenParser Char st RegisterFlag
pRegisterFlag' =
  do flag <- pString
     pWhitespace
     reg <- pRegister
     return (RegisterFlag flag reg)

pRegister :: GenParser Char st Register
pRegister =
  do str <- pString
     return (Register str)

pPrefixedRegister :: GenParser Char st Register
pPrefixedRegister = pLabeledData "register" pRegister

pDataSpace :: GenParser Char st DataSpace
pDataSpace =
      try (do regClass <- pRegisterClass
              return (DSRegisterClass regClass))
     -- TODO: add memory class

pRegisterClass :: GenParser Char st RegisterClass
pRegisterClass = pParens pRegisterClass'

pRegisterClass' :: GenParser Char st RegisterClass
pRegisterClass' =
  do regs <- many1 pRegisterEatWhitespace
     return (RegisterClass regs)

pRegisterEatWhitespace :: GenParser Char st Register
pRegisterEatWhitespace =
  do pWhitespace
     reg <- pRegister
     pWhitespace
     return reg

pNoValue :: GenParser Char st ()
pNoValue =
  do _ <- string "no-value"
     return ()

pTypeConvOp :: GenParser Char st TypeConversionOp
pTypeConvOp =
      try (do _ <- string "zext"
              return ZExt)
  <|> try (do _ <- string "sext"
              return SExt)
  <|> try (do _ <- string "trunc"
              return Trunc)

pUnaryStmtOp :: GenParser Char st (UnaryOp, Maybe ExprResultSize)
pUnaryStmtOp =
  do op <- pUnaryStmtOpType
     pWhitespace1
     if hasResultSize op
        then do size' <- pExprResultSize
                let size = Just size'
                return (op, size)
        else return (op, Nothing)

pUnaryStmtOpType :: GenParser Char st UnaryOp
pUnaryStmtOpType =
      try (do _ <- string "usqrt"
              return USqrt)
  <|> try (do _ <- string "sqrt"
              return Sqrt)
  <|> try (do _ <- string "fixpointsqrt"
              return FixPointSqrt)
  <|> try (do _ <- string "bit_not"
              return Not)

pBinaryStmtOp :: GenParser Char st (BinaryOp, Maybe ExprResultSize)
pBinaryStmtOp =
      try (do (op, size) <- pIntCompareStmtOp
              return (BinCompareOp op, size))
  <|> try (do (op, size) <- pFloatCompareStmtOp
              return (BinCompareOp op, size))
  <|> try (do (op, size) <- pArithmeticStmtOp
              return (BinArithmeticOp op, size))

pIntCompareStmtOp :: GenParser Char st (CompareOp, Maybe ExprResultSize)
pIntCompareStmtOp =
  do _ <- string "icmp"
     pWhitespace1
     op <- pIntCompareOp
     pWhitespace1
     size' <- pExprResultSize
     let size = Just size'
     return (op, size)

pIntCompareOp :: GenParser Char st CompareOp
pIntCompareOp =
      try (do _ <- string "eq"
              return ICmpEq)
  <|> try (do _ <- string "ne"
              return ICmpNEq)
  <|> try (do _ <- string "ugt"
              return IUCmpGT)
  <|> try (do _ <- string "sgt"
              return ISCmpGT)
  <|> try (do _ <- string "uge"
              return IUCmpGE)
  <|> try (do _ <- string "sge"
              return ISCmpGE)
  <|> try (do _ <- string "ult"
              return IUCmpLT)
  <|> try (do _ <- string "slt"
              return ISCmpLT)
  <|> try (do _ <- string "ule"
              return IUCmpLE)
  <|> try (do _ <- string "sle"
              return ISCmpLE)

pFloatCompareStmtOp :: GenParser Char st (CompareOp, Maybe ExprResultSize)
pFloatCompareStmtOp =
  do _ <- string "fcmp"
     pWhitespace1
     op <- pFloatCompareOp
     pWhitespace1
     size' <- pExprResultSize
     let size = Just size'
     return (op, size)

pFloatCompareOp :: GenParser Char st CompareOp
pFloatCompareOp =
      try (do _ <- string "ueq"
              return FUCmpEq)
  <|> try (do _ <- string "oeq"
              return FOCmpEq)
  <|> try (do _ <- string "une"
              return FUCmpNEq)
  <|> try (do _ <- string "one"
              return FOCmpNEq)
  <|> try (do _ <- string "ugt"
              return FUCmpGT)
  <|> try (do _ <- string "ogt"
              return FOCmpGT)
  <|> try (do _ <- string "uge"
              return FUCmpGE)
  <|> try (do _ <- string "oge"
              return FOCmpGE)
  <|> try (do _ <- string "ult"
              return FUCmpLT)
  <|> try (do _ <- string "olt"
              return FOCmpLT)
  <|> try (do _ <- string "ule"
              return FUCmpLE)
  <|> try (do _ <- string "ole"
              return FOCmpLE)
  <|> try (do _ <- string "uno"
              return FCmpUn)

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
      try (do _ <- string "add"
              return IAdd)
  <|> try (do _ <- string "+"
              return Plus)
  <|> try (do _ <- string "satadd"
              return ISatAdd)
  <|> try (do _ <- string "-"
              return Minus)
  <|> try (do _ <- string "sub"
              return ISub)
  <|> try (do _ <- string "satsub"
              return ISatSub)
  <|> try (do _ <- string "mul"
              return IMul)
  <|> try (do _ <- string "satmul"
              return ISatMul)
  <|> try (do _ <- string "bit_and"
              return And)
  <|> try (do _ <- string "bit_or"
              return Or)
  <|> try (do _ <- string "bit_xor"
              return Xor)
  <|> try (do _ <- string "shl"
              return Shl)
  <|> try (do _ <- string "lshr"
              return LShr)
  <|> try (do _ <- string "ashr"
              return AShr)
  <|> try (do _ <- string "udiv"
              return IUDiv)
  <|> try (do _ <- string "sdiv"
              return ISDiv)
  <|> try (do _ <- string "fixpointdiv"
              return FixPointDiv)
  <|> try (do _ <- string "fadd"
              return FAdd)
  <|> try (do _ <- string "fsub"
              return FSub)
  <|> try (do _ <- string "fmul"
              return FMul)
  <|> try (do _ <- string "fdiv"
              return FDiv)
  <|> try (do _ <- string "urem"
              return IURem)
  <|> try (do _ <- string "srem"
              return ISRem)

pLabeledData :: String -> GenParser Char st a -> GenParser Char st a
pLabeledData str p = pParens (pLabeledData' str p)

pLabeledData' :: String -> GenParser Char st a -> GenParser Char st a
pLabeledData' str p =
  do _ <- string str
     pWhitespace1
     result <- p
     return result

pLabeledDataList :: String -> GenParser Char st a -> GenParser Char st [a]
pLabeledDataList str p = pParens (pLabeledDataList' str p)

pLabeledDataList' :: String -> GenParser Char st a -> GenParser Char st [a]
pLabeledDataList' str p =
  do _ <- string str
     option [] (do pWhitespace1
                   result <- many p
                   return result)

pWhitespace :: GenParser Char st ()
pWhitespace =
  do _ <- many (oneOf whitespace)
     return ()

pWhitespace1 :: GenParser Char st ()
pWhitespace1 =
  do _ <- many1 (oneOf whitespace)
     return ()

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

whitespace :: [Char]
whitespace = " \r\n\t"
