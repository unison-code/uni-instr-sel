--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.LLVM.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing instructions as LLVM
-- patterns.
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Patterns.LLVM.Base where

import Language.InstructionSelection.Utils (Range (..), Natural)
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.SExpressions



-- | Record for representing a constant value.

data ConstantValue
    = ConstIntValue Integer
    deriving (Show, Eq)

-- | Record for a temporary.

data Temporary
    = Temporary

          -- | Identifier for the temporary. All temporaries are required to
          -- have a unique identifier within the function scope.

          Integer

    deriving (Show, Eq)

-- | Record for a register. A register can either by denoted by a register
-- symbol or via a temporary (which will, in some way, refer to a register).

data Register
    = RegByTemporary Temporary
    | RegBySymbol RegisterSymbol
    deriving (Show)

-- | Record for describing a register symbol.

data RegisterSymbol
    = RegisterSymbol String
    deriving (Show)

-- | Record for describing a register flag symbol.

data RegisterFlagSymbol
    = RegisterFlagSymbol String
    deriving (Show)

-- | Record for describing a register flag. All flags are associated with a
-- specific register.

data RegisterFlag
    = RegisterFlag RegisterFlagSymbol Register
    deriving (Show)

-- | Record for describing a register class.

data RegisterClass
    = RegisterClass
          -- | Identifier for the class.

          String
    deriving (Show, Eq)

-- | Record for describing a memory class.

data MemoryClass
    = MemoryClass

          -- | Identifier for the class.

          String

          -- | Address range within the memory class. This may be the entire
          -- range of the memory space, or a restricted subset.

          (Range Integer)
    deriving (Show, Eq)

-- | Record for describing a data space.

data DataSpace
    = DSRegisterClass RegisterClass
    | DSMemoryClass MemoryClass
    deriving (Show, Eq)

-- | Record for an immediate value symbol. This is used to represent an
-- immediate value which will become available during pattern matching.

data ImmediateSymbol
    = ImmediateSymbol String
    deriving (Show, Eq)

-- | Record for containing an potentially nested expression.

-- | Record for describing program data. The data can be of many different
-- types, e.g. a constant, an immediate, a temporary, etc.

data ProgramData

      -- | A value which is fixed and known at compile time.

    = PDConstant ConstantValue

      -- | An immediate value represented by a symbol.

    | PDImmediate ImmediateSymbol

      -- | A value located in a temporary.

    | PDTemporary Temporary

      -- | A value located in a register.

    | PDRegister Register

      -- | An undefined value.

    | PDNoValue

    deriving (Show)

-- | Record for representing values that are constant at compile time.

data ConstProgramData
    = CPDConstant ConstantValue
    | CPDImmediate ImmediateSymbol
    deriving (Show)

-- | Record for representing any form of data (register, register flag,
-- constant, temporary, etc.).

data AnyData
    = ADTemporary Temporary
    | ADRegister Register
    | ADRegisterFlag RegisterFlag
    | ADConstant ConstantValue
    | ADImmediate ImmediateSymbol
    | ADNoValue
    deriving (Show)

-- | Record for representing any form of storage unit (register, register flag,
-- temporary, etc.).

data AnyStorage
    = ASTemporary Temporary
    | ASRegister Register
    | ASRegisterFlag RegisterFlag
    deriving (Show)

-- | Record for representing any form of storage space (register flag or data
-- space).

data AnyStorageSpace
    = ASSRegisterFlag RegisterFlag
    | ASSDataSpace DataSpace
    deriving (Show)

-- | Record for representing an expression for a statement.

data StmtExpression

      -- | A binary expression. The first expression is the LHS and the second
      -- expression is the RHS.

    = BinaryOpStmtExpr
          BinaryOp

          -- | Size (in bits) of result

          (Maybe ExprResultSize)

          -- | LHS.

          StmtExpression

          -- | RHS.

          StmtExpression

      -- | A unary expression.

    | UnaryOpStmtExpr UnaryOp (Maybe ExprResultSize) StmtExpression

      -- | A phi expression.

    | PhiStmtExpr [PhiElement]

      -- | A data expression.

    | DataStmtExpr ProgramData

      -- | Gets the size (in bits) of a register.

    | SizeStmtExpr Register

      -- | A register range expression, which takes a range of bits from a
      -- register. The same effect can be achieved with a series of bit
      -- operations.

    | RegRangeStmtExpr Register (Range ConstProgramData)

    deriving (Show)

-- | Record for representing the size of the result of an expression.

data ExprResultSize

      -- | Size of a given register.

    = ERSRegSize Register

      -- | Constant size represented via a constant value.

    | ERSConstValue ConstantValue

      -- | Constant size represented via a temporary.

    | ERSConstTemporary Temporary

    deriving (Show)

-- | Record for containing an element inside a phi function. The label indicates
-- from where the value comes from.

data PhiElement
    = PhiElement StmtExpression Label
    deriving (Show)

-- | Record for containing a label.

data Label
    = Label

          -- | Label identifier.

          String

    deriving (Show, Eq)

-- | Record for representing an LLVM statement.

data Statement

      -- | Assigns the result of an expression to a temporary.

    = AssignmentStmt Temporary StmtExpression

      -- | Assigns the result of an expression to a register.

    | SetRegStmt Register StmtExpression

      -- | Performs an unconditional branch (or jump) to a label.

    | UncondBranchStmt Label

      -- | Performs an conditional branch (or jump) to a label.

    | CondBranchStmt
          Register

          -- | Label taken if the register evaluates to @False@.

          Label

          -- | Label taken if the register evaluates to @True@.

          Label

      -- | Declares a label.

    | LabelStmt Label

    deriving (Show)

-- | Record for a pattern constraint.

data Constraint

      -- | The @AllocateIn@ constraint dictates that a storage unit must be
      -- located in a particular storage space.

    = AllocateIn AnyStorage AnyStorageSpace

      -- | The @ImmediateRange@ constraint limits the range of values that an
      -- immediate value may take (including 0).

    | ImmediateRange ImmediateSymbol (Range Integer)

      -- | The @ImmediateRange@ constraint limits the range of values that an
      -- immediate value may take (excluding 0).

    | ImmediateRangeNoZero ImmediateSymbol (Range Integer)

      -- | The @Alias@ constraint dictates that a temporary must be the
      -- same, in the sense that both temporaries must be assigned the same
      -- register. Sometimes a temporary may be aliased with @no-value@, upon
      -- which there is no second temporary.

    | Alias Temporary (Maybe Register)

      -- | The @Assert@ constraints contain any other, arbitrary constraints.

    | Assert AssertExpression

      -- | The @RelAddressConstraint@ constraint forces an immediate value to be
      -- within a certain relative memory address range.

    | RelAddressConstraint ImmediateSymbol MemoryClass

      -- | Same as for @RelAddressConstraint@ but for absolute values.

    | AbsAddressConstraint ImmediateSymbol MemoryClass

    deriving (Show)

-- | Record for containing an assert expression.

data AssertExpression

      -- | Checks whether a register is within a certain register class.

    = AssertContainsExpr Register RegisterClass

      -- | Checks whether a comparison between two data holds.

    | AssertCompareExpr CompareOp (Maybe ExprResultSize) AnyData AnyData

      -- | Checks whether a certain flag in a register is set.

    | AssertRegFlagExpr RegisterFlag

      -- | Negates an assert expression.

    | AssertNotExpr AssertExpression

      -- | Always evaluate to 'False'.

    | AssertFalseExpr

      -- | Always evaluate to 'True'.

    | AssertTrueExpr

      -- | An immediate symbol.

    | AssertImmediateExpr ImmediateSymbol

    deriving (Show)

-- | Record for containing the assembly string to produce during code emission.

data AssemblyString
    = AssemblyString
          -- | TODO: refactor into something that is easier to process.
          String
    deriving (Show)

-- | Record for representing a pattern including the constraints.

data Pattern
    = Pattern

          -- | The LLVM statements.

          [Statement]

          -- | Constraints that must be enforced for the pattern.

          [Constraint]

    deriving (Show)

-- | Record for representing an instruction.

data Instruction
    = Instruction

          -- | Assembly string to produce upon code emission.

          AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

          [Pattern]

    deriving (Show)



--------------------------------------------------
-- SExpressionable instances
--------------------------------------------------

instance SExpressionable Instruction where
  prettySE (Instruction ass pats) i =
    let i1 = i + 1
    in "(instruction"
       ++ "\n" ++ indent i1 ++ (prettySE ass i1)
       ++ "\n" ++ (prettySEList pats i1)
       ++ ")"

instance SExpressionable AssemblyString where
  prettySE (AssemblyString str) i = str

instance SExpressionable Pattern where
  prettySE (Pattern stmts cnstrs) i =
    let i1 = i + 1
        i2 = i + 2
    in "(pattern"
       ++ "\n" ++ indent i1 ++ "(constraints"
       ++ "\n" ++ (prettySEList cnstrs i2)
       ++ ")"
       ++ "\n" ++ indent i1 ++ "(code"
       ++ "\n" ++ (prettySEList stmts i2)
       ++ ")"
       ++ ")"

instance SExpressionable Constraint where
  prettySE  (AllocateIn store space) i =
    "(allocate-in"
    ++ " " ++ prettySE store i
    ++ " " ++ prettySE space i
    ++ ")"
  prettySE  (ImmediateRange imm range) i =
    "(imm"
    ++ " " ++ prettySE imm i
    ++ " " ++ prettySE range i
    ++ ")"
  prettySE  (ImmediateRangeNoZero imm range) i =
    "(zimm"
    ++ " " ++ prettySE imm i
    ++ " " ++ prettySE range i
    ++ ")"
  prettySE  (Alias temp (Just reg)) i =
    "(alias"
    ++ " " ++ prettySE temp i
    ++ " " ++ prettySE reg i
    ++ ")"
  prettySE  (Alias temp Nothing) i =
    "(alias"
    ++ " " ++ prettySE temp i
    ++ " " ++ prettySE noValueStr i
    ++ ")"
  prettySE  (Assert expr) i =
    "(assert"
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE  (RelAddressConstraint imm memClass) i =
    "(rel-address"
    ++ " " ++ prettySE memClass i
    ++ " " ++ prettySE imm i
    ++ ")"
  prettySE  (AbsAddressConstraint imm memClass) i =
    "(abs-address"
    ++ " " ++ prettySE memClass i
    ++ " " ++ prettySE imm i
    ++ ")"

instance SExpressionable AssertExpression where
  prettySE (AssertContainsExpr reg regClass) i =
    "(contains?"
    ++ " " ++ prettySE regClass i
    ++ " " ++ prettySE reg i
    ++ ")"
  prettySE (AssertCompareExpr op (Just size) data1 data2) i =
    "("
    ++ prettySE op i
    ++ " " ++ prettySE size i
    ++ " " ++ prettySE data1 i
    ++ " " ++ prettySE data2 i
    ++ ")"
  prettySE (AssertRegFlagExpr flag) i =
    "()"
    -- TODO: implement
  prettySE (AssertNotExpr expr) i =
    "(not"
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE AssertFalseExpr i =
    "()"
    -- TODO: implement
  prettySE AssertTrueExpr i =
    "()"
    -- TODO: implement
  prettySE (AssertImmediateExpr imm) i =
    prettySE imm i

instance SExpressionable Statement where
  prettySE _ i = "()"
  -- TODO: implement

instance SExpressionable ExprResultSize where
  prettySE (ERSRegSize reg) i = prettySE reg i
  prettySE (ERSConstValue const) i = prettySE const i
  prettySE (ERSConstTemporary temp) i = prettySE temp i

instance SExpressionable AnyData where
  prettySE (ADTemporary temp) i = prettySE temp i
  prettySE (ADRegister reg) i = prettySE reg i
  prettySE (ADRegisterFlag flag) i = prettySE flag i
  prettySE (ADConstant const) i = prettySE const i
  prettySE (ADImmediate imm) i = prettySE imm i
  prettySE ADNoValue i = prettySE noValueStr i

instance SExpressionable AnyStorage where
  prettySE  (ASTemporary temp) i = prettySE temp i
  prettySE  (ASRegister reg) i = prettySE reg i
  prettySE  (ASRegisterFlag flag) i = prettySE flag i

instance SExpressionable Temporary where
  prettySE (Temporary int) i = "(tmp " ++ prettySE int i ++ ")"

instance SExpressionable Register where
  prettySE (RegByTemporary temp) i = prettySE temp i
  prettySE (RegBySymbol sym) i = prettySE sym i

instance SExpressionable RegisterSymbol where
  prettySE (RegisterSymbol str) i = prettySE str i

instance SExpressionable RegisterFlag where
  prettySE (RegisterFlag sym reg) i =
    "(reg-flag " ++ prettySE sym i ++ " " ++ prettySE reg i ++ ")"

instance SExpressionable RegisterFlagSymbol where
  prettySE (RegisterFlagSymbol str) i = prettySE str i

instance SExpressionable ImmediateSymbol where
  prettySE (ImmediateSymbol str) i = prettySE str i

instance SExpressionable AnyStorageSpace where
  prettySE (ASSRegisterFlag flag) i = prettySE flag i
  prettySE (ASSDataSpace space) i = prettySE space i

instance SExpressionable DataSpace where
  prettySE (DSRegisterClass regClass) i = prettySE regClass i
  prettySE (DSMemoryClass memClass) i = prettySE memClass i

instance SExpressionable RegisterClass where
  prettySE (RegisterClass str) i =
    "(register-class"
    ++ " " ++ str
    ++ ")"

instance SExpressionable MemoryClass where
  prettySE (MemoryClass str range) i = str ++ " " ++ prettySE range i

instance SExpressionable ConstantValue where
  prettySE (ConstIntValue int) i = prettySE int i

instance SExpressionable (Range Integer) where
  prettySE (Range lower upper) i = prettySE lower i ++ " " ++ prettySE upper i

noValueStr = "no-value"
