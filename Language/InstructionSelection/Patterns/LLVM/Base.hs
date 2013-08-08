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
    deriving (Show, Eq)

-- | Record for describing a register flag. All flags are associated with a
-- specific register.

data RegisterFlag
    = RegisterFlag RegisterFlagSymbol Register
    deriving (Show)

-- | Record for describing a register class.

data RegisterClass
    = RegisterClass

          -- | Set of registers.

          [RegisterSymbol]

    deriving (Show)

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
    deriving (Show)

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

          -- | Size (in bits) of result.

          (Maybe ExprResultSize)

          -- | LHS.

          StmtExpression

          -- | RHS.

          StmtExpression

      -- | A unary expression.

    | UnaryOpStmtExpr UnaryOp (Maybe ExprResultSize) StmtExpression

      -- | A load expression.

    | LoadStmtExpr

          -- | Memory area to load from.

          String

          -- | Size (in bits) of result.

          ExprResultSize

          -- | Memory address expression.

          StmtExpression

      -- | A float-to-int conversion expression.

    | FP2IStmtExpr

          -- | Size (in bits) of source.

          ExprResultSize

          -- | Expression to convert.

          StmtExpression

          -- | Size (in bits) of result.

          ExprResultSize

      -- | Truncates a value to the nearest integer not larger than the
      -- magnitude of the operand.

    | TruncStmtExpr

          -- | Size (in bits) of source.

          ExprResultSize

          -- | Expression to convert.

          StmtExpression

          -- | Size (in bits) of result.

          ExprResultSize

      -- | A phi expression.

    | PhiStmtExpr [PhiElement]

      -- | A data expression.

    | DataStmtExpr ProgramData

      -- | Gets the size (in bits) of a register.

    | SizeStmtExpr Register

      -- | A register range expression, which takes a range of bits from a
      -- register. The same effect can be achieved with a series of bit
      -- operations.

    | RegRangeStmtExpr Register (Range AnyData)

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

      -- | Stores the result of an expression to a memory location.

    | StoreStmt

          -- | Location to store in.

          StmtExpression

          -- | Memory area to store in.

          String

          -- | Size (in bits) of value to store.

          ExprResultSize

          -- | Value to store.

          StmtExpression

      -- | Performs an unconditional branch (or jump) to a label.

    | UncondBranchStmt Label

      -- | Performs an conditional branch (or jump) to a label.

    | CondBranchStmt
          Register

          -- | Label taken if the register evaluates to @True@.

          Label

          -- | Label taken if the register evaluates to @False@.

          Label

      -- | Declares a label.

    | LabelStmt Label

    deriving (Show)

-- | Record for a pattern constraint.

data Constraint

      -- | The @AllocateInConstraint@ constraint dictates that a storage unit
      -- must be located in a particular storage space.

    = AllocateInConstraint AnyStorage AnyStorageSpace

      -- | The @ImmediateConstraint@ constraint limits the range of values that
      -- an immediate value may take.

    | ImmediateConstraint ImmediateSymbol [Range Integer]

      -- | The @RegFlagConstraint@ constraint dictates that a register flag
      -- must be assigned any of the values within the set of ranges.

    | RegFlagConstraint RegisterFlag [Range Integer]

      -- | The @AliasesConstraint@ constraint indicates that two or more
      -- temporaries (or registers) are actually the same (i.e. one temporary is
      -- simply a name alias for another).

    | AliasesConstraint [[AliasValue]]

      -- | The @RelAddressConstraint@ constraint forces an immediate value to be
      -- within a certain relative memory address range.

    | RelAddressConstraint ImmediateSymbol MemoryClass

      -- | Same as for @RelAddressConstraint@ but for absolute values.

    | AbsAddressConstraint ImmediateSymbol MemoryClass

    deriving (Show)

-- | Record for containing values used for aliasing.

data AliasValue
    = AVTemporary Temporary
    | AVRegister Register
    | AVNoValue
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
       ++ "\n" ++ indent i1 ++ prettySE ass i1
       ++ prettySEList pats i1
       ++ ")"

instance SExpressionable AssemblyString where
  prettySE (AssemblyString str) i = str

instance SExpressionable Pattern where
  prettySE (Pattern stmts cnstrs) i =
    let i1 = i + 1
        i2 = i + 2
    in "(pattern"
       ++ "\n" ++ indent i1 ++ "(constraints"
       ++ prettySEList cnstrs i2
       ++ ")"
       ++ "\n" ++ indent i1 ++ "(code"
       ++ prettySEList stmts i2
       ++ ")"
       ++ ")"

instance SExpressionable Constraint where
  prettySE (AllocateInConstraint store space) i =
    "(allocate-in"
    ++ " " ++ prettySE store i
    ++ " " ++ prettySE space i
    ++ ")"
  prettySE (ImmediateConstraint imm ranges) i =
    "(immediate"
    ++ " " ++ prettySE imm i
    ++ " (" ++ prettySEListNoBreak ranges i ++ ")"
    ++ ")"
  prettySE (RegFlagConstraint flag ranges) i =
    "(reg-flag"
    ++ " " ++ prettySE flag i
    ++ " (" ++ prettySEListNoBreak ranges i ++ ")"
    ++ ")"
  prettySE (AliasesConstraint aliases) i =
    "(aliases"
    ++ " " ++ prettySEListNoBreak aliases i
    ++ ")"
  prettySE (RelAddressConstraint imm memClass) i =
    "(rel-address"
    ++ " " ++ prettySE imm i
    ++ " " ++ prettySE memClass i
    ++ ")"
  prettySE (AbsAddressConstraint imm memClass) i =
    "(abs-address"
    ++ " " ++ prettySE imm i
    ++ " " ++ prettySE memClass i
    ++ ")"

instance SExpressionable [AliasValue] where
  prettySE values i =
    "("
    ++ prettySEListNoBreak values i
    ++ ")"

instance SExpressionable AliasValue where
  prettySE (AVTemporary tmp) i = prettySE tmp i
  prettySE (AVRegister reg) i = prettySE reg i
  prettySE (AVNoValue) i = prettySE noValueStr i

instance SExpressionable Statement where
  prettySE (AssignmentStmt temp expr) i =
    "(="
    ++ " " ++ prettySE temp i
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE (SetRegStmt reg expr) i =
    "(set-reg"
    ++ " " ++ prettySE reg i
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE (StoreStmt dst area size value) i =
    "(store"
    ++ " " ++ prettySE dst i
    ++ " " ++ prettySE area i
    ++ " " ++ prettySE size i
    ++ " " ++ prettySE value i
    ++ ")"

  prettySE (UncondBranchStmt label) i =
    "(br"
    ++ " " ++ prettySE label i
    ++ ")"
  prettySE (CondBranchStmt reg tLabel fLabel) i =
    "(br"
    ++ " " ++ prettySE reg i
    ++ " " ++ prettySE tLabel i
    ++ " " ++ prettySE fLabel i
    ++ ")"
  prettySE (LabelStmt label) i =
    "(label"
    ++ " " ++ prettySE label i
    ++ ")"

instance SExpressionable StmtExpression where
  prettySE (BinaryOpStmtExpr op (Just result) lhs rhs) i =
    "(" ++ prettySE op i
    ++ " " ++ prettySE result i
    ++ " " ++ prettySE lhs i
    ++ " " ++ prettySE rhs i
    ++ ")"
  prettySE (BinaryOpStmtExpr op Nothing lhs rhs) i =
    "(" ++ prettySE op i
    ++ " " ++ prettySE lhs i
    ++ " " ++ prettySE rhs i
    ++ ")"
  prettySE (UnaryOpStmtExpr op (Just result) expr) i =
    "(" ++ prettySE op i
    ++ " " ++ prettySE result i
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE (UnaryOpStmtExpr op Nothing expr) i =
    "(" ++ prettySE op i
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE (LoadStmtExpr area size src) i =
    "(load-mem"
    ++ " " ++ prettySE area i
    ++ " " ++ prettySE size i
    ++ " " ++ prettySE src i
    ++ ")"
  prettySE (FP2IStmtExpr size_src expr size_dst) i =
    "(fptosi"
    ++ " " ++ prettySE size_src i
    ++ " " ++ prettySE expr i
    ++ " " ++ prettySE size_dst i
    ++ ")"
  prettySE (TruncStmtExpr size_src expr size_dst) i =
    "(trunc"
    ++ " " ++ prettySE size_src i
    ++ " " ++ prettySE expr i
    ++ " " ++ prettySE size_dst i
    ++ ")"
  prettySE (PhiStmtExpr elements) i =
    "(phi"
    ++ " " ++ prettySEListNoBreak elements i
    ++ ")"
  prettySE (DataStmtExpr pdata) i = prettySE pdata i
  prettySE (SizeStmtExpr reg) i = prettySE reg i
  prettySE (RegRangeStmtExpr reg range) i =
    "(reg-range"
    ++ " " ++ prettySE reg i
    ++ " " ++ prettySE range i
    ++ ")"

instance SExpressionable PhiElement where
  prettySE (PhiElement expr label) i =
    "(" ++ prettySE expr i
    ++ " . " ++ prettySE label i
    ++ ")"

instance SExpressionable Label where
  prettySE (Label str) i = prettySE str i

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
  prettySE (RegisterClass regs) i =
    "("
    ++ prettySEListNoBreak regs i
    ++ ")"

instance SExpressionable MemoryClass where
  prettySE (MemoryClass str range) i = str ++ " " ++ prettySE range i

instance SExpressionable ConstantValue where
  prettySE (ConstIntValue int) i =
    "(constant ?"
    ++ " " ++ prettySE int i
    ++ ")"

instance SExpressionable UnaryOp where
  prettySE USqrt _ = "usqrt"
  prettySE Sqrt _ = "sqrt"
  prettySE FixPointSqrt _ = "fixpointsqrt"
  prettySE Not _ = "bit_not"

instance SExpressionable BinaryOp where
  prettySE (BinArithmeticOp op) i = prettySE op i
  prettySE (BinCompareOp op) i = prettySE op i

instance SExpressionable ArithmeticOp where
  prettySE Plus _ = "+"
  prettySE Minus _ = "-"
  prettySE IAdd _ = "add"
  prettySE ISatAdd _ = "satadd"
  prettySE ISub _ = "sub"
  prettySE ISatSub _ = "satsub"
  prettySE IMul _ = "mul"
  prettySE ISatMul _ = "satmul"
  prettySE IUDiv _ = "udiv"
  prettySE ISDiv _ = "sdiv"
  prettySE IURem _ = "urem"
  prettySE ISRem _ = "srem"
  prettySE FixPointDiv _ = "fixpointdiv"
  prettySE FAdd _ = "fadd"
  prettySE FSub _ = "fsub"
  prettySE FMul _ = "fmul"
  prettySE FDiv _ = "fdiv"
  prettySE FRem _ = "frem"
  prettySE Shl _ = "shl"
  prettySE LShr _ = "lhsr"
  prettySE AShr _ = "ashr"
  prettySE And _ = "bit_and"
  prettySE Or _ = "bit_or"
  prettySE Xor _ = "bit_xor"
  prettySE ZExt _ = "zext"
  prettySE SExt _ = "sext"

instance SExpressionable CompareOp where
  prettySE ICmpEq i = "icmp eq"
  prettySE ICmpNEq i = "icmp neq"
  prettySE IUCmpGT i = "icmp ugt"
  prettySE ISCmpGT i = "icmp sgt"
  prettySE IUCmpGE i = "icmp uge"
  prettySE ISCmpGE i = "icmp sge"
  prettySE IUCmpLT i = "icmp ult"
  prettySE ISCmpLT i = "icmp slt"
  prettySE IUCmpLE i = "icmp ule"
  prettySE ISCmpLE i = "icmp sle"
  prettySE FUCmpEq i = "fcmp ueq"
  prettySE FOCmpEq i = "fcmp oeq"
  prettySE FUCmpNEq i = "fcmp une"
  prettySE FOCmpNEq i = "fcmp one"
  prettySE FUCmpGT i = "fcmp ugt"
  prettySE FOCmpGT i = "fcmp ogt"
  prettySE FUCmpGE i = "fcmp uge"
  prettySE FOCmpGE i = "fcmp oge"
  prettySE FUCmpLT i = "fcmp ult"
  prettySE FOCmpLT i = "fcmp olt"
  prettySE FUCmpLE i = "fcmp ule"
  prettySE FOCmpLE i = "fcmp ole"
  prettySE FCmpUn i = "fcmp uno"

instance SExpressionable (Range AnyData) where
  prettySE (Range lower upper) i =
    prettySE lower i
    ++ " " ++ prettySE upper i

instance SExpressionable ConstProgramData where
  prettySE (CPDConstant const) i = prettySE const i
  prettySE (CPDImmediate imm) i = prettySE imm i

instance SExpressionable ProgramData where
  prettySE (PDConstant const) i = prettySE const i
  prettySE (PDImmediate imm) i = prettySE imm i
  prettySE (PDTemporary temp) i = prettySE temp i
  prettySE (PDRegister reg) i = prettySE reg i
  prettySE PDNoValue i = prettySE noValueStr i

noValueStr = "no-value"
