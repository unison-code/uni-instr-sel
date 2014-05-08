--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Patterns.LLVM.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types and records for representing instructions as LLVM
-- patterns.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.LLVM.Base where

import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Utils (Range (..))
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.SExpressions



-- | Data type for representing a physical regsiter.

data Register
    = Register String
    deriving (Show, Eq)

-- | Data type for describing a register flag. All flags are associated with a
-- specific register.

data RegisterFlag
    = RegisterFlag

          -- | Name of the flag itself.

          String

          Register

    deriving (Show, Eq)

-- | Data type for representing a constant value.

data ConstantValue
    = ConstIntValue Integer
    deriving (Show, Eq)

-- | Data type for a temporary.

data Temporary
    = Temporary

          -- | Identifier for the temporary. All temporaries are required to
          -- have a unique identifier within the function scope.

          Integer

    deriving (Show, Eq)

-- | Data type for a symbol, which could basically represent any kind of
-- variable (immediate, register, etc.).

data Symbol
    = Symbol String
    deriving (Show, Eq)

-- | Data type for describing a register class.

data RegisterClass
    = RegisterClass

          -- | Set of registers.

          [Register]

    deriving (Show)

-- | Data type for describing a memory class.

data MemoryClass
    = MemoryClass

          -- | Identifier for the class.

          String

          -- | Address range within the memory class. This may be the entire
          -- range of the memory space, or a restricted subset.

          (Range Integer)

    deriving (Show, Eq)

-- | Data type for describing a data space.

data DataSpace
    = DSRegisterClass RegisterClass
    | DSRegisterFlag RegisterFlag
    | DSMemoryClass MemoryClass
    deriving (Show)

-- | Data type for describing program data. The data can be of many different
-- types, e.g. a constant, an immediate, a temporary, etc.

data ProgramData

      -- | A value which is fixed and known at compile time.

    = PDConstant ConstantValue

      -- | A value represented by a symbol.

    | PDSymbol Symbol

      -- | A value located in a temporary.

    | PDTemporary Temporary

      -- | A value located in a specific register.

    | PDRegister Register

      -- | An undefined value.

    | PDNoValue

    deriving (Show)

-- | Data type for describing program storage. The storage can be represented
-- through many different types, e.g. symbol, temporary, or register.

data ProgramStorage

      -- | A space represented by a symbol.

    = PSSymbol Symbol

      -- | A value located in a temporary.

    | PSTemporary Temporary

      -- | A value located in a specific register.

    | PSRegister Register

    deriving (Show)

-- | Data type for representing an expression for a statement.

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

      -- | A bit size modification expression.

    | TypeConvStmtExpr
          TypeConversionOp

          -- | The 'to' bit size.

          ExprResultSize

          -- | The 'from' bit size.

          ExprResultSize

          StmtExpression

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

      -- | A phi expression.

    | PhiStmtExpr [PhiElement]

      -- | A data expression.

    | DataStmtExpr ProgramData

      -- | Gets the size (in bits) of a register.

    | SizeStmtExpr Register

      -- | A register range expression, which takes a range of bits from a
      -- register. The same effect can be achieved with a series of bit
      -- operations.

    | RegRangeStmtExpr ProgramStorage (Range ProgramData)

    deriving (Show)

-- | Data type for representing the size of the result of an expression.

data ExprResultSize

      -- | Size of a given register.

    = ERSRegSize Register

      -- | Constant size represented via a constant value.

    | ERSConstValue ConstantValue

      -- | Constant size represented via a temporary.

    | ERSConstTemporary Temporary

      -- | Constant size represented via an immediate.

    | ERSConstSymbol Symbol

    deriving (Show)

-- | Data type for containing an element inside a phi function. The label
-- indicates from where the value comes from.

data PhiElement
    = PhiElement StmtExpression Label
    deriving (Show)

-- | Data type for containing a label.

data Label
    = Label

          -- | Label identifier.

          String

    deriving (Show, Eq)

-- | Data type for expressing the destination in a 'set-reg' statement.

data SetRegDestination
    = SRDRegister Register
    | SRDRegisterFlag RegisterFlag
    | SRDSymbol Symbol
    | SRDTemporary Temporary
    deriving (Show, Eq)

-- | Data type for representing an LLVM statement.

data Statement

      -- | Assigns the result of an expression to a temporary.

    = AssignmentStmt Temporary StmtExpression

      -- | Assigns the result of an expression to a register, register flag,
      -- register symbol, or a temporary (which in turn will reference to a
      -- specific register).

    | SetRegStmt SetRegDestination StmtExpression

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
          ProgramData

          -- | Label taken if the register evaluates to @True@.

          Label

          -- | Label taken if the register evaluates to @False@.

          Label

      -- | Declares a label.

    | LabelStmt Label

    deriving (Show)

-- | Data type for a pattern constraint.

data Constraint

      -- | The @AllocateInConstraint@ constraint dictates that a storage unit
      -- must be located in a particular storage space.

    = AllocateInConstraint (Either Temporary Symbol) DataSpace

      -- | The @ImmediateConstraint@ constraint limits the range of values that
      -- an immediate value may take.

    | ImmediateConstraint Symbol [Range Integer]

      -- | The @RegFlagConstraint@ constraint dictates that a register flag
      -- must be assigned any of the values within the set of ranges.

    | RegFlagConstraint RegisterFlag [Range Integer]

      -- | The @AliasesConstraint@ constraint indicates that two or more
      -- temporaries (or registers) are actually the same (i.e. one temporary is
      -- simply a name alias for another).

    | AliasesConstraint [[AliasValue]]

      -- | The @RelAddressConstraint@ constraint forces an immediate value to be
      -- within a certain relative memory address range.

    | RelAddressConstraint Symbol MemoryClass

      -- | Same as for @RelAddressConstraint@ but for absolute values.

    | AbsAddressConstraint Symbol MemoryClass

    deriving (Show)

isAllocateInConstraint :: Constraint -> Bool
isAllocateInConstraint (AllocateInConstraint _ _) = True
isAllocateInConstraint _ = False

isImmediateConstraint :: Constraint -> Bool
isImmediateConstraint (ImmediateConstraint _ _) = True
isImmediateConstraint _ = False

isRegFlagConstraint :: Constraint -> Bool
isRegFlagConstraint (RegFlagConstraint _ _) = True
isRegFlagConstraint _ = False

isAliasesConstraint :: Constraint -> Bool
isAliasesConstraint (AliasesConstraint _) = True
isAliasesConstraint _ = False

isRelAddressConstraint :: Constraint -> Bool
isRelAddressConstraint (RelAddressConstraint _ _) = True
isRelAddressConstraint _ = False

isAbsAddressConstraint :: Constraint -> Bool
isAbsAddressConstraint (AbsAddressConstraint _ _) = True
isAbsAddressConstraint _ = False

-- | Data type for containing values used for aliasing.

data AliasValue
    = AVTemporary Temporary
    | AVSymbol Symbol
    | AVNoValue
    deriving (Show, Eq)

isAVTemporary :: AliasValue -> Bool
isAVTemporary (AVTemporary _) = True
isAVTemporary _ = False

isAVSymbol :: AliasValue -> Bool
isAVSymbol (AVSymbol _) = True
isAVSymbol _ = False

isAVNoValue :: AliasValue -> Bool
isAVNoValue AVNoValue = True
isAVNoValue _ = False

-- | Data type for representing a pattern including the constraints.

data Pattern
    = Pattern

          -- | The LLVM statements.

          [Statement]

          -- | Constraints that must be enforced for the pattern.

          [Constraint]

    deriving (Show)

-- | Data type for representing an instruction.

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
    ++ " (" ++ prettySEListNoBreak aliases i
    ++ "))"
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

instance SExpressionable AliasValue where
  prettySE (AVTemporary tmp) i = prettySE tmp i
  prettySE (AVSymbol sym) i = prettySE sym i
  prettySE (AVNoValue) i = prettySE noValueStr i

instance SExpressionable Statement where
  prettySE (AssignmentStmt temp expr) i =
    "(="
    ++ " " ++ prettySE temp i
    ++ " " ++ prettySE expr i
    ++ ")"
  prettySE (SetRegStmt dest expr) i =
    "(set-reg"
    ++ " " ++ prettySE dest i
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
  prettySE (CondBranchStmt d tLabel fLabel) i =
    "(br"
    ++ " " ++ prettySE d i
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
  prettySE (TypeConvStmtExpr op size_dst size_src expr) i =
    "(" ++ prettySE op i
    ++ " " ++ prettySE size_src i
    ++ " " ++ prettySE expr i
    ++ " " ++ prettySE size_dst i
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
  prettySE (PhiStmtExpr elements) i =
    "(phi"
    ++ " " ++ prettySEListNoBreak elements i
    ++ ")"
  prettySE (DataStmtExpr pdata) i = prettySE pdata i
  prettySE (SizeStmtExpr reg) i = prettySE reg i
  prettySE (RegRangeStmtExpr sto range) i =
    "(reg-range"
    ++ " " ++ prettySE sto i
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
  prettySE (ERSConstValue c) i = prettySE c i
  prettySE (ERSConstTemporary temp) i = prettySE temp i
  prettySE (ERSConstSymbol sym) i = prettySE sym i

instance SExpressionable Temporary where
  prettySE (Temporary int) i = "(tmp " ++ prettySE int i ++ ")"

instance SExpressionable Register where
  prettySE (Register str) i = prettySE str i

instance SExpressionable Symbol where
  prettySE (Symbol str) i = prettySE str i

instance SExpressionable RegisterFlag where
  prettySE (RegisterFlag sym reg) i =
    "(reg-flag " ++ prettySE sym i ++ " " ++ prettySE reg i ++ ")"

instance SExpressionable DataSpace where
  prettySE (DSRegisterClass rc) i = prettySE rc i
  prettySE (DSRegisterFlag flag) i = prettySE flag i
  prettySE (DSMemoryClass mc) i = prettySE mc i

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

instance SExpressionable ProgramData where
  prettySE (PDConstant c) i = prettySE c i
  prettySE (PDSymbol sym) i = prettySE sym i
  prettySE (PDTemporary temp) i = prettySE temp i
  prettySE (PDRegister reg) i = prettySE reg i
  prettySE PDNoValue i = prettySE noValueStr i

instance SExpressionable SetRegDestination where
  prettySE (SRDRegister reg) i = prettySE reg i
  prettySE (SRDRegisterFlag flag) i = prettySE flag i
  prettySE (SRDSymbol reg) i = prettySE reg i
  prettySE (SRDTemporary temp) i = prettySE temp i

instance SExpressionable ProgramStorage where
  prettySE (PSSymbol sym) i = prettySE sym i
  prettySE (PSTemporary temp) i = prettySE temp i
  prettySE (PSRegister reg) i = prettySE reg i

noValueStr :: [Char]
noValueStr = "no-value"
