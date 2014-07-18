--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Constraints.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing instruction selection constraints.
--
--------------------------------------------------------------------------------

module Language.InstSel.Constraints.Base where

import Language.InstSel.Graphs.IDs
import Language.InstSel.Patterns.IDs
import Language.InstSel.TargetMachine.IDs



--------------
-- Data types
--------------

data Constraint

      -- | A constraint represented as a Boolean expression.

    = BoolExprConstraint { boolExpr :: BoolExpr }

    deriving (Show)

-- | Boolean expressions. For binary operations the first argument is always the
-- left-hand side and the second argument is always the right-hand side.

data BoolExpr

      -- | Equals.

    = EqExpr  NumExpr  NumExpr

      -- | Not equals.

    | NeqExpr NumExpr  NumExpr

      -- | Greater than.

    | GTExpr  NumExpr  NumExpr

      -- | Greater than or equals.

    | GEExpr  NumExpr  NumExpr

      -- | Less than.

    | LTExpr  NumExpr  NumExpr

      -- | Less than or equals.

    | LEExpr  NumExpr  NumExpr
    | AndExpr BoolExpr BoolExpr
    | OrExpr  BoolExpr BoolExpr

      -- | Implication.

    | ImpExpr BoolExpr BoolExpr

      -- | Equivalence.

    | EqvExpr BoolExpr BoolExpr
    | NotExpr BoolExpr
    | InSetExpr SetElemExpr SetExpr

      -- | An expression indicating that a particular data node represents a
      -- constant integer value.

    | DataNodeIsAnIntConstantExpr NodeExpr

      -- | An expression indicating that a particular data node represents an
      -- intermediate data value, meaning that its value cannot be reused by
      -- another pattern instance.

    | DataNodeIsIntermediateExpr NodeExpr

    deriving (Show)

-- | Numerical expressions. For binary operations the first argument is always
-- the left-hand side and the second argument is always the right-hand side.

data NumExpr
    = PlusExpr  NumExpr NumExpr
    | MinusExpr NumExpr NumExpr

      -- | Converts an integer value to a numerical expression.

    | Int2NumExpr IntExpr

      -- | Converts a Boolean value to a numerical expression.

    | Bool2NumExpr BoolExpr

      -- | Converts a node to a numerical expression.

    | Node2NumExpr NodeExpr

      -- | Converts a pattern instance to a numerical expression.

    | PatternInstance2NumExpr PatternInstanceExpr

      -- | Converts an instruction to a numerical expression.

    | Instruction2NumExpr InstructionExpr

      -- | Converts a pattern to a numerical expression.

    | Pattern2NumExpr PatternExpr

      -- | Converts a label to a numerical expression.

    | Label2NumExpr LabelExpr

      -- | Converts a register to a numerical expression.

    | Register2NumExpr RegisterExpr

      -- | Represents the distance between a pattern instance and a label. The
      -- distance starts from the end of the instruction represented by the
      -- pattern and stops at the beginning of the first instruction within the
      -- basic block represented by the label. The distance is negative if the
      -- label appears before the pattern.

    | DistanceBetweenInstanceAndLabelExpr PatternInstanceExpr LabelExpr

    deriving (Show)

-- | Integer value expressions.

data IntExpr

      -- | Introduces an integer value.

    = AnIntegerExpr Integer

      -- | Retrieves the value of a data node which represents an integer
      -- constant. This expression *must* be used together with
      -- 'DataNodeIsIntConstantConstraint'!

    | IntConstValueOfDataNodeExpr NodeExpr

    deriving (Show)

-- | Node expressions.

data NodeExpr

      -- | Introduces a node ID.

    = ANodeIDExpr NodeID

    deriving (Show)

-- | Instance expressions.

data PatternInstanceExpr

      -- | Introduces a pattern instance ID.

    = APatternInstanceIDExpr PatternInstanceID

      -- | Retrieves the pattern instance in which this expression appears.

    | ThisPatternInstanceExpr

      -- | Retrieves the pattern instance which covers a certain action node.

    | CovererOfActionNodeExpr NodeExpr

      -- | Retrieves the pattern instance which defines a certain data node.

    | DefinerOfDataNodeExpr NodeExpr

      -- | Retrieves the pattern instance which defines a certain state node.

    | DefinerOfStateNodeExpr NodeExpr

    deriving (Show)

-- | Instruction expressions.

data InstructionExpr

      -- | Introduces an instruction ID.

    = AnInstructionIDExpr InstructionID

      -- | Retrieves the instruction to which a pattern belongs.

    | InstructionOfPatternExpr PatternExpr

    deriving (Show)

-- | Pattern expressions.

data PatternExpr

     -- | Introduces a pattern ID.

    = APatternIDExpr PatternID

      -- | Retrieves the pattern to which a pattern instance is derived from.

    | PatternOfPatternInstanceExpr PatternInstanceExpr

    deriving (Show)

-- | Label expressions.

data LabelExpr

      -- | Retrieves the of the label to which a pattern instance has been
      -- allocated.

    = LabelAllocatedToPatternInstanceExpr PatternInstanceExpr

      -- | Retrieves the label associated with a label node.

    | LabelOfLabelNodeExpr NodeExpr

    deriving (Show)

-- | Register expressions.

data RegisterExpr

      -- | Introduces a register ID.

    = ARegisterIDExpr RegisterID

      -- | Retrieves the of the register to which a data node has been
      -- allocated.

    | RegisterAllocatedToDataNodeExpr NodeExpr

    deriving (Show)

-- | Set construction expressions.

data SetExpr

    = UnionSetExpr SetExpr SetExpr
    | IntersectSetExpr SetExpr SetExpr

      -- | @A@ `diff` @B@

    | DiffSetExpr

          -- | Set @A@.

          SetExpr

          -- | Set @B@.

          SetExpr

      -- | Retrieves the dominator set of a label.

    | DomSetOfLabelExpr LabelExpr

      -- | Retrieves a register class (which is expressed as a set of individual
      -- registers belonging to that class).

    | RegisterClassExpr [RegisterExpr]

    deriving (Show)



-- | Set element expressions.

data SetElemExpr

      -- | Converts a label to a set element expression.

    = Label2SetElemExpr LabelExpr

      -- | Converts a register to a set element expression.

    | Register2SetElemExpr RegisterExpr

    deriving (Show)
