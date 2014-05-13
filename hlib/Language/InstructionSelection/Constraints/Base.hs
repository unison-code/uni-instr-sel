--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Constraints.Base
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

module Language.InstructionSelection.Constraints.Base (
  BoolExpr (..)
, Constraint (..)
, PatternInstanceIDExpr (..)
, InstructionIDExpr (..)
, IntExpr (..)
, LabelIDExpr (..)
, NodeIDExpr (..)
, NumExpr (..)
, PatternIDExpr (..)
, RegisterIDExpr (..)
, SetElemExpr (..)
, SetExpr (..)
) where

import Language.InstructionSelection.Graphs (NodeID)
import Language.InstructionSelection.Patterns.IDs
import Language.InstructionSelection.TargetMachine (RegisterID)



--------------
-- Data types
--------------

data Constraint

      -- | A constraint represented as a Boolean expression.

    = BoolExprConstraint { boolExpr :: BoolExpr }

      -- | A constraint indicating that a particular data node represents a
      -- constant integer value. Constraints on the value itself are provided
      -- via the 'BoolExprConstraint'.

    | DataNodeIsIntConstantConstraint { intConstDataNode :: NodeID }

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

      -- | Converts a node ID to a numerical expression.

    | NodeID2NumExpr NodeIDExpr

      -- | Converts a pattern instance ID to a numerical expression.

    | PatternInstanceID2NumExpr PatternInstanceIDExpr

      -- | Converts an instruction ID to a numerical expression.

    | InstructionID2NumExpr InstructionIDExpr

      -- | Converts a pattern ID to a numerical expression.

    | PatternID2NumExpr PatternIDExpr

      -- | Converts a label ID to a numerical expression.

    | LabelID2NumExpr LabelIDExpr

      -- | Converts a register ID to a numerical expression.

    | RegisterID2NumExpr RegisterIDExpr

      -- | Represents the distance between a pattern instance and a label. The
      -- distance starts from the end of the instruction represented by the
      -- pattern and stops at the beginning of the first instruction within the
      -- basic block represented by the label. The distance is negative if the
      -- label appears before the pattern.

    | DistanceBetweenInstanceAndLabelExpr PatternInstanceIDExpr LabelIDExpr

    deriving (Show)

-- | Integer value expressions.

data IntExpr

      -- | Introduces an integer value.

    = AnIntegerExpr Integer

      -- | Retrieves the value of a data node which represents an integer
      -- constant. This expression *must* be used together with
      -- 'IsIntConstantConstraint'!

    | IntConstValueOfDataNodeExpr NodeIDExpr

    deriving (Show)

-- | Node ID expressions.

data NodeIDExpr

      -- | Introduces a node ID.

    = ANodeIDExpr NodeID

    -- TODO: add missing functions

    deriving (Show)

-- | Instance ID expressions.

data PatternInstanceIDExpr

      -- | Introduces a pattern instance ID.

    = APatternInstanceIDExpr PatternInstanceID

      -- | Retrieves the ID of this pattern instance ID.

    | ThisPatternInstanceIDExpr

      -- | Retrieves the pattern instance ID which covers a certain action node.

    | CovererOfActionNodeExpr NodeIDExpr

      -- | Retrieves the pattern instance ID which defines a certain data node.

    | DefinerOfDataNodeExpr NodeIDExpr

      -- | Retrieves the pattern instance ID which defines a certain state node.

    | DefinerOfStateNodeExpr NodeIDExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Instruction ID expressions.

data InstructionIDExpr

      -- | Introduces an instruction ID.

    = AnInstructionIDExpr InstructionID

      -- | Retrieves the instruction ID to which a pattern belongs.

    | InstructionIDOfPatternExpr PatternIDExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Pattern ID expressions.

data PatternIDExpr

     -- | Introduces a pattern ID.

    = APatternIDExpr PatternID

      -- | Retrieves the pattern ID to which a pattern instance is derived from.

    | PatternIDOfInstanceExpr PatternInstanceIDExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Label ID expressions.

data LabelIDExpr

      -- | Retrieves the ID of the label to which a pattern instance has been
      -- allocated.

    = LabelIDAllocatedToInstanceExpr PatternInstanceIDExpr

      -- | Retrieves the label ID associated with a label node.

    | LabelIDOfLabelNodeExpr NodeIDExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Register ID expressions.

data RegisterIDExpr

      -- | Introduces a register ID.

    = ARegisterIDExpr RegisterID

      -- | Retrieves the ID of the register to which a data node has been
      -- allocated.

    | RegisterIDAllocatedToDataNodeExpr NodeIDExpr

    -- TODO: add missing functions

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

      -- | Retrieves the dominator set of a label ID.

    | DomSetOfLabelIDExpr LabelIDExpr

      -- | Retrieves a register class (which is expressed as a set of individual
      -- registers belonging to that class).

    | RegisterClassExpr [RegisterIDExpr]

    deriving (Show)



-- | Set element expressions.

data SetElemExpr

      -- | Converts a label ID to a set element expression.

    = LabelID2SetElemExpr LabelIDExpr

      -- | Converts a register ID to a set element expression.

    | RegisterID2SetElemExpr RegisterIDExpr

    deriving (Show)
