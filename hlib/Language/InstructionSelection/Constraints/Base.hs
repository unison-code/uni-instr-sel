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
, InstanceIdExpr (..)
, InstructionIdExpr (..)
, IntExpr (..)
, LabelIdExpr (..)
, NodeIdExpr (..)
, NumExpr (..)
, PatternIdExpr (..)
, RegisterIdExpr (..)
, SetElemExpr (..)
, SetExpr (..)
) where

import Language.InstructionSelection.Graphs (NodeId)
import Language.InstructionSelection.Patterns.Ids
import Language.InstructionSelection.TargetMachine (RegisterId)



--------------
-- Data types
--------------

data Constraint

      -- | A constraint represented as a Boolean expression.

    = BoolExprConstraint { boolExpr :: BoolExpr }

      -- | A constraint indicating that a particular node represents a constant
      -- integer value. Constraints on the value itself are provided via the
      -- 'BoolExprConstraint'.

    | IsIntConstantConstraint { intConstNode :: NodeId }

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

    | NodeId2NumExpr NodeIdExpr

      -- | Converts a pattern instance ID to a numerical expression.

    | InstanceId2NumExpr InstanceIdExpr

      -- | Converts an instruction ID to a numerical expression.

    | InstructionId2NumExpr InstructionIdExpr

      -- | Converts a pattern ID to a numerical expression.

    | PatternId2NumExpr PatternIdExpr

      -- | Converts a label ID to a numerical expression.

    | LabelId2NumExpr LabelIdExpr

      -- | Converts a register ID to a numerical expression.

    | RegisterId2NumExpr RegisterIdExpr

      -- | Represents the distance between a pattern instance and a label. The
      -- distance starts from the end of the instruction represented by the
      -- pattern and stops at the beginning of the first instruction within the
      -- basic block represented by the label. The distance is negative if the
      -- label appears before the pattern.

    | DistanceBetweenInstanceAndLabelExpr InstanceIdExpr LabelIdExpr

    deriving (Show)

-- | Integer value expressions.

data IntExpr

      -- | Introduces an integer value.

    = AnIntegerExpr Integer

      -- | Retrieves the value of a data node which represents an integer
      -- constant. This expression *must* be used together with
      -- 'IsIntConstantConstraint'!

    | IntConstValueOfDataNodeExpr NodeIdExpr

    deriving (Show)

-- | Node ID expressions.

data NodeIdExpr

      -- | Introduces a node ID.

    = ANodeIdExpr NodeId

    -- TODO: add missing functions

    deriving (Show)

-- | Instance ID expressions.

data InstanceIdExpr

      -- | Introduces a pattern instance ID.

    = AnInstanceIdExpr InstanceId

      -- | Retrieves the ID of this pattern instance ID.

    | ThisInstanceIdExpr

      -- | Retrieves the pattern instance ID which covers a certain action node.

    | CovererOfActionNodeExpr NodeIdExpr

      -- | Retrieves the pattern instance ID which defines a certain data node.

    | DefinerOfDataNodeExpr NodeIdExpr

      -- | Retrieves the pattern instance ID which defines a certain state node.

    | DefinerOfStateNodeExpr NodeIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Instruction ID expressions.

data InstructionIdExpr

      -- | Introduces an instruction ID.

    = AnInstructionIdExpr InstructionId

      -- | Retrieves the instruction ID to which a pattern belongs.

    | InstructionIdOfPatternExpr PatternIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Pattern ID expressions.

data PatternIdExpr

     -- | Introduces a pattern ID.

    = APatternIdExpr PatternId

      -- | Retrieves the pattern ID to which a pattern instance is derived from.

    | PatternIdOfInstanceExpr InstanceIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Label ID expressions.

data LabelIdExpr

      -- | Retrieves the ID of the label to which a pattern instance has been
      -- allocated.

    = LabelIdAllocatedToInstanceExpr InstanceIdExpr

      -- | Retrieves the label ID associated with a label node.

    | LabelIdOfLabelNodeExpr NodeIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Register ID expressions.

data RegisterIdExpr

      -- | Introduces a register ID.

    = ARegisterIdExpr RegisterId

      -- | Retrieves the ID of the register to which a data node has been
      -- allocated.

    | RegisterIdAllocatedToDataNodeExpr NodeIdExpr

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

    | DomSetOfLabelIdExpr LabelIdExpr

      -- | Retrieves a register class (which is expressed as a set of individual
      -- registers belonging to that class).

    | RegisterClassExpr [RegisterIdExpr]

    deriving (Show)



-- | Set element expressions.

data SetElemExpr

      -- | Converts a label ID to a set element expression.

    = LabelId2SetElemExpr LabelIdExpr

      -- | Converts a register ID to a set element expression.

    | RegisterId2SetElemExpr RegisterIdExpr

    deriving (Show)
