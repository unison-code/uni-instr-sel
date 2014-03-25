--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Constraints.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types for representing instruction selection constraints.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Constraints.Base (
  BoolExpr (..)
, Constraint (..)
, InstanceIdExpr (..)
, InstructionIdExpr (..)
, LabelIdExpr (..)
, NodeIdExpr (..)
, NumExpr (..)
, PatternIdExpr (..)
, RegisterIdExpr (..)
, fromConstraint
, toConstraint
) where

import Language.InstructionSelection.Graphs (NodeId)
import Language.InstructionSelection.Machine (RegisterId)
import Language.InstructionSelection.Patterns.Ids



--------------
-- Data types
--------------

newtype Constraint
    = Constraint BoolExpr
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
    deriving (Show)

-- | Numerical expressions. For binary operations the first argument is always
-- the left-hand side and the second argument is always the right-hand side.

data NumExpr
    = PlusExpr  NumExpr NumExpr
    | MinusExpr NumExpr NumExpr

      -- | Introduces an integer.

    | AnIntegerExpr Integer

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

      -- | Represents the ID of this pattern instance ID.

    | ThisInstanceIdExpr

      -- | Represents the pattern instance ID which covers a certain action
      -- node.

    | CovererOfActionNodeExpr NodeIdExpr

      -- | Represents the pattern instance ID which defines a certain entity
      -- node.

    | DefinerOfEntityNodeExpr NodeIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Instruction ID expressions.

data InstructionIdExpr

      -- | Introduces an instruction ID.

    = AnInstructionIdExpr InstructionId

      -- | Represents the instruction ID to which a pattern belongs.

    | InstructionIdOfPatternExpr PatternIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Pattern ID expressions.

data PatternIdExpr

     -- | Introduces a pattern ID.

    = APatternIdExpr PatternId

      -- | Represents the pattern ID to which a pattern instance is derived
      -- from.

    | PatternIdOfInstanceExpr InstanceIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Label ID expressions.

data LabelIdExpr

      -- | Represents the ID of the label to which a pattern instance has been
      -- allocated.

    = LabelIdAllocatedToInstanceExpr InstanceIdExpr

      -- | Represents the label ID associated with a label node.

    | LabelIdOfLabelNodeExpr NodeIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Register ID expressions.

data RegisterIdExpr

      -- | Introduces a register ID.

    = ARegisterIdExpr RegisterId

      -- | Represents the ID of the register to which a data node has been
      -- allocated.

    | RegisterIdAllocatedToDataNodeExpr NodeIdExpr

    -- TODO: add missing functions

    deriving (Show)



-------------
-- Functions
-------------

toConstraint :: BoolExpr -> Constraint
toConstraint = Constraint

fromConstraint :: Constraint -> BoolExpr
fromConstraint (Constraint e) = e
