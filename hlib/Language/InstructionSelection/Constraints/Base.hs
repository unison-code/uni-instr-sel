--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Constraints.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO: write description
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Constraints.Base (
-- TODO: add export list
) where

import Language.InstructionSelection.Graphs (NodeId)
import Language.InstructionSelection.Patterns ( InstanceId
                                              , InstructionId
                                              , PatternId
                                              )



--------------
-- Data types
--------------

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
    = PluxExpr  NumExpr NumExpr
    | MinuxExpr NumExpr NumExpr

      -- | Introduces an integer.

    | IntExpr Integer

      -- | Converts a node ID to a numerical expression.

    | NodeId2NumExpr NodeIdExpr

      -- | Converts a pattern instance ID to a numerical expression.

    | InstanceId2NumExpr InstanceIdExpr

      -- | Converts an instruction ID to a numerical expression.

    | InstructionId2NumExpr InstructionIdExpr

      -- | Converts a pattern ID to a numerical expression.

    | PatternId2NumExpr PatternIdExpr

    deriving (Show)

-- | Node ID expressions.

data NodeIdExpr

      -- | Introduces a node ID.

    = NodeIdExpr NodeId

    -- TODO: add missing functions

    deriving (Show)

-- | Instance ID expressions.

data InstanceIdExpr

      -- | Introduces a pattern instance ID.

    = InstanceIdExpr InstanceId

      -- | Gets the pattern instance ID which covers a certain action node.

    | CovererOfActionExpr NodeId

      -- | Gets the pattern instance ID which defines a certain entity node.

    | DefinerOfEntityExpr NodeId

    -- TODO: add missing functions

    deriving (Show)

-- | Instruction ID expressions.

data InstructionIdExpr

      -- | Introduces an instruction ID.

    = InstructionIdExpr InstructionId

      -- | Gets the instruction ID to which a pattern belongs.

    | InstructionIdOfPatternExpr PatternIdExpr

    -- TODO: add missing functions

    deriving (Show)

-- | Pattern ID expressions.

data PatternIdExpr

     -- | Introduces a pattern ID.

    = PatternIdExpr PatternId

      -- | Gets the pattern ID to which a pattern instance is derived from.

    | PatternIdOfInstanceExpr InstanceIdExpr

    -- TODO: add missing functions

    deriving (Show)
