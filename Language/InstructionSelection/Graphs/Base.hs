--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Graphs.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing graphs which are
-- specialized for representing program functions and patterns.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.Base where

import Data.Graph.Inductive
import Language.InstructionSelection.Utils
import Language.InstructionSelection.OpTypes



type NodeId = Natural
type EdgeId = Natural

data BBLabel
    = BBLabel String
    deriving (Show,Eq)

data NodeType
    = NTBinaryOp BinaryOp
    | NTUnaryOp UnaryOp
    | NTMemoryLoad
    | NTMemoryStore
    | NTUncondBranch BBLabel
    | NTCondBranch

          -- | Label taken if the register evaluates to @True@.

          BBLabel

          -- | Label taken if the register evaluates to @False@.

          BBLabel

    | NTPhi

    -- | Both temporary nodes (appearing in IR and pattern code) and register
    -- nodes (appearing only in pattern code) are represented as registers.
    -- For the latter, the specific register is specified as a constraint.

    | NTRegister

    -- | Both constant values (appearing in IR and pattern code) and immediates
    -- (appearing only in pattern code) are represented as constants. For the
    -- former, the specific value is specified as a constraint.

    | NTConstant

    deriving (Show,Eq)

data NodeLabel
    = NodeLabel

          -- | Node identifier. Most often this is equal to the 'Node'
          -- identifier used by FGL, but it does not need to be.

          NodeId

          -- | Type of node.

          NodeType

          -- | Label of the basic block that the node belongs to.

          BBLabel

          -- | A field to put arbitrary text in; used when printing the graph.

          String

    deriving (Show,Eq)

data EdgeLabel
    = EdgeLabel

          -- | Source edge number.

          EdgeId

          -- | Destination edge number.

          EdgeId

    deriving (Show,Eq)

data Graph
    = Graph (Gr NodeLabel EdgeLabel)
    deriving (Show)
