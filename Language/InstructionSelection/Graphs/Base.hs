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



data Label
    = Label String
    deriving (Show)

data NodeType
    = NTBinaryOp BinaryOp
    | NTUnaryOp UnaryOp
    | NTMemoryLoad
    | NTMemoryStore
    | NTUncondBranch Label
    | NTCondBranch

          -- | Label taken if the register evaluates to @True@.

          Label

          -- | Label taken if the register evaluates to @False@.

          Label

    | NTPhi
    | NTData
    deriving (Show)

data NodeLabel
    = NodeLabel

          -- | Node identifier.

          Natural

          NodeType

    deriving (Show)

data EdgeLabel
    = EdgeType

          -- | Source identifier.

          Natural

          -- | Destination identifier.

          Natural

    deriving (Show)

data Graph
    = Graph (Gr (LNode NodeLabel) (LEdge EdgeLabel))
    deriving (Show)
