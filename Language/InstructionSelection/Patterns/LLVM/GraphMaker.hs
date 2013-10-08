--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.LLVM.GraphMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts an LLVM pattern into the internal graph format.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.LLVM.GraphMaker (
  graphify
) where

import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Patterns.LLVM
import Data.Graph.Inductive.Graph hiding (Graph)



graphify :: [Statement] -> Graph
graphify stmts = Graph empty
-- | TODO: implement
