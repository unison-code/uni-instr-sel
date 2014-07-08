--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.CPModel.PostProcessor
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Performs the post-processing of the CP solution and post-processing
-- parameters.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.PostProcessor
  ( DataDepGraph
  , mkDataDepGraph
  )
where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Patterns (PatternInstanceID)
import qualified Data.Graph.Inductive as I



--------------
-- Data types
--------------

-- | A data type representing a graph where the nodes represent pattern
-- instances, and the directed edges represent data dependencies between the
-- pattern instances.

type DataDepGraph = I.Gr PatternInstanceID ()



-------------
-- Functions
-------------

-- | Takes a list of pattern instance data and pattern instance IDs, and
-- produces a data dependency graph such that every pattern instance ID is
-- represented by a node, and there is a directed edge between two nodes if the
-- pattern instance indicated by the target node uses data produced by the
-- pattern instance indicated by the source node.

mkDataDepGraph :: [PatternInstanceData]
                  -> [PatternInstanceID]
                  -> DataDepGraph
-- TODO: implement
mkDataDepGraph _ _ = I.empty