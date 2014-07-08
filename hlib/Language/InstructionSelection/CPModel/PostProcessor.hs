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
  ( DataDepDAG
  , mkDataDepDAG
  )
where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Patterns (PatternInstanceID)
import qualified Data.Graph.Inductive as I



--------------
-- Data types
--------------

-- | A data type representing a DAG where the nodes represent pattern instances,
-- and the directed edges represent data dependencies between the pattern
-- instances.

type DataDepDAG = I.Gr PatternInstanceID ()



-------------
-- Functions
-------------

-- | Takes a list of pattern instance data and pattern instance IDs, and
-- produces a data dependency DAG such that every pattern instance ID is
-- represented by a node, and there is a directed edge between two nodes if the
-- pattern instance indicated by the target node uses data produced by the
-- pattern instance indicated by the source node. Potential cyclic dependencies
-- caused by Phi patterns will be broken such that the Phi patterns appear as
-- the originator of the data.

mkDataDepDAG :: [PatternInstanceData]
                -> [PatternInstanceID]
                -> DataDepDAG
-- TODO: implement
mkDataDepDAG _ _ = I.empty