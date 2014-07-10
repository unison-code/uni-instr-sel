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
  , emitInstructions
  , mkDataDepDAG
  )
where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
  (NodeID)
import Language.InstructionSelection.Patterns
  ( Instruction
  , PatternInstanceID
  )
import qualified Data.Graph.Inductive as I
import Data.Maybe



--------------
-- Data types
--------------

-- | A data type representing a DAG where the nodes represent pattern instances,
-- and the directed edges represent data dependencies between the pattern
-- instances. Each edge is labeled with the node ID of the data or state node
-- which represents the data involved in that edge.

type DataDepDAG = I.Gr PatternInstanceID NodeID



-------------
-- Functions
-------------

-- | Takes a list of pattern instance data and pattern instance IDs, and
-- produces a data dependency DAG such that every pattern instance ID is
-- represented by a node, and there is a directed edge between two nodes if the
-- pattern instance indicated by the target node uses data produced by the
-- pattern instance indicated by the source node. Cyclic dependencies be broken
-- such that the pattern containing the phi node which makes use of the data
-- appears at the top of the DAG.

mkDataDepDAG :: [PatternInstanceData]
                -> [PatternInstanceID]
                -> DataDepDAG
mkDataDepDAG ds is =
  foldr (addUseEdgesToDAG ds) (I.mkGraph (zip [0..] is) []) is

-- | Adds an edge for each use of data or state of the given pattern instance
-- ID. If the source node is not present in the graph, no edge is added. It is
-- assumed that there always exists exactly one node in the graph representing
-- the pattern instance ID given as argument to the function. Note that this may
-- lead to cycles, which will have to be broken as a second step.

addUseEdgesToDAG :: [PatternInstanceData]
                    -> PatternInstanceID
                    -> DataDepDAG
                    -> DataDepDAG
addUseEdgesToDAG ds pid g0 =
  let pi_n = fromJust $ getNodeOfPI g0 pid
      pi_data = getPIData ds pid
      ns = I.labNodes g0
      d_uses_of_pi = filter (`notElem` patDataNodesUsedByPhis pi_data)
                     $ patDataNodesUsed pi_data
      s_uses_of_pi = patStateNodesUsed pi_data
      ns_d_defs = map (\(n, i) -> (n, patDataNodesDefined $ getPIData ds i)) ns
      ns_s_defs = map (\(n, i) -> (n, patStateNodesDefined $ getPIData ds i)) ns
      g1 = foldr (addUseEdgesToDAG' pi_n ns_d_defs) g0 d_uses_of_pi
      g2 = foldr (addUseEdgesToDAG' pi_n ns_s_defs) g1 s_uses_of_pi
  in g2

addUseEdgesToDAG' :: I.Node
                     -> [(I.Node, [NodeID])] -- ^ List of defs.
                     -> NodeID               -- ^ A use.
                     -> DataDepDAG
                     -> DataDepDAG
addUseEdgesToDAG' n def_maps use g =
  let ns = map fst $ filter (\m -> use `elem` snd m) def_maps
  in foldr (\n' g' -> I.insEdge (n', n, use) g') g ns

-- | Gets the internal node ID (if any) of the node with a given pattern
-- instance ID as its label. It is assumed that there is always at most one such
-- node in the graph.

getNodeOfPI :: DataDepDAG
               -> PatternInstanceID
               -> Maybe I.Node
getNodeOfPI g pid =
  let ns = filter (\n -> snd n == pid) $ I.labNodes g
  in if length ns > 0
        then Just (fst $ head ns)
        else Nothing

-- | Retrieves the 'PatternInstanceData' entity with matching pattern instance
-- ID. It is assumed that such an entity always exists in the given list.

getPIData :: [PatternInstanceData]
             -> PatternInstanceID
             -> PatternInstanceData
getPIData ds pid = head $ filter (\d -> patInstanceID d == pid) ds
