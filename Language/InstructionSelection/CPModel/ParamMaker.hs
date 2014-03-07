--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.ParamMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructs the parameters which are used to create the CP model. These
-- include the program input data, such as the IDs of the various nodes, and the
-- pattern data, which also contain the match sets.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ParamMaker (
  mkParams
) where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Utils ( Natural
                                           , removeDuplicates
                                           , toNatural
                                           )



-------------
-- Functions
-------------

mkParams :: OpStructure -- ^ The program function.
            -> [(OpStructure, [NodeMatchset], Natural)] -- ^ The patterns.
            -> CPModelParams
mkParams func pats =
  CPModelParams (mkProgramGraphData func)
                (map mkPatternGraphData pats)
                -- TODO: fix building of machine data
               MachineData

mkNodePartition :: OpStructure -> NodePartition
mkNodePartition os =
  let g = osGraph os
  in NodePartition (getUniqueNodeIdsByType g isComputationNode)
                   (getUniqueNodeIdsByType g isControlNode)
                   (getUniqueNodeIdsByType g isDataNode)
                   (getUniqueNodeIdsByType g isLabelNode)
                   (getUniqueNodeIdsByType g isPhiNode)
                   (getUniqueNodeIdsByType g isStateNode)
                   (getUniqueNodeIdsByType g isTransferNode)

getUniqueNodeIdsByType :: Graph -> (Node -> Bool) -> [NodeId]
getUniqueNodeIdsByType g f =
  removeDuplicates $ map (nodeId) $ filter (f) $ allNodes g

-- | Computes the dominator relations between the label nodes after all other
-- nodes have been removed, which yields the dominator relations between the
-- basic blocks.

computeLabelDoms :: OpStructure -> [( NodeId   -- ^ The dominated node.
                                    , [NodeId] -- ^ The dominator set.
                                    )]
computeLabelDoms os =
  let g = osGraph os
      nodes_to_remove = filter (\n -> not (isLabelNode n || isControlNode n))
                        $ allNodes g
      cfg_with_ctrl_nodes = foldl (flip delNode) g nodes_to_remove
      cfg = foldl delNodeKeepEdges g (filter isControlNode $ allNodes g)
  in []

mkProgramGraphData :: OpStructure -> ProgramGraphData
mkProgramGraphData os =
  ProgramGraphData (mkNodePartition os)
                   (computeLabelDoms os)
                   (osConstraints os)

mkPatternGraphData :: (OpStructure, [NodeMatchset], Natural) -> PatternGraphData
mkPatternGraphData (os, matches, id) =
  let g = osGraph os
  in PatternGraphData (mkNodePartition os)
                      (mkUseDefs g isDataNode)
                      (mkUseDefs g isLabelNode)
                      (mkUseDefs g isStateNode)
                      (osConstraints os)
                      (map convertMatchsetNToId matches)

mkUseDefs :: Graph -> (Node -> Bool) -> UseDefNodes
mkUseDefs g f =
  let nodes = filter f (allNodes g)
      use_nodes = filter (\n -> length (successors g n) > 0) nodes
      def_nodes = filter (\n -> length (predecessors g n) > 0) nodes
  in UseDefNodes (map nodeId use_nodes) (map nodeId def_nodes)

-- | Deletes a node from the graph, and redirects any edges involving the given
-- node such that all outbound edges will become outbound edges of the node's
-- parent. It is assumed the graph has at most one predecessor of the node to
-- remove (if there are more than one predecessor then the edges will be
-- redirected to one of them, but it is undefined which).

delNodeKeepEdges :: Graph -> Node -> Graph
delNodeKeepEdges g n =
  let preds = predecessors g n
  in if length preds > 0
        then mergeNodes (head preds) n g
        else delNode n g
