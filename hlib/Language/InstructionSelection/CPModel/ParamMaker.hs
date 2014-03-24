--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.ParamMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructs the parameters which are used to create the CP model. These
-- include the function data, such as the IDs of the various nodes, and the
-- pattern data, which also contain the matchsets.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ParamMaker (
  mkParams
) where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns ( InstanceId
                                              , InstProperties (..)
                                              )



-------------
-- Functions
-------------

-- | Takes a function and a list of pattern instance data and transforms it into
-- a CP model parameters object.

mkParams :: OpStructure -- ^ Function data.
            -> [( OpStructure
                , [(Matchset Node, InstanceId)]
                , InstProperties
                )] -- ^ Pattern instance data.
            -> CPModelParams
mkParams f ps =
  CPModelParams (mkFunctionGraphData f)
                (concatMap mkPatternInstanceData ps)
                -- TODO: fix building of machine data
                MachineData

mkFunctionGraphData :: OpStructure -> FunctionGraphData
mkFunctionGraphData os =
  let g = osGraph os
      nodeIdsByType f = nodeIds $ filter f $ allNodes g
  in FunctionGraphData (nodeIdsByType isActionNode)
                       (nodeIdsByType isEntityNode)
                       (computeLabelDoms g)
                       (osConstraints os)

mkPatternInstanceData :: ( OpStructure
                         , [(Matchset Node, InstanceId)]
                         , InstProperties
                         )
                      -> [PatternInstanceData]
mkPatternInstanceData (os, matchsets, props) =
  let g = osGraph os
      a_ns = (nodeIds $ filter isActionNode $ allNodes g)
      entityNodes p = nodeIds
                      $ filter (\n -> length (p g n) > 0)
                      $ filter isEntityNode
                      $ allNodes g
      e_def_ns = entityNodes predecessors
      e_use_ns = entityNodes successors
      f (m, id) = mkPatternInstanceData' a_ns
                                         e_def_ns
                                         e_use_ns
                                         (osConstraints os)
                                         (convertMatchsetNToId m)
                                         id
                                         props
  in map f matchsets

mkPatternInstanceData' :: [NodeId]    -- ^ Action nodes in the pattern.
                          -> [NodeId] -- ^ Defined entity nodes.
                          -> [NodeId] -- ^ Used entity nodes.
                          -> [Constraint]
                          -> Matchset NodeId
                          -> InstanceId
                          -> InstProperties
                          -> PatternInstanceData
mkPatternInstanceData' a_ns e_def_ns e_use_ns cs matchset id props =
  PatternInstanceData id
                      (mappedNodesPToF matchset a_ns)
                      (mappedNodesPToF matchset e_def_ns)
                      (mappedNodesPToF matchset e_use_ns)
                      -- TODO: convert node IDs in constraints
                      cs
                      -- TODO: change how the cost is chosen
                      (latency props)

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

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.

computeLabelDoms g =
  let nodes_to_remove = filter (\n -> not (isLabelNode n || isControlNode n))
                        $ allNodes g
      cfg_with_ctrl_nodes = foldl (flip delNode) g nodes_to_remove
      cfg = foldl delNodeKeepEdges
                  cfg_with_ctrl_nodes
                  (filter isControlNode $ allNodes cfg_with_ctrl_nodes)
      root = head $ filter (\n -> length (predecessors g n) == 0) (allNodes cfg)
      node_domsets = dom cfg root
      node_id_domsets = map (\(n, ns) -> (nodeId n, map nodeId ns)) node_domsets
  in node_id_domsets
