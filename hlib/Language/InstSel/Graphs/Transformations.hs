-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a set of transformation functions that can be applied on a given
-- graph.
--------------------------------------------------------------------------------

module Language.InstSel.Graphs.Transformations
  ( branchExtendWhen
  , copyExtendWhen
  )
where

import Language.InstSel.Graphs.Base
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Functions
  ( BasicBlockLabel (..) )
import qualified Language.InstSel.OpTypes as O

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Inserts a copy node along every data flow edge that involves a use of a
-- data node and passes the predicate function. This also updates the definition
-- placement edges to retain the same semantics of the original graph. This
-- means that if there is a definition placement edge $e$ that involves a data
-- node used by a phi node, then upon copy extension $e$ will be moved to the
-- new data node. Otherwise $e$ will remain on the original data node. In cases
-- where the same data node is used by multiple phi nodes, $e$ will be
-- duplicated for each such instance, which are then moved for each copy
-- extension (hence no invariants are violated when returning).
copyExtendWhen
  :: (Graph -> Edge -> Bool)
     -- ^ The predicate function, which checks whether to copy-extend the given
     -- edge.
  -> Graph
     -- ^ The graph to extend.
 -> Graph
copyExtendWhen f g =
  let nodes = filter isDataNode (getAllNodes g)
      edges = concatMap (getDFOutEdges g) nodes
      filtered_edges = filter (f g) edges
  in foldl insertCopy (duplicateDPEdgesForPhis g) filtered_edges

-- | Inserts a new copy and data node along a given data flow edge. If the data
-- node is used by a phi node, and there is a definition placement edge on that
-- data node, then that edge will be moved to the new data node (it is assumed
-- that the data node has as many definition placement edges as it number of
-- uses by phi nodes).
insertCopy :: Graph -> Edge -> Graph
insertCopy g0 df_edge =
  let orig_d_node = getSourceNode g0 df_edge
      orig_op_n = getTargetNode g0 df_edge
      dp_edge = if isPhiNode orig_op_n
                then let d_node_edges = getOutEdges g0 orig_d_node
                         dp_edges = filter isDefPlaceEdge d_node_edges
                     in Just $ head dp_edges
                else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      (g2, new_d_node) = insertNewNodeAlongEdge
                           (DataNode D.AnyType Nothing)
                           (head $ getOutEdges g1 new_cp_node)
                           g1
      g3 = if isJust dp_edge
           then let e = fromJust dp_edge
                in fst $
                     addNewDPEdge
                     (new_d_node, getTargetNode g2 e)
                     (delEdge e g2)
           else g2
  in g3

-- | For each data node $d$ that has a definition placement edge, that edge will
-- be duplicated the number of times $d$ is used by a phi node - 1.
duplicateDPEdgesForPhis :: Graph -> Graph
duplicateDPEdgesForPhis g0 =
  let d_nodes = filter isDataNode (getAllNodes g0)
  in foldl
       ( \g n ->
           let all_out_edges = getOutEdges g n
               dp_edges = filter isDefPlaceEdge all_out_edges
           in if length dp_edges > 0
              then let dp_edge = head dp_edges
                       df_edges = filter isDataFlowEdge all_out_edges
                       num_phis =
                         length $ filter (isPhiNode . getTargetNode g) df_edges
                       src = getSourceNode g dp_edge
                       dst = getTargetNode g dp_edge
                   in (iterate (fst . addNewDPEdge (src, dst)) g)
                      !!
                      max (num_phis - 1) 0
                      -- ^ We use max here as (num_phis - 1) can result in a
                      -- negative number
              else g
       )
       g0
       d_nodes

-- | Inserts a new label node and jump control node along each outbound control
-- edge from every conditional jump control node and passes the predicate
-- function.
branchExtendWhen
  :: (Graph -> Edge -> Bool)
     -- ^ The predicate function, which checks whether to branch-extend the
     -- given edge.
  -> Graph
     -- ^ The graph to extend.
 -> Graph
branchExtendWhen f g =
  let c_nodes = filter isControlNode (getAllNodes g)
      nodes = filter
                    (\n -> (ctrlOp $ getNodeType n) == O.CondBranch)
                    c_nodes
      edges = concatMap (getCFOutEdges g) nodes
      filtered_edges = filter (f g) edges
  in foldl insertBranch g filtered_edges

-- | Inserts a new label node and jump control node along the given control flow
-- edge.
insertBranch :: Graph -> Edge -> Graph
insertBranch g0 e =
  let (g1, l) = insertNewNodeAlongEdge (LabelNode $ BasicBlockLabel "") e g0
      new_e = head $ getCFOutEdges g1 l
      (g2, _) = insertNewNodeAlongEdge (ControlNode O.Branch) new_e g1
  in g2
