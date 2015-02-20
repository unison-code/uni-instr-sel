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
  ( mkEmptyBBLabel )
import qualified Language.InstSel.OpTypes as O

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Inserts a copy node along every data flow edge that involves a use of a
-- data node and passes the predicate function. This also updates the dominance
-- edges to retain the same semantics of the original graph. This means that if
-- there is a dominance edge $e$ that involves a data node used by a phi node,
-- then upon copy extension $e$ will be moved to the new data node. Otherwise
-- $e$ will remain on the original data node. Note that postdominance edges are
-- not affected.
--
-- The new data nodes will not have any origin, and will be of any data type.
copyExtendWhen
  :: (Graph -> Edge -> Bool)
     -- ^ The predicate function, which checks whether to copy-extend the given
     -- edge.
  -> Graph
     -- ^ The graph to extend.
 -> Graph
copyExtendWhen f g =
  let nodes = filter isDataNode (getAllNodes g)
      edges = concatMap (getDtFlowOutEdges g) nodes
      filtered_edges = filter (f g) edges
  in foldl insertCopy g filtered_edges

-- | Inserts a new copy and data node along a given data flow edge. If the data
-- node is used by a phi node, and there is a dominance edge on that data node,
-- then the dominance edge with matching out-edge number will be moved to the
-- new data node. Note that postdominance edges are not affected.
insertCopy :: Graph -> Edge -> Graph
insertCopy g0 df_edge =
  let orig_d_node = getSourceNode g0 df_edge
      orig_op_n = getTargetNode g0 df_edge
      dom_edge = if isPhiNode orig_op_n
                 then let d_node_edges = getOutEdges g0 orig_d_node
                          dom_edges = filter isDomEdge d_node_edges
                      in Just
                         $ head
                         $ filter (\n -> getOutEdgeNr n == getOutEdgeNr df_edge)
                                  dom_edges
                 else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      (g2, new_d_node) =
        insertNewNodeAlongEdge (DataNode D.AnyType Nothing)
                               (head $ getOutEdges g1 new_cp_node)
                               g1
      g3 = if isJust dom_edge
           then let e = fromJust dom_edge
                in fst $ addNewDomEdge (new_d_node, getTargetNode g2 e)
                                       (delEdge e g2)
           else g2
  in g3

-- | Inserts a new label node and jump control node along each outbound control
-- edge from every conditional jump control node and passes the predicate
-- function.
--
-- The new label nodes will all have empty basic block labels.
branchExtendWhen
  :: (Graph -> Edge -> Bool)
     -- ^ The predicate function, which checks whether to branch-extend the
     -- given edge.
  -> Graph
     -- ^ The graph to extend.
 -> Graph
branchExtendWhen f g =
  let c_nodes = filter isControlNode (getAllNodes g)
      nodes = filter (\n -> (ctrlOp $ getNodeType n) == O.CondBr)
                     c_nodes
      edges = concatMap (getCtrlFlowOutEdges g) nodes
      filtered_edges = filter (f g) edges
  in foldl insertBranch g filtered_edges

-- | Inserts a new label node and jump control node along the given control flow
-- edge.
insertBranch :: Graph -> Edge -> Graph
insertBranch g0 e =
  let (g1, l) = insertNewNodeAlongEdge (LabelNode mkEmptyBBLabel) e g0
      new_e = head $ getCtrlFlowOutEdges g1 l
      (g2, _) = insertNewNodeAlongEdge (ControlNode O.Br) new_e g1
  in g2
