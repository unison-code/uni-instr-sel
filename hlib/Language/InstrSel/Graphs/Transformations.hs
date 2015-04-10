-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a set of transformation functions that can be applied on a given
-- graph.
--------------------------------------------------------------------------------

module Language.InstrSel.Graphs.Transformations
  ( branchExtendWhen
  , copyExtendWhen
  )
where

import Language.InstrSel.Graphs.Base
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Functions
  ( mkEmptyBBLabel )
import qualified Language.InstrSel.OpTypes as O

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Inserts a copy node along every data flow edge that involves a use of a
-- value node and passes the predicate function. This also updates the
-- definition edges to retain the same semantics of the original graph. This
-- means that if there is a definition edge $e$ that involves a value node used
-- by a phi node, then upon copy extension $e$ will be moved to the new value
-- node. Otherwise $e$ will remain on the original value node. Note that
-- definition edges where the target is a value node are not affected.
copyExtendWhen
  :: (Graph -> Edge -> Bool)
     -- ^ The predicate function, which checks whether to copy-extend the given
     -- data-flow edge.
  -> (D.DataType -> D.DataType)
     -- ^ Function for creating the data type of the new value node based on the
     -- data type of the original value node (that is, the value node to be
     -- copy-extended).
  -> Graph
     -- ^ The graph to extend.
 -> Graph
copyExtendWhen pf df g =
  let nodes = filter isValueNode (getAllNodes g)
      edges = concatMap (getDtFlowOutEdges g) nodes
      filtered_edges = filter (pf g) edges
  in foldl (insertCopy df) g filtered_edges

-- | Inserts a new copy and value node (whose data type is decided using the
-- provided function) along a given data flow edge. If the value node is used by
-- a phi node, and there is a definition edge on that value node, then the
-- definition edge with matching out-edge number will be moved to the new data
-- node. Note that definition edges where the target is a value node are not
-- affected.
insertCopy :: (D.DataType -> D.DataType) -> Graph -> Edge -> Graph
insertCopy df g0 df_edge =
  let orig_d_node = getSourceNode g0 df_edge
      orig_op_n = getTargetNode g0 df_edge
      def_edge = if isPhiNode orig_op_n
                 then let d_node_edges = getOutEdges g0 orig_d_node
                          def_edges = filter isDefEdge d_node_edges
                      in Just
                         $ head
                         $ filter (\n -> getOutEdgeNr n == getOutEdgeNr df_edge)
                                  def_edges
                 else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      new_dt = df $ getDataTypeOfValueNode orig_d_node
      (g2, new_d_node) =
        insertNewNodeAlongEdge (ValueNode new_dt Nothing)
                               (head $ getOutEdges g1 new_cp_node)
                               g1
      g3 = if isJust def_edge
           then let e = fromJust def_edge
                in fst $ addNewDefEdge (new_d_node, getTargetNode g2 e)
                                       (delEdge e g2)
           else g2
  in g3

-- | Inserts a new block node and jump control node along each outbound control
-- edge from every conditional jump control node and passes the predicate
-- function. The new block nodes will all have empty block name.
--
-- TODO: update the def-placement edges when phi nodes are involved!
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

-- | Inserts a new block node and jump control node along the given control flow
-- edge.
insertBranch :: Graph -> Edge -> Graph
insertBranch g0 e =
  let (g1, l) = insertNewNodeAlongEdge (BlockNode mkEmptyBBLabel) e g0
      new_e = head $ getCtrlFlowOutEdges g1 l
      (g2, _) = insertNewNodeAlongEdge (ControlNode O.Br) new_e g1
  in g2
