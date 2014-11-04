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
-- Provides a set of transformation function which can be applied on a given
-- graph.
--------------------------------------------------------------------------------

module Language.InstSel.Graphs.Transformations
  ( extendWithCopies )
where

import Language.InstSel.Graphs.Base



-------------
-- Functions
-------------

-- | Inserts a copy node along every data flow edge that involves a use of a
-- data node. Note that the definition placement edges will not be updated!
extendWithCopies :: Graph -> Graph
extendWithCopies g =
  let d_nodes = filter isDataNode (getAllNodes g)
      df_out_edges_from_d_nodes = concatMap
                                  (filter isDataFlowEdge $ getOutEdges g)
                                  d_nodes
      new_g = foldl insertCopy g df_out_edges_from_d_nodes
  in new_g

-- | Inserts a copy and data node along a given edge.
insertCopy :: Graph -> Edge -> Graph
insertCopy g0 e =
  let orig_src_n = getSourceNode g0 e
      orig_dst_n = getTargetNode g0 e
      (g1, new_d_node) = insertNewNodeAlongEdge CopyNode e g0
      new_e = head $ getOutEdges g1 new_d_node
      (g2, _) = insertNewNodeAlongEdge (DataNode D.AnyType Nothing) new_e g1
  in g2
