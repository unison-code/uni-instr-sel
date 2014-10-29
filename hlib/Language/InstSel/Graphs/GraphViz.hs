--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.GraphViz
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts a graph into a GraphViz graph of DotGraph format.
--------------------------------------------------------------------------------

module Language.InstSel.Graphs.GraphViz
  ( toDotGraph )
where

import Language.InstSel.Graphs.Base
import Language.InstSel.PrettyPrint
import qualified Data.Graph.Inductive as I
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import Data.Maybe



-------------
-- Functions
-------------

-- | Converts a graph into GraphViz's DotGraph format.
toDotGraph :: Graph -> GV.DotGraph I.Node
toDotGraph g = GV.graphToDot mkParams (intGraph g)

-- | Constructs the appropriate parameters.
mkParams :: GV.GraphvizParams I.Node NodeLabel EdgeLabel () NodeLabel
mkParams =
  GV.nonClusteredParams
  { GV.fmtNode = mkNodeAttr
  , GV.fmtEdge = mkEdgeAttr
  }

-- | Constructs the appropriate node attributes, depending on the node type.
mkNodeAttr :: (I.LNode NodeLabel) -> GV.Attributes
mkNodeAttr (_, NodeLabel _ nt) = mkNodeTypeAttr nt

-- | Constructs the node attributes based on its node type.
mkNodeTypeAttr :: NodeType -> GV.Attributes
mkNodeTypeAttr (ComputationNode op) =
  [GV.shape GV.Circle, GV.toLabel (prettyShow op)]
mkNodeTypeAttr (TypeConversionNode op) =
  [GV.shape GV.Circle, GV.toLabel (prettyShow op)]
mkNodeTypeAttr (ControlNode op) =
  [GV.shape GV.InvHouse, GV.toLabel (prettyShow op)]
mkNodeTypeAttr (DataNode _ src) =
  let shape_attr = GV.shape GV.BoxShape
  in if isJust src
        then [shape_attr, GV.toLabel $ fromJust src]
        else [shape_attr, GV.toLabel ""]
mkNodeTypeAttr (LabelNode (BBLabelID l)) =
  [GV.shape GV.BoxShape, GV.penWidth 3.0, GV.toLabel l]
mkNodeTypeAttr PhiNode = [GV.shape GV.Circle, GV.toLabel "phi"]
mkNodeTypeAttr StateNode = [GV.shape GV.BoxShape, GV.toLabel ""]
mkNodeTypeAttr CopyNode = [GV.shape GV.DiamondShape, GV.toLabel ""]
mkNodeTypeAttr NullNode =
  [GV.shape GV.Circle, GV.style GV.dashed, GV.toLabel ""]

-- | Constructs the appropriate edge attributes.
mkEdgeAttr :: (I.LEdge EdgeLabel) -> GV.Attributes
mkEdgeAttr (_, _, EdgeLabel out_nr in_nr) =
  [ GVA.TailLabel (GV.toLabelValue $ show out_nr)
  , GVA.HeadLabel (GV.toLabelValue $ show in_nr)
  , GVA.LabelDistance 2.0
  ]
