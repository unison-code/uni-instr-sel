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



-------------
-- Functions
-------------

-- | Converts a graph into GraphViz's DotGraph format.
toDotGraph :: Graph -> GV.DotGraph I.Node
toDotGraph g = GV.graphToDot mkParams (intGraph g)

-- | Constructs the appropriate parameters.
mkParams :: GV.GraphvizParams I.Node NodeLabel EdgeLabel () NodeLabel
mkParams =
  GV.nonClusteredParams { GV.fmtNode = mkNodeAttr
                        , GV.fmtEdge = mkEdgeAttr
                        }

-- | Constructs the appropriate node attributes, depending on the node type.
mkNodeAttr :: (I.LNode NodeLabel) -> GV.Attributes
mkNodeAttr n = mkNodeAttrByType (getNodeType (Node n))

-- | Constructs the node attributes based on the given node type.
mkNodeAttrByType :: NodeType -> GV.Attributes
mkNodeAttrByType (ComputationNode op) = mkCompNodeAttr (prettyShow op)
mkNodeAttrByType (ControlNode op) = mkControlNodeAttr (prettyShow op)
mkNodeAttrByType (DataNode _ src) = mkDataNodeAttr (maybe "" id src)
mkNodeAttrByType (LabelNode (BBLabelID l)) = mkLabelNodeAttr l
mkNodeAttrByType PhiNode = mkPhiNodeAttr
mkNodeAttrByType StateNode = mkStateNodeAttr
mkNodeAttrByType CopyNode = mkCopyNodeAttr

mkCompNodeAttr :: String -> GV.Attributes
mkCompNodeAttr s = [GV.shape GV.Circle, GV.toLabel s]

mkControlNodeAttr :: String -> GV.Attributes
mkControlNodeAttr s = [GV.shape GV.InvHouse, GV.toLabel s]

mkDataNodeAttr :: String -> GV.Attributes
mkDataNodeAttr s = [GV.shape GV.BoxShape, GV.toLabel s]

mkLabelNodeAttr :: String -> GV.Attributes
mkLabelNodeAttr s = [GV.shape GV.BoxShape, GV.penWidth 3.0, GV.toLabel s]

mkPhiNodeAttr :: GV.Attributes
mkPhiNodeAttr = mkCompNodeAttr "phi"

mkStateNodeAttr :: GV.Attributes
mkStateNodeAttr = mkDataNodeAttr ""

mkCopyNodeAttr :: GV.Attributes
mkCopyNodeAttr = mkCompNodeAttr "copy"

-- | Constructs the appropriate edge attributes.
mkEdgeAttr :: (I.LEdge EdgeLabel) -> GV.Attributes
mkEdgeAttr e = mkEdgeAttrByType (getEdgeType (Edge e))
               ++
               mkEdgeNrAttributes e

-- | Constructs attributes for the edge numbers.
mkEdgeNrAttributes :: (I.LEdge EdgeLabel) -> GV.Attributes
mkEdgeNrAttributes e =
  [ GVA.TailLabel (GV.toLabelValue $ show $ getOutEdgeNr (Edge e))
  , GVA.HeadLabel (GV.toLabelValue $ show $ getInEdgeNr (Edge e))
  , GVA.LabelDistance 2.0
  ]

-- | Constructs the edge attributes based on the given edge type.
mkEdgeAttrByType :: EdgeType -> GV.Attributes
mkEdgeAttrByType ControlFlowEdge = mkControlFlowEdgeAttr
mkEdgeAttrByType DataFlowEdge = mkDataFlowEdgeAttr
mkEdgeAttrByType StateFlowEdge = mkStateFlowEdgeAttr
mkEdgeAttrByType DefPlaceEdge = mkDefPlaceEdgeAttr

mkControlFlowEdgeAttr :: GV.Attributes
mkControlFlowEdgeAttr = [GV.style GV.dashed]

mkDataFlowEdgeAttr :: GV.Attributes
mkDataFlowEdgeAttr = [GV.style GV.solid]

mkStateFlowEdgeAttr :: GV.Attributes
mkStateFlowEdgeAttr = [GV.style GV.solid]

mkDefPlaceEdgeAttr :: GV.Attributes
mkDefPlaceEdgeAttr = [GV.style GV.dotted]
