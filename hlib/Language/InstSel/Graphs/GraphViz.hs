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
  ( noMoreEdgeAttr
  , noMoreNodeAttr
  , toDotGraph
  , toDotGraphWith
  , toDotString
  , toDotStringWith
  )
where

import Language.InstSel.DebugShow
import Language.InstSel.Graphs.Base

import qualified Data.Graph.Inductive as I
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.GraphViz.Printing as GVP
import qualified Data.Text.Lazy as T



-------------
-- Functions
-------------

-- | Converts a graph into GraphViz's DotGraph internal format.
toDotGraph :: Graph -> GV.DotGraph I.Node
toDotGraph = toDotGraphWith noMoreNodeAttr noMoreEdgeAttr

-- | Same as 'toDotGraph' but may take two additional functions for customizing
-- the appearance of the nodes and edges. This is done by adding additional
-- attributes to the nodes and edges.
toDotGraphWith
  :: (Node -> GV.Attributes)
  -> (Edge -> GV.Attributes)
  -> Graph
  -> GV.DotGraph I.Node
toDotGraphWith nf ef g =
  GV.graphToDot (mkParams nf ef) (intGraph g)

-- | Converts a graph into GraphViz's DotGraph string format, which can then be
-- written to file.
toDotString :: Graph -> String
toDotString = toDotStringWith noMoreNodeAttr noMoreEdgeAttr

-- | Same as 'toDotString' but may take two additional functions for customizing
-- the appearance of the nodes and edges. This is done by adding additional
-- attributes to the nodes and edges.
toDotStringWith
  :: (Node -> GV.Attributes)
  -> (Edge -> GV.Attributes)
  -> Graph
  -> String
toDotStringWith nf ef g =
  let text = GVP.renderDot $ GVP.toDot $ toDotGraphWith nf ef g
      str = T.unpack text
  in str

-- | Constructs the dot graph parameters, including the attributes for the nodes
-- and edges.
mkParams
  :: (Node -> GV.Attributes)
  -> (Edge -> GV.Attributes)
  -> GV.GraphvizParams I.Node NodeLabel EdgeLabel () NodeLabel
mkParams nf ef =
  let mkNodeAttr n = mkDefaultNodeAttr n ++ nf (Node n)
      mkEdgeAttr e = mkDefaultEdgeAttr e ++ ef (Edge e)
  in GV.nonClusteredParams
       { GV.fmtNode = mkNodeAttr
       , GV.fmtEdge = mkEdgeAttr
       }

-- | Constructs the default node attributes, depending on the node type.
mkDefaultNodeAttr :: (I.LNode NodeLabel) -> GV.Attributes
mkDefaultNodeAttr n = mkNodeAttrByType (getNodeType (Node n))

-- | Constructs the node attributes based on the given node type.
mkNodeAttrByType :: NodeType -> GV.Attributes
mkNodeAttrByType (ComputationNode op) = mkCompNodeAttr (dShow op)
mkNodeAttrByType (ControlNode op) = mkControlNodeAttr (dShow op)
mkNodeAttrByType (DataNode _ src) = mkDataNodeAttr (maybe "" id src)
mkNodeAttrByType (LabelNode (BasicBlockLabel l)) = mkLabelNodeAttr l
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

-- | A function that produces an empty list of attributes, no matter the
-- argument.
noMoreNodeAttr :: Node -> GV.Attributes
noMoreNodeAttr _ = []

-- | Constructs the default edge attributes.
mkDefaultEdgeAttr :: (I.LEdge EdgeLabel) -> GV.Attributes
mkDefaultEdgeAttr e = mkEdgeAttrByType (getEdgeType (Edge e))
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

-- | A function that produces an empty list of attributes, no matter the
-- argument.
noMoreEdgeAttr :: Edge -> GV.Attributes
noMoreEdgeAttr _ = []
