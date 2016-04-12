--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs.GraphViz
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts a graph into a GraphViz graph of DotGraph format.
--------------------------------------------------------------------------------

module Language.InstrSel.Graphs.GraphViz
  ( noMoreEdgeAttr
  , noMoreNodeAttr
  , toDotGraph
  , toDotGraphWith
  , toDotString
  , toDotStringWith
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.IDs
  ( NodeID )

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
mkDefaultNodeAttr i_n =
  let n = Node i_n
      nt = getNodeType n
      nid = getNodeID n
  in mkNodeShapeAttr nt ++ mkNodeLabelAttr nid nt

-- | Creates attribute for drawing thick lines.
thickWidthAttr :: GV.Attribute
thickWidthAttr = GV.penWidth 3.0

-- | Constructs the node shape attributes based on the given node type.
mkNodeShapeAttr :: NodeType -> GV.Attributes
mkNodeShapeAttr (ComputationNode _) = [GV.shape GV.Circle]
mkNodeShapeAttr (ControlNode _) = [GV.shape GV.DiamondShape, thickWidthAttr]
mkNodeShapeAttr (CallNode _) = [GV.shape GV.Circle]
mkNodeShapeAttr (ValueNode _ _) = [GV.shape GV.BoxShape]
mkNodeShapeAttr (BlockNode _) = [GV.shape GV.BoxShape, thickWidthAttr]
mkNodeShapeAttr PhiNode = [GV.shape GV.Circle]
mkNodeShapeAttr StateNode = [GV.shape GV.BoxShape]
mkNodeShapeAttr CopyNode = [GV.shape GV.Circle]

-- | Constructs a label attribute based on the node ID and node type.
mkNodeLabelAttr :: NodeID -> NodeType -> GV.Attributes
mkNodeLabelAttr nid nt =
  [GV.toLabel s]
  where s = f nt ++ "\n" ++ pShow nid
        f (ComputationNode op) = pShow op
        f (ControlNode op) = pShow op
        f (CallNode func) = "call " ++ pShow func
        f (ValueNode dt src) = pShow dt ++ maybe "" (" " ++) src
        f (BlockNode l) = pShow l
        f PhiNode = "phi"
        f StateNode = ""
        f CopyNode = "cp"

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
  [ GVA.TailLabel (GV.toLabelValue $ pShow $ getOutEdgeNr (Edge e))
  , GVA.HeadLabel (GV.toLabelValue $ pShow $ getInEdgeNr (Edge e))
  , GVA.LabelDistance 2.0
  ]

-- | Constructs the edge attributes based on the given edge type.
mkEdgeAttrByType :: EdgeType -> GV.Attributes
mkEdgeAttrByType ControlFlowEdge = mkControlFlowEdgeAttr
mkEdgeAttrByType DataFlowEdge = mkDataFlowEdgeAttr
mkEdgeAttrByType StateFlowEdge = mkStateFlowEdgeAttr
mkEdgeAttrByType DefEdge = mkDefEdgeAttr

mkControlFlowEdgeAttr :: GV.Attributes
mkControlFlowEdgeAttr = [GV.style GV.solid, thickWidthAttr]

mkDataFlowEdgeAttr :: GV.Attributes
mkDataFlowEdgeAttr = [GV.style GV.solid]

mkStateFlowEdgeAttr :: GV.Attributes
mkStateFlowEdgeAttr = [GV.style GV.solid]

mkDefEdgeAttr :: GV.Attributes
mkDefEdgeAttr = [GV.style GV.dotted]

-- | A function that produces an empty list of attributes, no matter the
-- argument.
noMoreEdgeAttr :: Edge -> GV.Attributes
noMoreEdgeAttr _ = []
