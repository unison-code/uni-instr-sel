{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Graphs.GraphViz
  ( noMoreEdgeAttr
  , noMoreNodeAttr
  , showEdgeNrsAttr
  , toDotGraph
  , toDotGraphWith
  , toDotString
  , toDotStringWith
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.DataTypes
  ( isTypeAConstValue )
import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.IDs
  ( NodeID )

import qualified Data.Graph.Inductive as I
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVA
import qualified Data.GraphViz.Printing as GVP
import qualified Data.Text.Lazy as T

import Data.Maybe
  ( isJust
  , fromJust
  )



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
  :: (Graph -> Node -> GV.Attributes)
  -> (Graph -> Edge -> GV.Attributes)
  -> Graph
  -> GV.DotGraph I.Node
toDotGraphWith nf ef g =
  GV.graphToDot (mkParams g nf ef) (intGraph g)

-- | Converts a graph into GraphViz's DotGraph string format, which can then be
-- written to file.
toDotString :: Graph -> String
toDotString = toDotStringWith noMoreNodeAttr noMoreEdgeAttr

-- | Same as 'toDotString' but may take two additional functions for customizing
-- the appearance of the nodes and edges. This is done by adding additional
-- attributes to the nodes and edges.
toDotStringWith
  :: (Graph -> Node -> GV.Attributes)
  -> (Graph -> Edge -> GV.Attributes)
  -> Graph
  -> String
toDotStringWith nf ef g =
  let text = GVP.renderDot $ GVP.toDot $ toDotGraphWith nf ef g
      str = T.unpack text
  in str

-- | Constructs the dot graph parameters, including the attributes for the nodes
-- and edges.
mkParams
  :: Graph
  -> (Graph -> Node -> GV.Attributes)
  -> (Graph -> Edge -> GV.Attributes)
  -> GV.GraphvizParams I.Node NodeLabel EdgeLabel () NodeLabel
mkParams g nf ef =
  let mkNodeAttr n = mkDefaultNodeAttr g n ++ nf g (Node n)
      mkEdgeAttr e = mkDefaultEdgeAttr g e ++ ef g (Edge e)
  in GV.nonClusteredParams
       { GV.fmtNode = mkNodeAttr
       , GV.fmtEdge = mkEdgeAttr
       }

-- | Constructs the default node attributes, depending on the node type.
mkDefaultNodeAttr :: Graph -> (I.LNode NodeLabel) -> GV.Attributes
mkDefaultNodeAttr g i_n =
  let n = Node i_n
      nt = getNodeType n
      nid = getNodeID n
  in mkNodeShapeAttr g n nt ++ mkNodeLabelAttr nid nt

-- | Creates attribute for drawing thick lines.
thickWidthAttr :: GV.Attribute
thickWidthAttr = GV.penWidth 3.0

-- | Constructs the node shape attributes based on the given node type.
mkNodeShapeAttr :: Graph -> Node -> NodeType -> GV.Attributes
mkNodeShapeAttr _ _ (ComputationNode _) = [GV.shape GV.Circle]
mkNodeShapeAttr _ _ (ControlNode _) = [GV.shape GV.DiamondShape, thickWidthAttr]
mkNodeShapeAttr _ _ (CallNode _) = [GV.shape GV.Circle]
mkNodeShapeAttr _ _ IndirCallNode = [GV.shape GV.Circle]
mkNodeShapeAttr _ _ (ValueNode _ _) = [GV.shape GV.BoxShape]
mkNodeShapeAttr g n (BlockNode _) =
  let attr = [GV.shape GV.BoxShape, thickWidthAttr]
      entry = entryBlockNode g
  in if isJust entry && fromJust entry == n
     then attr ++ [GVA.Peripheries 2]
     else attr
mkNodeShapeAttr _ _ PhiNode = [GV.shape GV.Circle]
mkNodeShapeAttr _ _ StateNode = [GV.shape GV.BoxShape]
mkNodeShapeAttr _ _ CopyNode = [GV.shape GV.Circle]

-- | Constructs a label attribute based on the node ID and node type.
mkNodeLabelAttr :: NodeID -> NodeType -> GV.Attributes
mkNodeLabelAttr nid nt =
  [GV.toLabel s]
  where s = let pre = f nt
            in pre ++ (if length pre > 0 then "\n" else "") ++ pShow nid
        f (ComputationNode op) = pShow op
        f (ControlNode op) = pShow op
        f (CallNode func) = "call " ++ pShow func
        f IndirCallNode = "indir-call"
        f (ValueNode dt origin) = pShow dt
                               ++
                               ( if not (isTypeAConstValue dt)
                                 then if length origin > 0
                                      then " " ++
                                           if length origin == 1
                                           then head origin
                                           else show origin
                                      else ""
                                 else ""
                               )
        f (BlockNode l) = pShow l
        f PhiNode = "phi"
        f StateNode = ""
        f CopyNode = "cp"

-- | A function that produces an empty list of attributes, no matter the
-- argument.
noMoreNodeAttr :: Graph -> Node -> GV.Attributes
noMoreNodeAttr _ _ = []

-- | Constructs the default edge attributes.
mkDefaultEdgeAttr :: Graph -> (I.LEdge EdgeLabel) -> GV.Attributes
mkDefaultEdgeAttr _ e = mkEdgeAttrByType (getEdgeType (Edge e))
                        ++
                        mkEdgeNrAttributes e

-- | Constructs attributes for the edge numbers.
mkEdgeNrAttributes :: (I.LEdge EdgeLabel) -> GV.Attributes
mkEdgeNrAttributes _ =
  [ GVA.LabelDistance 2.0 ]

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
mkStateFlowEdgeAttr = [GV.style GV.dashed]

mkDefEdgeAttr :: GV.Attributes
mkDefEdgeAttr = [GV.style GV.dotted]

-- | A function that produces an empty list of attributes, no matter the
-- argument.
noMoreEdgeAttr :: Graph -> Edge -> GV.Attributes
noMoreEdgeAttr _ _ = []

-- | Returns a function that constructs attributes for showing an edge's edge
-- numbers.
showEdgeNrsAttr :: Graph -> Edge -> GV.Attributes
showEdgeNrsAttr =
  ( \_ e ->
    [ GVA.TailLabel (GV.toLabelValue $ pShow $ getEdgeOutNr e)
    , GVA.HeadLabel (GV.toLabelValue $ pShow $ getEdgeInNr e)
    ]
  )
