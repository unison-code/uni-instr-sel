--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Graphs.GraphViz
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts a graph into a GraphViz graph of DotGraph format.
--
-- TODO: enforce edge ordering
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.GraphViz (
  toDotGraph
) where

import Language.InstructionSelection.Graphs.Base
import Language.InstructionSelection.PrettyPrint
import qualified Data.Graph.Inductive as I
import qualified Data.GraphViz as GV
import Data.Maybe



-------------
-- Functions
-------------

-- | Converts a graph into GraphViz's DotGraph format.

toDotGraph :: Graph -> GV.DotGraph I.Node
toDotGraph g = GV.graphToDot mkParams (intGraph g)

-- | Constructs the appropriate parameters.

mkParams = GV.nonClusteredParams { GV.fmtNode = mkNodeAttr }

-- | Constructs the appropriate node attributes, depending on the node type.

mkNodeAttr :: (I.LNode NodeLabel) -> GV.Attributes
mkNodeAttr (_, NodeLabel _ nt) = mkNodeTypeAttr nt

-- | Constructs the node attributes based on its node type.

mkNodeTypeAttr :: NodeType -> GV.Attributes
mkNodeTypeAttr (ComputationNode op) =
  [GV.shape GV.Circle, GV.toLabel (prettyShow op)]
mkNodeTypeAttr (ControlNode op) =
  [GV.shape GV.InvHouse, GV.toLabel (prettyShow op)]
mkNodeTypeAttr (DataNode _ src) =
  let shape_attr = GV.shape GV.BoxShape
  in if isJust src
        then [shape_attr, GV.toLabel $ fromJust src]
        else [shape_attr]
mkNodeTypeAttr (LabelNode (BBLabel l)) =
  [GV.shape GV.BoxShape, GV.toLabel l]
mkNodeTypeAttr PhiNode = [GV.shape GV.Circle, GV.toLabel "phi"]
mkNodeTypeAttr StateNode = [GV.shape GV.BoxShape]
mkNodeTypeAttr TransferNode = [GV.shape GV.DiamondShape]
mkNodeTypeAttr NullNode = [GV.shape GV.Circle, GV.style GV.dashed]
