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
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.GraphViz (
  toDotGraph
) where

import Language.InstructionSelection.Graphs.Base
import qualified Data.Graph.Inductive as I
import qualified Data.GraphViz as GV



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
mkNodeAttr (_, NodeLabel _ (NodeInfo nt _)) = mkNodeAppearanceAttr nt

-- | Constructs the node appearance attributes.

mkNodeAppearanceAttr :: NodeType -> GV.Attributes
mkNodeAppearanceAttr (ComputationNode _) = [GV.shape GV.Circle]
mkNodeAppearanceAttr (ControlNode _) = [GV.shape GV.InvHouse]
mkNodeAppearanceAttr (DataNode _) = [GV.shape GV.BoxShape]
mkNodeAppearanceAttr (LabelNode _) = [GV.shape GV.BoxShape]
mkNodeAppearanceAttr PhiNode = [GV.shape GV.Circle]
mkNodeAppearanceAttr StateNode = [GV.shape GV.BoxShape]
mkNodeAppearanceAttr TransferNode = [GV.shape GV.DiamondShape]
mkNodeAppearanceAttr NullNode = [GV.shape GV.Pentagon]
