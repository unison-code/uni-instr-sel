--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Graphs.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing graphs which are
-- specialized for representing program functions and patterns.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.Base (
  NodeId
, EdgeId
, BBLabel (..)
, NodeType (..)
, NodeInfo (..)
, NodeLabel (..)
, EdgeLabel (..)
, Graph (..)
, I.Node
, I.LNode
, I.LEdge
, empty
, mkGraph
, addNewNode
, addNewEdge
, newNodeId
, nodes
, nodesByNodeId
, nodeId
, hasSameNodeId
, haveSameNodeIds
, hasSameLabel
, haveSameLabels
, haveSameNodeIdsAndLabels
, replaceNodeLabel
, copyNodeLabel
, mergeNodes
, lastAddedNode
) where

import qualified Data.Graph.Inductive as I
import Language.InstructionSelection.Utils
import Language.InstructionSelection.OpTypes
import Data.Maybe



type NodeId = Natural
type EdgeId = Natural

data BBLabel
    = BBLabel String
    deriving (Show,Eq)

data NodeType
    = NTBinaryOp BinaryOp
    | NTUnaryOp UnaryOp
    | NTMemoryLoad
    | NTMemoryStore
    | NTUncondBranch BBLabel
    | NTCondBranch

          -- | Label taken if the register evaluates to @True@.

          BBLabel

          -- | Label taken if the register evaluates to @False@.

          BBLabel

    | NTPhi

    -- | Both temporary nodes (appearing in IR and pattern code) and register
    -- nodes (appearing only in pattern code) are represented as registers.
    -- For the latter, the specific register is specified as a constraint.

    | NTRegister

    -- | Both constant values (appearing in IR and pattern code) and immediates
    -- (appearing only in pattern code) are represented as constants. For the
    -- former, the specific value is specified as a constraint.

    | NTConstant

    deriving (Show,Eq)

data NodeInfo
    = NodeInfo

          -- | Type of node.

          NodeType

          -- | Label of the basic block that the node belongs to.

          BBLabel

          -- | A field to put arbitrary text in; used when printing the graph.

          String

    deriving (Show, Eq)

data NodeLabel
    = NodeLabel

          -- | Node identifier. Most often this is equal to the 'Node'
          -- identifier used by FGL, but it does not need to be.

          NodeId

          -- | Additional data to describe the node.

          NodeInfo

    deriving (Show, Eq)

-- | Data type for describing how an edge relates to the two nodes. Since edges
-- are ordered, there will be an edge number for each node - one for indicating
-- which output edge it is of the source node, and another for indicating which
-- input edge it is for the destination node.

data EdgeLabel
    = EdgeLabel

          -- | Source edge number.

          EdgeId

          -- | Destination edge number.

          EdgeId

    deriving (Show,Eq)

data Graph
    = Graph (I.Gr NodeLabel EdgeLabel)
    deriving (Show)

-- | Creates an empty graph.

empty = Graph I.empty

-- | Makes a graph.

mkGraph nodes edges = Graph (I.mkGraph nodes edges)

-- | Gets the next internal node ID which does not currently appear in the
-- graph.

nextNodeInt g = 1 + (maximum $ [0] ++ I.nodes g)

-- | Makes a unique node ID not already appearing in the graph.

newNodeId (Graph g) = toNatural $ toInteger $ nextNodeInt g

-- | Gets the node ID from a (Node, NodeLabel) tuple.

nodeId (_, NodeLabel id _) = id

-- | Gets the internal node ID from a (Node, NodeLabel) tuple.

nodeInt (int, _) = int

-- | Checks if a (Node, NodeLabel) tuple has a given node ID.

hasSameNodeId id1 (_, NodeLabel id2 _) = id1 == id2

-- | Checks if two (Node, NodeLabel) tuples have the same node ID.

haveSameNodeIds (_, NodeLabel id1 _) (_, NodeLabel id2 _) =
  id1 == id2

-- | Checks if a (Node, NodeLabel) tuple has a given label.

hasSameLabel label1 (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

-- | Checks if two (Node, NodeLabel) tuples have the same label.

haveSameLabels (_, NodeLabel _ (NodeInfo _ label1 _))
               (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

-- | Checks if two (Node, NodeLabel) tuples have the same node ID and label.

haveSameNodeIdsAndLabels n1 n2 = haveSameNodeIds n1 n2 && haveSameLabels n1 n2

-- | Gets the list of nodes.

nodes (Graph g) = I.labNodes g

-- | Gets a list of nodes with the same node ID.

nodesByNodeId id g = filter (hasSameNodeId id) $ nodes g

-- | Replaces the node label of an already existing node.

replaceNodeLabel :: NodeLabel -> (I.LNode NodeLabel) -> Graph -> Graph
replaceNodeLabel new_label (int, _) (Graph g) =
  Graph (I.insNode (int, new_label) $ I.delNode int g)

-- | Replaces the node info of an already existing node.

replaceNodeInfo :: NodeInfo -> (I.LNode NodeLabel) -> Graph -> Graph
replaceNodeInfo new_info (int, NodeLabel id _) (Graph g) =
  Graph (I.insNode (int, NodeLabel id new_info) $ I.delNode int g)

-- | Copies the node label from one node to another node.

copyNodeLabel :: NodeId -> NodeId -> Graph -> Graph
copyNodeLabel to_id from_id g =
  let (_, new_label) = head $ nodesByNodeId to_id g
      nodes_to_update = nodesByNodeId from_id g
  in foldr (replaceNodeLabel new_label) g nodes_to_update

-- | Merges all nodes that are the same as a given node according to some
-- user-defined criteria.

mergeNodes :: (I.LNode NodeLabel -> I.LNode NodeLabel -> Bool) -- ^ Test
                                                               -- function.
              -> I.LNode NodeLabel -- ^ Node to merge to.
              -> Graph
              -> Graph
mergeNodes f node (Graph g) =
  let same_nodes = filter (f node) (I.labNodes g)
      nodes_to_merge = filter (/= node) same_nodes
      redirected_g = foldr (redirectEdges (nodeInt node))
                     g (map nodeInt nodes_to_merge)
      pruned_g = I.delNodes (map nodeInt nodes_to_merge) redirected_g
  in Graph pruned_g
redirectEdges dst_int replace_int g =
  redirectOutEdges dst_int replace_int $ redirectInEdges dst_int replace_int g
redirectInEdges dst_int replace_int g =
  foldr (redirectInEdge dst_int) g (I.inn g replace_int)
redirectInEdge dst_int e@(out_int, _, EdgeLabel out_nr in_nr) g =
  let new_e = (out_int, dst_int, EdgeLabel out_nr (nextInEdgeNr g dst_int))
  in I.insEdge new_e $ I.delLEdge e g
redirectOutEdges dst_int replace_int g =
  foldr (redirectOutEdge dst_int) g (I.out g replace_int)
redirectOutEdge dst_int e@(_, in_int, EdgeLabel out_nr in_nr) g =
  let new_e = (dst_int, in_int, EdgeLabel (nextOutEdgeNr g dst_int) in_nr)
  in I.insEdge new_e $ I.delLEdge e g
nextInEdgeNr :: (I.Gr NodeLabel EdgeLabel) -> I.Node -> EdgeId
nextInEdgeNr g int = 1 + (maximum $ [0] ++ (map inEdgeNr $ I.inn g int))
nextOutEdgeNr g int = 1 + (maximum $ [0] ++ (map outEdgeNr $ I.out g int))
inEdgeNr (_, _, EdgeLabel _ nr) = nr
outEdgeNr (_, _, EdgeLabel nr _) = nr

-- | Adds a new node to the graph.

addNewNode nl (Graph g) = Graph (I.insNode (nextNodeInt g, nl) g)

-- | Adds a new edge between to nodes to the graph. The edge numberings will be
-- set accordingly.

addNewEdge :: Graph
              -> ( I.LNode NodeLabel -- ^ Source node (from).
                 , I.LNode NodeLabel -- ^ Destination node (to).
                 )
              -> Graph
addNewEdge (Graph g) ((from_node_int, from_nl), (to_node_int, to_nl)) =
  let out_edge_nr = nextOutEdgeNr g from_node_int
      in_edge_nr = nextInEdgeNr g to_node_int
      new_e = (from_node_int, to_node_int, EdgeLabel out_edge_nr in_edge_nr)
  in Graph (I.insEdge new_e g)

lastAddedNode (Graph g) =
  let last_int = maximum $ I.nodes g
      label = I.lab g last_int
  in if isJust label
        then Just (last_int, fromJust label)
        else Nothing

-- | Normalizes a graph by merging nodes with identical node IDs and labels, as
-- well as adjacent data nodes (the parent will be kept).

normalize :: Graph -> Graph
normalize g = g
-- TODO: implement