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
  Node
, NodeId
, Edge
, EdgeNr
, BBLabel (..)
, NodeType (..)
, NodeInfo (..)
, NodeLabel (..)
, EdgeLabel (..)
, Graph (..)
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
, replaceNodeInfo
, copyNodeLabel
, mergeNodes
, lastAddedNode
) where

import qualified Data.Graph.Inductive as I
import Language.InstructionSelection.Utils
import Language.InstructionSelection.OpTypes
import Data.Maybe



type Node = I.LNode NodeLabel
type NodeId = Natural
type Edge = I.LEdge EdgeLabel
type EdgeNr = Natural
type IntGraph = I.Gr NodeLabel EdgeLabel

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

          EdgeNr

          -- | Destination edge number.

          EdgeNr

    deriving (Show,Eq)

data Graph
    = Graph { intGraph :: IntGraph }
    deriving (Show)

-- | Creates an empty graph.

empty :: Graph
empty = Graph I.empty

-- | Makes a graph from a list of nodes and edges.

mkGraph :: [Node] -> [Edge] -> Graph
mkGraph ns es = Graph (I.mkGraph ns es)

-- | Gets the next internal node ID which does not currently appear in the
-- graph.

nextNodeInt :: IntGraph -> I.Node
nextNodeInt g = 1 + (maximum $ [0] ++ I.nodes g)

-- | Makes a unique node ID not already appearing in the graph.

newNodeId :: Graph -> NodeId
newNodeId (Graph g) = toNatural $ toInteger $ nextNodeInt g

-- | Gets the node ID from a node.

nodeId :: Node -> NodeId
nodeId (_, NodeLabel i _) = i

-- | Gets the internal node ID from a node.

nodeInt :: Node -> I.Node
nodeInt (int, _) = int

-- | Checks if an has a given node ID.

hasSameNodeId :: NodeId -> Node -> Bool
hasSameNodeId i1 (_, NodeLabel i2 _) = i1 == i2

-- | Checks if two nodes have the same node ID.

haveSameNodeIds :: Node -> Node -> Bool
haveSameNodeIds (_, NodeLabel i1 _) (_, NodeLabel i2 _) = i1 == i2

-- | Checks if a node has a given basic-block label.

hasSameLabel :: BBLabel -> Node -> Bool
hasSameLabel label1 (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

-- | Checks if two nodes have the same label.

haveSameLabels :: Node -> Node -> Bool
haveSameLabels (_, NodeLabel _ (NodeInfo _ label1 _))
               (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

-- | Checks if two nodes have the same node ID and label.

haveSameNodeIdsAndLabels :: Node -> Node -> Bool
haveSameNodeIdsAndLabels n1 n2 = haveSameNodeIds n1 n2 && haveSameLabels n1 n2

-- | Gets the list of nodes.

nodes :: Graph -> [Node]
nodes (Graph g) = I.labNodes g

-- | Gets a list of nodes with the same node ID.

nodesByNodeId :: NodeId -> Graph -> [Node]
nodesByNodeId i g = filter (hasSameNodeId i) $ nodes g

-- | Replaces the node label of an already existing node.

replaceNodeLabel :: NodeLabel -> Node -> Graph -> Graph
replaceNodeLabel new_label (int, _) (Graph g) =
  Graph (I.insNode (int, new_label) $ I.delNode int g)

-- | Replaces the node info of an already existing node.

replaceNodeInfo :: NodeInfo -> Node -> Graph -> Graph
replaceNodeInfo new_info (int, NodeLabel i _) (Graph g) =
  Graph (I.insNode (int, NodeLabel i new_info) $ I.delNode int g)

-- | Copies the node label from one node to another node.

copyNodeLabel :: NodeId -> NodeId -> Graph -> Graph
copyNodeLabel to_i from_i g =
  let (_, new_label) = head $ nodesByNodeId to_i g
      nodes_to_update = nodesByNodeId from_i g
  in foldr (replaceNodeLabel new_label) g nodes_to_update

-- | Merges all nodes that are the same as a given node according to some
-- user-defined criteria.

mergeNodes :: (Node -> Node -> Bool) -- ^ Test function.
              -> Node                -- ^ Node to merge to.
              -> Graph
              -> Graph
mergeNodes f node (Graph g) =
  let same_nodes = filter (f node) (I.labNodes g)
      nodes_to_merge = filter (/= node) same_nodes
      redirected_g = foldr (redirectEdges (nodeInt node))
                     g (map nodeInt nodes_to_merge)
      pruned_g = I.delNodes (map nodeInt nodes_to_merge) redirected_g
  in Graph pruned_g

redirectEdges :: I.Node -> I.Node -> IntGraph -> IntGraph
redirectEdges dst_int replace_int g =
  redirectOutEdges dst_int replace_int $ redirectInEdges dst_int replace_int g
redirectInEdges :: I.Node -> I.Node -> IntGraph -> IntGraph
redirectInEdges dst_int replace_int g =
  foldr (redirectInEdge dst_int) g (I.inn g replace_int)
redirectInEdge :: I.Node -> Edge -> IntGraph -> IntGraph
redirectInEdge dst_int e@(out_int, _, EdgeLabel out_nr _) g =
  let new_e = (out_int, dst_int, EdgeLabel out_nr (nextInEdgeNr g dst_int))
  in I.insEdge new_e $ I.delLEdge e g
redirectOutEdges :: I.Node -> I.Node -> IntGraph -> IntGraph
redirectOutEdges dst_int replace_int g =
  foldr (redirectOutEdge dst_int) g (I.out g replace_int)
redirectOutEdge :: I.Node -> Edge -> IntGraph -> IntGraph
redirectOutEdge dst_int e@(_, in_int, EdgeLabel _ in_nr) g =
  let new_e = (dst_int, in_int, EdgeLabel (nextOutEdgeNr g dst_int) in_nr)
  in I.insEdge new_e $ I.delLEdge e g
nextInEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextInEdgeNr g int = 1 + (maximum $ [0] ++ (map inEdgeNr $ I.inn g int))
nextOutEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextOutEdgeNr g int = 1 + (maximum $ [0] ++ (map outEdgeNr $ I.out g int))
inEdgeNr :: Edge -> EdgeNr
inEdgeNr (_, _, EdgeLabel _ nr) = nr
outEdgeNr :: Edge -> EdgeNr
outEdgeNr (_, _, EdgeLabel nr _) = nr

-- | Adds a new node to the graph.

addNewNode :: NodeLabel -> Graph -> Graph
addNewNode nl (Graph g) = Graph (I.insNode (nextNodeInt g, nl) g)

-- | Adds a new edge between to nodes to the graph. The edge numberings will be
-- set accordingly.

addNewEdge :: ( Node -- ^ Source node (from).
              , Node -- ^ Destination node (to).
              )
              -> Graph
              -> Graph
addNewEdge ((from_node_int, _), (to_node_int, _)) (Graph g) =
  let out_edge_nr = nextOutEdgeNr g from_node_int
      in_edge_nr = nextInEdgeNr g to_node_int
      new_e = (from_node_int, to_node_int, EdgeLabel out_edge_nr in_edge_nr)
  in Graph (I.insEdge new_e g)

lastAddedNode :: Graph -> Maybe Node
lastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
        then let last_int = maximum ns
                 label = I.lab g last_int
             in Just (last_int, fromJust label)
        else Nothing
