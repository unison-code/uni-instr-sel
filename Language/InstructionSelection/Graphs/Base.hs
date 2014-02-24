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
  Edge
, EdgeLabel (..)
, EdgeNr
, Graph (..)
, I.LNode
, I.LEdge
, Node
, NodeId
, NodeInfo (..)
, NodeLabel (..)
, NodeType (..)
, addNewEdge
, addNewNode
, children
, copyNodeLabel
, empty
, hasSameNodeId
, hasSameNodeType
, haveSameNodeIds
, haveSameNodeTypes
, inGraph
, isDataNodeType
, lastAddedNode
, mergeNodes
, mkGraph
, newNodeId
, nodeId
, nodeInfo
, nodeLabel
, nodes
, nodesByNodeId
, nodeType
, numNodes
, replaceNodeInfo
, replaceNodeLabel
) where

import Language.InstructionSelection.DataTypes
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.Utils
import qualified Data.Graph.Inductive as I
import Data.Maybe



--------------
-- Data types
--------------

type IntGraph = I.Gr NodeLabel EdgeLabel
type Node = I.LNode NodeLabel
type NodeId = Natural
type Edge = I.LEdge EdgeLabel
type EdgeNr = Natural
type BBLabel = String

-- | The outer-most data type which contains the graph itself.

data Graph
    = Graph { intGraph :: IntGraph }
    deriving (Show)

-- | Node label, consisting of an ID that can be shared by multiple nodes (thus
-- representing that they are actually the same node) and node information which
-- denotes the type of node and other auxiliary information.

data NodeLabel
    = NodeLabel

          -- | Node identifier. Most often this is equal to the 'Node'
          -- identifier used by FGL, but it does not need to be.

          NodeId

          -- | Additional data to describe the node.

          NodeInfo

    deriving (Show, Eq)

-- | Node information. Most importantly this specifies the node type.

data NodeInfo
    = NodeInfo

          -- | Type of node.

          NodeType

          -- | A field to put arbitrary text in (used when printing the graph).

          String

    deriving (Show, Eq)

data NodeType
    = ComputationNode O.CompOp
    | ControlNode O.ControlOp

      -- | Temporary and constant nodes (appearing in IR and pattern code), as
      -- well as register and immediate nodes (appearing only in pattern code),
      -- are all represented as data nodes. What distinguishes one from another
      -- are the constraints applied to it.

    | DataNode DataType
    | LabelNode BBLabel
    | PhiNode
    | StateNode
    | TransferNode

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



-------------
-- Functions
-------------

isDataNodeType :: NodeType -> Bool
isDataNodeType (DataNode _) = True
isDataNodeType _ = False

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

-- | Gets the node label from a node.

nodeLabel :: Node -> NodeLabel
nodeLabel (_, nl) = nl

-- | Gets the node info from a node.

nodeInfo :: Node -> NodeInfo
nodeInfo (_, NodeLabel _ ni) = ni

-- | Gets the node type from a node.

nodeType :: Node -> NodeType
nodeType (_, NodeLabel _ (NodeInfo nt _)) = nt

-- | Gets the internal node ID from a node.

nodeInt :: Node -> I.Node
nodeInt (int, _) = int

-- | Checks if an has a given node ID.

hasSameNodeId :: NodeId -> Node -> Bool
hasSameNodeId i n = i == (nodeId n)

-- | Checks if two nodes have the same node ID.

haveSameNodeIds :: Node -> Node -> Bool
haveSameNodeIds n1 n2 = (nodeId n1) == (nodeId n2)

-- | Checks if two nodes have the same internal node IDs.

haveSameNodeInts :: Node -> Node -> Bool
haveSameNodeInts n1 n2 = (nodeInt n1) == (nodeInt n2)

-- | Checks if an has a given node type

hasSameNodeType :: NodeType -> Node -> Bool
hasSameNodeType nt n = nt == (nodeType n)

-- | Checks if two nodes have the same node types

haveSameNodeTypes :: Node -> Node -> Bool
haveSameNodeTypes n1 n2 = (nodeType n1) == (nodeType n2)

-- | Gets the number of nodes.

numNodes :: Graph -> Int
numNodes g = length $ nodes g

-- | Gets the list of nodes.

nodes :: Graph -> [Node]
nodes (Graph g) = I.labNodes g

-- | Gets a list of nodes with the same node ID.

nodesByNodeId :: NodeId -> Graph -> [Node]
nodesByNodeId i g = filter (hasSameNodeId i) $ nodes g

-- | Replaces the node label of an already existing node.

replaceNodeLabel :: NodeLabel -> Node -> Graph -> Graph
replaceNodeLabel nl (int, _) (Graph g) =
  Graph $ I.insNode (int, nl) g

-- | Replaces the node info of an already existing node.

replaceNodeInfo :: NodeInfo -> Node -> Graph -> Graph
replaceNodeInfo ni (int, NodeLabel i _) (Graph g) =
  Graph $ I.insNode (int, NodeLabel i ni) g

-- | Copies the node label from one node to another node. If the two nodes are
-- actually the same node, nothing happens.

copyNodeLabel :: Node     -- ^ Node to copy label from.
                 -> Node  -- ^ Node to copy label to.
                 -> Graph
                 -> Graph
copyNodeLabel from_n to_n g
  | haveSameNodeInts from_n to_n = g
  | otherwise = replaceNodeLabel (nodeLabel from_n) to_n g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. Edges that goes between the two nodes are not
-- added. If the two nodes are actually the same node, nothing happens.

mergeNodes :: Node     -- ^ Node to merge to.
              -> Node  -- ^ Node to merge.
              -> Graph
              -> Graph
mergeNodes to_n from_n (Graph g)
  | haveSameNodeInts from_n to_n = (Graph g)
  | otherwise =  Graph $ I.delNode (nodeInt from_n)
                 $ redirectEdges (nodeInt to_n) (nodeInt from_n) g

redirectEdges :: I.Node -> I.Node -> IntGraph -> IntGraph
redirectEdges dst_int replace_int g =
  redirectOutEdges dst_int replace_int $ redirectInEdges dst_int replace_int g

redirectInEdges :: I.Node -> I.Node -> IntGraph -> IntGraph
redirectInEdges dst_int replace_int g =
  let es_not_to_redirect = filter (\(int, _, _) -> int == dst_int)
                           (I.inn g replace_int)
      g' = foldr I.delLEdge g es_not_to_redirect
  in foldr (redirectInEdge dst_int) g (I.inn g' replace_int)

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
addNewEdge (from_n, to_n) (Graph g) =
  let from_node_int = nodeInt from_n
      to_node_int = nodeInt to_n
      out_edge_nr = nextOutEdgeNr g from_node_int
      in_edge_nr = nextInEdgeNr g to_node_int
      new_e = (from_node_int, to_node_int, EdgeLabel out_edge_nr in_edge_nr)
  in Graph (I.insEdge new_e g)

-- | Gets the node that was last added to the graph.

lastAddedNode :: Graph -> Maybe Node
lastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
        then let last_int = maximum ns
             in fromNodeInt g last_int
        else Nothing

fromNodeInt :: IntGraph -> I.Node -> Maybe Node
fromNodeInt g int = maybe Nothing (\nl -> Just (int, nl)) (I.lab g int)

-- | Gets the children (if any) of a given node.

children :: Node -> Graph -> [Node]
children n (Graph g) = map (fromJust . fromNodeInt g) $ I.suc g (nodeInt n)

-- | Checks if a given node is within the graph.

inGraph :: Graph -> Node -> Bool
inGraph (Graph g) n = isJust $ fromNodeInt g (nodeInt n)
