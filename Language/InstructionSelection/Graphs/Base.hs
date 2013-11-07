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
, numNodes
, nodeId
, nodeLabel
, nodeInfo
, bbLabel
, nodeType
, hasSameNodeId
, haveSameNodeIds
, hasSameBBLabel
, haveSameBBLabels
, replaceNodeLabel
, replaceNodeInfo
, copyNodeLabel
, mergeNodes
, lastAddedNode
, topSort
, children
, isDataNodeType
, inGraph
) where

import qualified Data.Graph.Inductive as I
import Language.InstructionSelection.DataTypes
import Language.InstructionSelection.OpTypes
import Language.InstructionSelection.Utils
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
    | NTRet
    | NTUncondBranch BBLabel
    | NTCondBranch

          -- | Label taken if the register evaluates to @True@.

          BBLabel

          -- | Label taken if the register evaluates to @False@.

          BBLabel

    | NTPhi

      -- | Temporary and constant nodes (appearing in IR and pattern code), as
      -- well as register and immediate nodes (appearing only in pattern code),
      -- are all represented as a data node. What distinguishes one from another
      -- is the constraints applied to it. A data node may (and should) also be
      -- of a certain data type.

    | NTData (Maybe DataType)

    deriving (Show,Eq)

isDataNodeType :: NodeType -> Bool
isDataNodeType (NTData _) = True
isDataNodeType _ = False

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

-- | Gets the node label from a node.

nodeLabel :: Node -> NodeLabel
nodeLabel (_, nl) = nl

-- | Gets the node info from a node.

nodeInfo :: Node -> NodeInfo
nodeInfo (_, NodeLabel _ ni) = ni

-- | Gets the node type from a node.

nodeType :: Node -> NodeType
nodeType (_, NodeLabel _ (NodeInfo nt _ _)) = nt

-- | Gets the basic-block label from a node.

bbLabel :: Node -> BBLabel
bbLabel (_, NodeLabel _ (NodeInfo _ l _)) = l

-- | Gets the internal node ID from a node.

nodeInt :: Node -> I.Node
nodeInt (int, _) = int

-- | Checks if an has a given node ID.

hasSameNodeId :: NodeId -> Node -> Bool
hasSameNodeId i1 (_, NodeLabel i2 _) = i1 == i2

-- | Checks if two nodes have the same node ID.

haveSameNodeIds :: Node -> Node -> Bool
haveSameNodeIds (_, NodeLabel i1 _) (_, NodeLabel i2 _) = i1 == i2

-- | Checks if two nodes have the same internal node IDs.

haveSameNodeInts :: Node -> Node -> Bool
haveSameNodeInts (i1, _) (i2, _) = i1 == i2

-- | Checks if a node has a given basic-block label.

hasSameBBLabel :: BBLabel -> Node -> Bool
hasSameBBLabel label1 (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

-- | Checks if two nodes have the same label.

haveSameBBLabels :: Node -> Node -> Bool
haveSameBBLabels (_, NodeLabel _ (NodeInfo _ label1 _))
               (_, NodeLabel _ (NodeInfo _ label2 _)) = label1 == label2

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
copyNodeLabel n_from n_to g
  | haveSameNodeInts n_from n_to = g
  | otherwise = replaceNodeLabel (nodeLabel n_from) n_to g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. Edges that goes between the two nodes are not
-- added. If the two nodes are actually the same node, nothing happens.

mergeNodes :: Node     -- ^ Node to merge to.
              -> Node  -- ^ Node to merge.
              -> Graph
              -> Graph
mergeNodes n_to n_from (Graph g)
  | haveSameNodeInts n_from n_to = (Graph g)
  | otherwise =  Graph $ I.delNode (nodeInt n_from)
                 $ redirectEdges (nodeInt n_to) (nodeInt n_from) g

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
addNewEdge ((from_node_int, _), (to_node_int, _)) (Graph g) =
  let out_edge_nr = nextOutEdgeNr g from_node_int
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

-- | Gets the list of nodes arragned in topological order.

topSort :: Graph -> [Node]
topSort (Graph g) = map (fromJust . fromNodeInt g) (I.topsort g)

-- | Gets the children (if any) of a given node.

children :: Node -> Graph -> [Node]
children n (Graph g) = map (fromJust . fromNodeInt g) $ I.suc g (nodeInt n)

-- | Checks if a given node is within the graph.

inGraph :: Graph -> Node -> Bool
inGraph (Graph g) (int, _) = isJust $ fromNodeInt g int
