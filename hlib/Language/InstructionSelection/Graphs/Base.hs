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
-- Every node has an internal ID, and an external ID. The users of the graph
-- will only be exposed to the external ID, where nodes with the same external
-- ID indicate that the nodes are the same. However, internally the nodes may
-- still be different. Using the equality operator (@==@) checks if two nodes
-- are the same node internally, and comparing the node IDs checks if two nodes
-- are the same node externally.
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Graphs.Base (
  BBLabel
, Edge
, EdgeLabel (..)
, EdgeNr
, Graph (..)
, Matchable (..)
, MatchsetId
, NodeIdMatchset
, NodeMatchset
, Node
, NodeId
, NodeIdMapping
, NodeInfo (..)
, NodeLabel (..)
, NodeMapping
, NodeType (..)
, allNodes
, allEdges
, addNewEdge
, addNewNode
, convertMatchsetNToId
, convertMappingNToId
, copyNodeLabel
, delEdge
, delNode
, dom
, edges
, empty
, fromNodeId
, hasOrderedInEdges
, hasOrderedOutEdges
, iDom
, inEdgeNr
, inEdges
, isActionNode
, isComputationNode
, isComputationNodeType
, isControlNode
, isControlNodeType
, isDataNode
, isDataNodeType
, isEntityNode
, isInGraph
, isLabelNode
, isLabelNodeType
, isNullNode
, isNullNodeType
, isPhiNode
, isPhiNodeType
, isStateNode
, isStateNodeType
, isTransferNode
, isTransferNodeType
, lastAddedNode
, mapToPatternNodeIds
, mapToPatternNodes
, mapToSearchNodeIds
, mapToSearchNodes
, mergeNodes
, mkGraph
, nodeId
, nodeIds
, nodeInfo
, nodeLabel
, nodesByNodeId
, nodeType
, numNodes
, outEdgeNr
, outEdges
, predecessors
, redirectEdges
, redirectInEdges
, redirectOutEdges
, sourceOfEdge
, successors
, targetOfEdge
, updateEdgeSource
, updateEdgeTarget
, updateNodeId
, updateNodeInfo
, updateNodeLabel
) where

import Language.InstructionSelection.DataTypes
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.Utils ( Natural
                                           , removeDuplicates
                                           , toNatural
                                           )
import qualified Data.Graph.Inductive as I
import Data.List (sortBy)
import Data.Maybe



----------------
-- Type classes
----------------

class Matchable a where

  -- | Checks if one type matches another.

  matches :: a -> a -> Bool



--------------
-- Data types
--------------

type IntGraph = I.Gr NodeLabel EdgeLabel
type Node = I.LNode NodeLabel
type NodeId = Natural
type Edge = I.LEdge EdgeLabel
type EdgeNr = Natural
type BBLabel = String
type MatchsetId = Natural
type NodeIdMatchset = [NodeIdMapping]
type NodeMatchset = [NodeMapping]
type NodeIdMapping = ( NodeId -- ^ Node ID in search graph.
                     , NodeId -- ^ Node ID in pattern graph.
                     )
type NodeMapping = ( Node -- ^ Node in search graph.
                   , Node -- ^ Node in pattern graph.
                   )

-- | The outer-most data type which contains the graph itself.

data Graph
    = Graph IntGraph
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
    = ComputationNode { compOpType :: O.CompOp }
    | ControlNode { contOpType :: O.ControlOp }

      -- | Temporary and constant nodes (appearing in IR and pattern code), as
      -- well as register and immediate nodes (appearing only in pattern code),
      -- are all represented as data nodes. What distinguishes one from another
      -- are the constraints applied to it.

    | DataNode { dataType :: DataType }
    | LabelNode { bbLabel :: BBLabel }
    | PhiNode
    | StateNode
    | TransferNode

      -- | A node which matches any other node, meaning that @n == m@ is always
      -- @True@ if either @n@ or @m@ is of type `NullNode'. Only to be used
      -- within patterns (like in the generic phi patterns).

    | NullNode

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



------------------------
-- Type class instances
------------------------

instance Matchable (I.LNode NodeLabel) where
  n1 `matches` n2 = (nodeType n1) `matches` (nodeType n2)

instance Matchable NodeType where
  (ComputationNode op1) `matches` (ComputationNode op2) = op1 == op2
  (ControlNode op1) `matches` (ControlNode op2) = op1 == op2
  (DataNode d1) `matches` (DataNode d2) = d1 == d2
  (LabelNode _) `matches` (LabelNode _) = True
  PhiNode `matches` PhiNode = True
  StateNode `matches` StateNode = True
  TransferNode `matches` TransferNode = True
  NullNode `matches` _ = True
  _ `matches` NullNode = True
  _ `matches` _ = False



-------------
-- Functions
-------------

isActionNode :: Node -> Bool
isActionNode n =
     isComputationNode n
  || isControlNode n
  || isPhiNode n
  || isTransferNode n

isEntityNode :: Node -> Bool
isEntityNode n =
     isDataNode n
  || isStateNode n

isComputationNode :: Node -> Bool
isComputationNode n = isComputationNodeType $ nodeType n

isControlNode :: Node -> Bool
isControlNode n = isControlNodeType $ nodeType n

isDataNode :: Node -> Bool
isDataNode n = isDataNodeType $ nodeType n

isLabelNode :: Node -> Bool
isLabelNode n = isLabelNodeType $ nodeType n

isNullNode :: Node -> Bool
isNullNode n = isNullNodeType $ nodeType n

isPhiNode :: Node -> Bool
isPhiNode n = isPhiNodeType $ nodeType n

isStateNode :: Node -> Bool
isStateNode n = isStateNodeType $ nodeType n

isTransferNode :: Node -> Bool
isTransferNode n = isTransferNodeType $ nodeType n

isComputationNodeType :: NodeType -> Bool
isComputationNodeType (ComputationNode _) = True
isComputationNodeType _ = False

isControlNodeType :: NodeType -> Bool
isControlNodeType (ControlNode _) = True
isControlNodeType _ = False

isDataNodeType :: NodeType -> Bool
isDataNodeType (DataNode _) = True
isDataNodeType _ = False

isLabelNodeType :: NodeType -> Bool
isLabelNodeType (LabelNode _) = True
isLabelNodeType _ = False

isNullNodeType :: NodeType -> Bool
isNullNodeType NullNode = True
isNullNodeType _ = False

isPhiNodeType :: NodeType -> Bool
isPhiNodeType PhiNode = True
isPhiNodeType _ = False

isStateNodeType :: NodeType -> Bool
isStateNodeType StateNode = True
isStateNodeType _ = False

isTransferNodeType :: NodeType -> Bool
isTransferNodeType TransferNode = True
isTransferNodeType _ = False

-- | Creates an empty graph.

empty :: Graph
empty = Graph I.empty

-- | Makes a graph from a list of nodes and edges.

mkGraph :: [Node] -> [Edge] -> Graph
mkGraph ns es = Graph (I.mkGraph ns es)

-- | Gets the next internal node ID which does not already appear in the graph.

nextNodeInt :: IntGraph -> I.Node
nextNodeInt g =
  let existing_nodes = I.nodes g
  in if length existing_nodes > 0
        then 1 + maximum existing_nodes
        else 0

-- | Gets the next node ID which does not already appear in the graph.

nextNodeId :: IntGraph -> NodeId
nextNodeId g = toNatural $ toInteger $ nextNodeInt g

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

-- | Gets the number of nodes.

numNodes :: Graph -> Int
numNodes g = length $ allNodes g

-- | Gets a list of all nodes.

allNodes :: Graph -> [Node]
allNodes (Graph g) = I.labNodes g

-- | Deletes a node from the graph. Any edges involving the given node will be
-- removed.

delNode :: Node -> Graph -> Graph
delNode n (Graph g) = Graph (I.delNode (nodeInt n) g)

-- | Deletes an edge from the graph.

delEdge :: Edge -> Graph -> Graph
delEdge e (Graph g) = Graph (I.delLEdge e g)

-- | Gets a list of nodes with the same node ID.

nodesByNodeId :: Graph -> NodeId -> [Node]
nodesByNodeId g i = filter ((i ==) . nodeId) $ allNodes g

-- | Updates the node label of an already existing node.

updateNodeLabel ::  NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph ((nodeInt n, new_label):all_nodes_but_n) (allEdges g)

-- | Updates the node info of an already existing node.

updateNodeInfo :: NodeInfo -> Node -> Graph -> Graph
updateNodeInfo new_info n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph ((nodeInt n, NodeLabel (nodeId n) new_info):all_nodes_but_n)
     (allEdges g)

-- | Updates the node ID of an already existing node.

updateNodeId :: NodeId -> Node -> Graph -> Graph
updateNodeId new_id n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph ((nodeInt n, NodeLabel new_id (nodeInfo n)):all_nodes_but_n)
     (allEdges g)

-- | Copies the node label from one node to another node. If the two nodes are
-- actually the same node, nothing happens.

copyNodeLabel :: Node     -- ^ Node to copy label to.
                 -> Node  -- ^ Node to copy label from.
                 -> Graph
                 -> Graph
copyNodeLabel to_n from_n g
  | (nodeInt from_n) == (nodeInt to_n) = g
  | otherwise = updateNodeLabel (nodeLabel from_n) to_n g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. If the two nodes are actually the same node, nothing
-- happens. Any edges already involving the two nodes will be removed.

mergeNodes :: Node     -- ^ Node to merge with (will be kept).
              -> Node  -- ^ Node to merge with (will be discarded).
              -> Graph
              -> Graph
mergeNodes n_to_keep n_to_discard g
  | (nodeInt n_to_keep) == (nodeInt n_to_discard) = g
  | otherwise = let edges_to_ignore = edges g n_to_discard n_to_keep
                in delNode n_to_discard
                   $ redirectEdges n_to_keep n_to_discard
                   $ foldl (flip delEdge) g edges_to_ignore

-- | Redirects all edges involving one node to another node.

redirectEdges :: Node        -- ^ Node to redirect edges to.
                 -> Node     -- ^ Node to redirect edges from.
                 -> Graph
                 -> Graph
redirectEdges to_n from_n g =
  redirectInEdges to_n from_n $ redirectOutEdges to_n from_n g

-- | Redirects all inbound edges to one node to another node.

redirectInEdges :: Node     -- ^ Node to redirect edges to.
                   -> Node  -- ^ Node to redirect edges from.
                   -> Graph
                   -> Graph
redirectInEdges to_n from_n g =
  foldr (updateEdgeTarget to_n) g (inEdges g from_n)

-- | Updates the target of an edge.

updateEdgeTarget :: Node     -- ^ New target.
                    -> Edge  -- ^ The edge to update.
                    -> Graph
                    -> Graph
updateEdgeTarget new_target e@(source, _, EdgeLabel out_nr _) (Graph g) =
  let new_target_int = nodeInt new_target
      new_e = (source,
               new_target_int,
               EdgeLabel out_nr (nextInEdgeNr g new_target_int))
  in Graph (I.insEdge new_e $ I.delLEdge e g)

-- | Redirects the outbound edges from one node to another.

redirectOutEdges :: Node     -- ^ Node to redirect edges to.
                    -> Node  -- ^ Node to redirect edges from.
                    -> Graph
                    -> Graph
redirectOutEdges to_n from_n g =
  foldr (updateEdgeSource to_n) g (outEdges g from_n)

-- | Updates the source of an edge.

updateEdgeSource :: Node     -- ^ New source.
                    -> Edge  -- ^ The edge to update.
                    -> Graph
                    -> Graph
updateEdgeSource new_source e@(_, target, EdgeLabel _ in_nr) (Graph g) =
  let new_source_int = nodeInt new_source
      new_e = (new_source_int,
               target,
               EdgeLabel (nextOutEdgeNr g new_source_int) in_nr)
  in Graph (I.insEdge new_e $ I.delLEdge e g)

nextInEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextInEdgeNr g int =
  let existing_numbers = (map inEdgeNr $ I.inn g int)
  in if length existing_numbers > 0
        then 1 + maximum existing_numbers
        else 0

nextOutEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextOutEdgeNr g int =
  let existing_numbers = (map outEdgeNr $ I.inn g int)
  in if length existing_numbers > 0
        then 1 + maximum existing_numbers
        else 0

inEdgeNr :: Edge -> EdgeNr
inEdgeNr (_, _, EdgeLabel _ nr) = nr

outEdgeNr :: Edge -> EdgeNr
outEdgeNr (_, _, EdgeLabel nr _) = nr

-- | Adds a new node to the graph.

addNewNode :: NodeInfo -> Graph -> ( Graph -- ^ The new graph.
                                   , Node  -- ^ The newly added node.
                                   )
addNewNode ni (Graph g) =
  let new_n = (nextNodeInt g, NodeLabel (nextNodeId g) ni)
      new_g = Graph (I.insNode new_n g)
  in (new_g, new_n)

-- | Adds a new edge between to nodes to the graph. The edge numberings will be
-- set accordingly.

addNewEdge :: ( Node -- ^ Source node (from).
              , Node -- ^ Destination node (to).
              )
              -> Graph
              -> ( Graph -- ^ The new graph.
                 , Edge  -- ^ The newly added edge.
                 )
addNewEdge (from_n, to_n) (Graph g) =
  let from_node_int = nodeInt from_n
      to_node_int = nodeInt to_n
      out_edge_nr = nextOutEdgeNr g from_node_int
      in_edge_nr = nextInEdgeNr g to_node_int
      new_e = (from_node_int, to_node_int, EdgeLabel out_edge_nr in_edge_nr)
      new_g = Graph (I.insEdge new_e g)
  in (new_g, new_e)

-- | Gets the node that was last added to the graph.

lastAddedNode :: Graph -> Maybe Node
lastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
        then let last_int = maximum ns
             in fromNodeInt g last_int
        else Nothing

-- | Gets the corresponding node from an internal node ID.

fromNodeInt :: IntGraph -> I.Node -> Maybe Node
fromNodeInt g int = maybe Nothing (\nl -> Just (int, nl)) (I.lab g int)

-- | Gets the predecessors (if any) of a given node. A node A is a predecessor
-- of another node B if there is a directed edge from B to A.

predecessors :: Graph -> Node -> [Node]
predecessors (Graph g) n = map (fromJust . fromNodeInt g) $ I.pre g (nodeInt n)

-- | Gets the successors (if any) of a given node. A node A is a successor of
-- another node B if there is a directed edge from A to B.

successors :: Graph -> Node -> [Node]
successors (Graph g) n = map (fromJust . fromNodeInt g) $ I.suc g (nodeInt n)

-- | Checks if a given node is within the graph.

isInGraph :: Graph -> Node -> Bool
isInGraph (Graph g) n = isJust $ fromNodeInt g (nodeInt n)

-- | Gets a list of all edges.

allEdges :: Graph -> [Edge]
allEdges (Graph g) = I.labEdges g

-- | Gets all inbound edges to a particular node.

inEdges :: Graph -> Node -> [Edge]
inEdges (Graph g) n = I.inn g (nodeInt n)

-- | Gets all outbound edges from a particular node.

outEdges :: Graph -> Node -> [Edge]
outEdges (Graph g) n = I.out g (nodeInt n)

-- | Gets a particular edge or edges between two nodes. If there are more than
-- one edge, the list will be sorted in increasing order of out-edge numbers.

edges :: Graph
        -> Node -- ^ The 'from' node.
        -> Node -- ^ The 'to' node.
        -> [Edge]
edges g from_n to_n =
  let out_edges = outEdges g from_n
      from_int = nodeInt from_n
      to_int = nodeInt to_n
      es = filter (\(n1, n2, _) -> from_int == n1 && to_int == n2) out_edges
      sorted_es = sortBy
                  (\n -> \m -> if inEdgeNr n < inEdgeNr m then LT else GT)
                  es
  in sorted_es

-- | Checks if a node has ordered inbound edges. This also entails that the
-- number of edges must be the same.

hasOrderedInEdges :: Node -> Bool
hasOrderedInEdges n = hasOrderedInEdges' (nodeType n)

hasOrderedInEdges' :: NodeType -> Bool
hasOrderedInEdges' (ComputationNode op) = not $ O.isOpCommutative op
hasOrderedInEdges' (LabelNode _) = True
hasOrderedInEdges' PhiNode = True
hasOrderedInEdges' _ = False

  -- | Checks if a node has ordered outbound edges. This also entails that the
-- number of edges must be the same.

hasOrderedOutEdges :: Node -> Bool
hasOrderedOutEdges n = hasOrderedOutEdges' (nodeType n)

hasOrderedOutEdges' :: NodeType -> Bool
hasOrderedOutEdges' (ControlNode O.CondBranch) = True
hasOrderedOutEdges' _ = False

-- | Gets the source node of an edge.

sourceOfEdge :: Graph -> Edge -> Node
sourceOfEdge (Graph g) (n, _, _) = fromJust $ fromNodeInt g n

-- | Gets the target node of an edge.

targetOfEdge :: Graph -> Edge -> Node
targetOfEdge (Graph g) (_, n, _) = fromJust $ fromNodeInt g n

-- | Gets the nodes that matches a given node ID.

fromNodeId :: Graph -> NodeId -> [Node]
fromNodeId g id = filter (\n -> nodeId n == id) (allNodes g)

-- | Gets a list of dominator sets, given a root node.

dom :: Graph
       -> Node      -- ^ The root node.
       -> [( Node   -- ^ The dominated node.
           , [Node] -- ^ The dominator nodes.
           )]
dom (Graph g) n =
  let dom_sets = I.dom g (nodeInt n)
  in map (\(n1, ns2) -> (fromJust $ fromNodeInt g n1,
                         map (fromJust . fromNodeInt g) ns2))
         dom_sets

-- | Gets a list of immediate-dominator mappings, given a root node.

iDom :: Graph
        -> Node    -- ^ The root node.
        -> [( Node -- ^ The dominated node.
            , Node -- ^ The dominator node.
            )]
iDom (Graph g) n =
  let idom_maps = I.iDom g (nodeInt n)
  in map (\(n1, n2) -> (fromJust $ fromNodeInt g n1,
                        fromJust $ fromNodeInt g n2))
         idom_maps

-- | Gets the node IDs of a node matchset. Duplicated entries are removed.

convertMatchsetNToId :: NodeMatchset -> NodeIdMatchset
convertMatchsetNToId node_maps =
  let id_maps = map (\(n1, n2) -> (nodeId n1, nodeId n2)) node_maps
      unique_id_maps = removeDuplicates id_maps
  in unique_id_maps

-- | Creates a node ID mapping from a node mapping.

convertMappingNToId :: NodeMapping -> NodeIdMapping
convertMappingNToId (n1, n2) = (nodeId n1, nodeId n2)

-- | Gets the node IDs of a list of nodes. Duplicate node IDs are removed.

nodeIds :: [Node] -> [NodeId]
nodeIds = removeDuplicates . map nodeId

-- | For a list of nodes in the search graph and a node mapping, get the pattern
-- nodes which has a corresponding mapping to a search node in the list.

mapToPatternNodes :: [Node]          -- ^ List of search graph nodes.
                     -> NodeMatchset -- ^ Search-to-pattern matchset.
                     -> [Node]       -- ^ List of pattern graph nodes.
mapToPatternNodes ns m = [ n2 | (n1, n2) <- m, n <- ns, n == n1]

-- | Same as `mapToPatternNodes` but goes in the other direction.

mapToSearchNodes :: [Node]          -- ^ List of pattern graph nodes.
                    -> NodeMatchset -- ^ Search-to-pattern matchset.
                    -> [Node]       -- ^ List of search graph nodes.
mapToSearchNodes ns m = [ n1 | (n1, n2) <- m, n <- ns, n == n2]

-- | Same as `mapToPatternNodes` but operates on `NodeId`s instead of `Node`s.

mapToPatternNodeIds :: [NodeId]          -- ^ List of search graph node IDs.
                       -> NodeIdMatchset -- ^ Search-to-pattern matchset.
                       -> [NodeId]       -- ^ List of pattern graph node IDs.
mapToPatternNodeIds ns m = [ n2 | (n1, n2) <- m, n <- ns, n == n1]

-- | Same as `mapToPatternNodeIds` but goes in the other direction.

mapToSearchNodeIds :: [NodeId]          -- ^ List of pattern graph node IDs.
                      -> NodeIdMatchset -- ^ Search-to-pattern matchset.
                      -> [NodeId]       -- ^ List of search graph node IDs.
mapToSearchNodeIds ns m = [ n1 | (n1, n2) <- m, n <- ns, n == n2]
