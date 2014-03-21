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

module Language.InstructionSelection.Graphs.Base (
  BBLabel
, Edge
, EdgeLabel (..)
, EdgeNr (..)
, Graph
, Mapping (..)
, Matchset (..)
, Node
, NodeId (..)
, NodeInfo (..)
, NodeLabel (..)
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
, fNode
, fNodes
, fromNodeId
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
, mappedNodesFToP
, mappedNodesPToF
, matchingNodes
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
, pNode
, pNodes
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



--------------
-- Data types
--------------

type IntGraph = I.Gr NodeLabel EdgeLabel

-- | The outer-most data type which contains the graph itself.

newtype Graph
    = Graph IntGraph
    deriving (Show)

-- | Represents a distinct node.

newtype Node
    = Node (I.LNode NodeLabel)
    deriving (Show, Eq)

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

-- | Node ID data type.

newtype NodeId
    = NodeId Natural
    deriving (Show, Eq, Ord)

instance Num NodeId where
    fromInteger = NodeId . toNatural
    (NodeId x) + (NodeId y) = NodeId (x + y)
    (NodeId x) - (NodeId y) = NodeId (x - y)
    (NodeId x) * (NodeId y) = NodeId (x * y)
    abs (NodeId x) = NodeId (abs x)
    signum (NodeId x) = NodeId (signum x)

instance Enum NodeId where
  toEnum = NodeId . toEnum
  fromEnum (NodeId x) = fromEnum x



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

-- | Represents a distinct edge.

newtype Edge
    = Edge (I.LEdge EdgeLabel)
    deriving (Show, Eq)

instance Num EdgeNr where
    fromInteger = EdgeNr . toNatural
    (EdgeNr x) + (EdgeNr y) = EdgeNr (x + y)
    (EdgeNr x) - (EdgeNr y) = EdgeNr (x - y)
    (EdgeNr x) * (EdgeNr y) = EdgeNr (x * y)
    abs (EdgeNr x) = EdgeNr (abs x)
    signum (EdgeNr x) = EdgeNr (signum x)

instance Enum EdgeNr where
  toEnum = EdgeNr . toEnum
  fromEnum (EdgeNr x) = fromEnum x

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

    deriving (Show, Eq)

-- | Edge number, used for ordering edges.

newtype EdgeNr
    = EdgeNr Natural
    deriving (Show, Eq, Ord)

-- | Represents a basic block label.

newtype BBLabel
    = BBLabel String
    deriving (Show, Eq)

-- | Represents a matchset, which is a list of mappings.

newtype Matchset n
    = Matchset [Mapping n]
    deriving (Show)

-- | Represents a mapping between two entities (typically @Node@s or @NodeId@s).
-- The first value is the entity in the function, and the second value is the
-- entity in the pattern.

newtype Mapping n
    = Mapping (n, n)
    deriving (Show, Eq)



-------------
-- Functions
-------------

-- | Get the function node from a mapping.

fNode :: (Mapping n) -> n
fNode (Mapping (n, _)) = n

-- | Get all function nodes from a matchset.

fNodes :: (Matchset n) -> [n]
fNodes (Matchset m) = map fNode m

-- | Get the pattern node from a mapping.

pNode :: (Mapping n) -- ^ A mapping.
         -> n        -- ^ The pattern node in the mapping.
pNode (Mapping (_, n)) = n

-- | Get all pattern nodes from a matchset.

pNodes :: (Matchset n) -> [n]
pNodes (Matchset m) = map pNode m

toNode :: I.LNode NodeLabel -> Node
toNode = Node

toNodes :: [I.LNode NodeLabel] -> [Node]
toNodes = map Node

fromNode :: Node -> I.LNode NodeLabel
fromNode (Node n) = n

fromNodes :: [Node] -> [I.LNode NodeLabel]
fromNodes = map fromNode

toEdge :: I.LEdge EdgeLabel -> Edge
toEdge = Edge

toEdges :: [I.LEdge EdgeLabel] -> [Edge]
toEdges = map Edge

fromEdge :: Edge -> I.LEdge EdgeLabel
fromEdge (Edge e) = e

fromEdges :: [Edge] -> [I.LEdge EdgeLabel]
fromEdges = map fromEdge

toEdgeNr :: Natural -> EdgeNr
toEdgeNr = EdgeNr

toEdgeNrs :: [Natural] -> [EdgeNr]
toEdgeNrs = map EdgeNr

fromEdgeNr :: EdgeNr -> Natural
fromEdgeNr (EdgeNr n) = n

fromEdgeNrs :: [EdgeNr] -> [Natural]
fromEdgeNrs = map fromEdgeNr

toMapping :: (n, n) -> Mapping n
toMapping = Mapping

fromMapping :: Mapping n -> (n, n)
fromMapping (Mapping m) = m

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
mkGraph ns es = Graph (I.mkGraph (fromNodes ns) (fromEdges es))

-- | Gets the next internal node ID which does not already appear in the graph.

nextIntNodeId :: IntGraph -> I.Node
nextIntNodeId g =
  let existing_nodes = I.nodes g
  in if length existing_nodes > 0
        then 1 + maximum existing_nodes
        else 0

-- | Gets the next node ID which does not already appear in the graph.

nextNodeId :: IntGraph -> NodeId
nextNodeId g = NodeId $ toNatural $ toInteger $ nextIntNodeId g

-- | Gets the node ID from a node.

nodeId :: Node -> NodeId
nodeId (Node (_, NodeLabel i _)) = i

-- | Gets the node label from a node.

nodeLabel :: Node -> NodeLabel
nodeLabel (Node (_, nl)) = nl

-- | Gets the node info from a node.

nodeInfo :: Node -> NodeInfo
nodeInfo (Node (_, NodeLabel _ ni)) = ni

-- | Gets the node type from a node.

nodeType :: Node -> NodeType
nodeType (Node (_, NodeLabel _ (NodeInfo nt _))) = nt

-- | Gets the internal node ID from a node.

intNodeId :: Node -> I.Node
intNodeId (Node (id, _)) = id

-- | Gets the number of nodes.

numNodes :: Graph -> Int
numNodes g = length $ allNodes g

-- | Gets a list of all nodes.

allNodes :: Graph -> [Node]
allNodes (Graph g) = map Node (I.labNodes g)

-- | Deletes a node from the graph. Any edges involving the given node will be
-- removed.

delNode :: Node -> Graph -> Graph
delNode n (Graph g) = Graph (I.delNode (intNodeId n) g)

-- | Deletes an edge from the graph.

delEdge :: Edge -> Graph -> Graph
delEdge (Edge e) (Graph g) = Graph (I.delLEdge e g)

-- | Gets a list of nodes with the same node ID.

nodesByNodeId :: Graph -> NodeId -> [Node]
nodesByNodeId g i = filter ((i ==) . nodeId) $ allNodes g

-- | Updates the node label of an already existing node.

updateNodeLabel ::  NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph (Node (intNodeId n, new_label):all_nodes_but_n) (allEdges g)

-- | Updates the node info of an already existing node.

updateNodeInfo :: NodeInfo -> Node -> Graph -> Graph
updateNodeInfo new_info n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph (Node (intNodeId n, NodeLabel (nodeId n) new_info):all_nodes_but_n)
             (allEdges g)

-- | Updates the node ID of an already existing node.

updateNodeId :: NodeId -> Node -> Graph -> Graph
updateNodeId new_id n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph (Node (intNodeId n, NodeLabel new_id (nodeInfo n)):all_nodes_but_n)
             (allEdges g)

-- | Copies the node label from one node to another node. If the two nodes are
-- actually the same node, nothing happens.

copyNodeLabel :: Node     -- ^ Node to copy label to.
                 -> Node  -- ^ Node to copy label from.
                 -> Graph
                 -> Graph
copyNodeLabel to_n from_n g
  | (intNodeId from_n) == (intNodeId to_n) = g
  | otherwise = updateNodeLabel (nodeLabel from_n) to_n g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. If the two nodes are actually the same node, nothing
-- happens. Any edges already involving the two nodes will be removed.

mergeNodes :: Node     -- ^ Node to merge with (will be kept).
              -> Node  -- ^ Node to merge with (will be discarded).
              -> Graph
              -> Graph
mergeNodes n_to_keep n_to_discard g
  | (intNodeId n_to_keep) == (intNodeId n_to_discard) = g
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
updateEdgeTarget new_target (Edge e@(source, _, EdgeLabel out_nr _)) (Graph g) =
  let new_target_id = intNodeId new_target
      new_e = (source,
               new_target_id,
               EdgeLabel out_nr (nextInEdgeNr g new_target_id))
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
updateEdgeSource new_source (Edge e@(_, target, EdgeLabel _ in_nr)) (Graph g) =
  let new_source_id = intNodeId new_source
      new_e = (new_source_id,
               target,
               EdgeLabel (nextOutEdgeNr g new_source_id) in_nr)
  in Graph (I.insEdge new_e $ I.delLEdge e g)

nextInEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextInEdgeNr g int =
  let existing_numbers = map inEdgeNr $ toEdges $ I.inn g int
  in if length existing_numbers > 0
        then maximum existing_numbers + 1
        else 0

nextOutEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextOutEdgeNr g int =
  let existing_numbers = map outEdgeNr $ toEdges $ I.inn g int
  in if length existing_numbers > 0
        then maximum existing_numbers + 1
        else 0

inEdgeNr :: Edge -> EdgeNr
inEdgeNr (Edge (_, _, EdgeLabel _ nr)) = nr

outEdgeNr :: Edge -> EdgeNr
outEdgeNr (Edge (_, _, EdgeLabel nr _)) = nr

-- | Adds a new node to the graph.

addNewNode :: NodeInfo -> Graph -> ( Graph -- ^ The new graph.
                                   , Node  -- ^ The newly added node.
                                   )
addNewNode ni (Graph g) =
  let new_n = (nextIntNodeId g, NodeLabel (nextNodeId g) ni)
      new_g = Graph (I.insNode new_n g)
  in (new_g, Node new_n)

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
  let from_node_id = intNodeId from_n
      to_node_id = intNodeId to_n
      out_edge_nr = nextOutEdgeNr g from_node_id
      in_edge_nr = nextInEdgeNr g to_node_id
      new_e = (from_node_id, to_node_id, EdgeLabel out_edge_nr in_edge_nr)
      new_g = Graph (I.insEdge new_e g)
  in (new_g, Edge new_e)

-- | Gets the node that was last added to the graph, which is the node with the
-- highest internal node ID.

lastAddedNode :: Graph -> Maybe Node
lastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
        then intNodeId2Node g (maximum ns)
        else Nothing

-- | Gets the corresponding node from an internal node ID.

intNodeId2Node :: IntGraph -> I.Node -> Maybe Node
intNodeId2Node g id = maybe Nothing (\l -> Just (Node (id, l))) (I.lab g id)

-- | Gets the predecessors (if any) of a given node. A node A is a predecessor
-- of another node B if there is a directed edge from B to A.

predecessors :: Graph -> Node -> [Node]
predecessors (Graph g) n = map (fromJust . intNodeId2Node g)
                               (I.pre g (intNodeId n))

-- | Gets the successors (if any) of a given node. A node A is a successor of
-- another node B if there is a directed edge from A to B.

successors :: Graph -> Node -> [Node]
successors (Graph g) n = map (fromJust . intNodeId2Node g) (I.suc g (intNodeId n))

-- | Checks if a given node is within the graph.

isInGraph :: Graph -> Node -> Bool
isInGraph (Graph g) n = isJust $ intNodeId2Node g (intNodeId n)

-- | Gets a list of all edges.

allEdges :: Graph -> [Edge]
allEdges (Graph g) = toEdges $ I.labEdges g

-- | Gets all inbound edges to a particular node.

inEdges :: Graph -> Node -> [Edge]
inEdges (Graph g) n = toEdges $ I.inn g (intNodeId n)

-- | Gets all outbound edges from a particular node.

outEdges :: Graph -> Node -> [Edge]
outEdges (Graph g) n = toEdges $ I.out g (intNodeId n)

-- | Gets a particular edge or edges between two nodes. If there are more than
-- one edge, the list will be sorted in increasing order of out-edge numbers.

edges :: Graph
        -> Node -- ^ The 'from' node.
        -> Node -- ^ The 'to' node.
        -> [Edge]
edges g from_n to_n =
  let out_edges = fromEdges $ outEdges g from_n
      from_id = intNodeId from_n
      to_id = intNodeId to_n
      es = toEdges
           $ filter (\(n1, n2, _) -> from_id == n1 && to_id == n2) out_edges
      sorted_es = sortBy
                  (\n -> \m -> if inEdgeNr n < inEdgeNr m then LT else GT)
                  es
  in sorted_es

-- | Gets the source node of an edge.

sourceOfEdge :: Graph -> Edge -> Node
sourceOfEdge (Graph g) (Edge (n, _, _)) = fromJust $ intNodeId2Node g n

-- | Gets the target node of an edge.

targetOfEdge :: Graph -> Edge -> Node
targetOfEdge (Graph g) (Edge (_, n, _)) = fromJust $ intNodeId2Node g n

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
  let dom_sets = I.dom g (intNodeId n)
  in map (\(n1, ns2) -> (fromJust $ intNodeId2Node g n1,
                         map (fromJust . intNodeId2Node g) ns2))
         dom_sets

-- | Gets a list of immediate-dominator mappings, given a root node.

iDom :: Graph
        -> Node    -- ^ The root node.
        -> [( Node -- ^ The dominated node.
            , Node -- ^ The dominator node.
            )]
iDom (Graph g) n =
  let idom_maps = I.iDom g (intNodeId n)
  in map (\(n1, n2) -> (fromJust $ intNodeId2Node g n1,
                        fromJust $ intNodeId2Node g n2))
         idom_maps

-- | Converts matchset of nodes into a matchset of node IDs. Duplicated entries
-- are removed.

convertMatchsetNToId :: Matchset Node -> (Matchset NodeId)
convertMatchsetNToId (Matchset m_nodes) =
  let m_ids = map convertMappingNToId m_nodes
      m_unique_ids = removeDuplicates m_ids
  in Matchset m_unique_ids

-- | Converts a mapping of nodes into a mapping of node IDs.

convertMappingNToId :: Mapping Node -> (Mapping NodeId)
convertMappingNToId m = (Mapping (nodeId $ fNode m, nodeId $ pNode m))

-- | Gets the node IDs of a list of nodes. Duplicate node IDs are removed.

nodeIds :: [Node] -> [NodeId]
nodeIds = removeDuplicates . map nodeId

-- | Checks if a node matches another node.

matchingNodes :: Graph         -- ^ The function graph.
              -> Graph         -- ^ The pattern graph.
              -> Matchset Node -- ^ Current matchset state.
              -> Mapping Node  -- ^ Candidate mapping.
              -> Bool
matchingNodes fg pg st m =
     matchingNodeTypes (nodeType $ fNode m) (nodeType $ pNode m)
  && matchingEdges fg pg st m

-- | Checks if two node types are matching-compatible.

matchingNodeTypes :: NodeType -> NodeType -> Bool
matchingNodeTypes (ComputationNode op1) (ComputationNode op2) = op1 == op2
matchingNodeTypes (ControlNode op1) (ControlNode op2) = op1 == op2
matchingNodeTypes (DataNode d1) (DataNode d2) = d1 == d2
matchingNodeTypes (LabelNode _) (LabelNode _) = True
matchingNodeTypes PhiNode PhiNode = True
matchingNodeTypes StateNode StateNode = True
matchingNodeTypes TransferNode TransferNode = True
matchingNodeTypes NullNode _ = True
matchingNodeTypes _ NullNode = True
matchingNodeTypes _ _ = False

-- | Checks, for cases where it matters, that the nodes have a matching number
-- of edges, and that the already mapped predecessors and successors have a
-- matching edge ordering. At this point it can be assumed that the nodes in the
-- candidate mapping have matching node types (although one of the nodes can be
-- a NullNode).

matchingEdges :: Graph         -- ^ The function graph.
              -> Graph         -- ^ The pattern graph.
              -> Matchset Node -- ^ Current matchset state.
              -> Mapping Node  -- ^ Candidate mapping.
              -> Bool
matchingEdges fg pg st m =
     matchingNumberOfInEdges fg pg st m
  && matchingNumberOfOutEdges fg pg st m
  && matchingOrderingOfInEdges fg pg st m
  && matchingOrderingOfOutEdges fg pg st m
  && matchingOutEdgeOrderingOfPreds fg pg st m
  && matchingInEdgeOrderingOfSuccs fg pg st m

matchingNumberOfInEdges :: Graph            -- ^ The function graph.
                           -> Graph         -- ^ The pattern graph.
                           -> Matchset Node -- ^ Current matchset state.
                           -> Mapping Node  -- ^ Candidate mapping.
                           -> Bool
matchingNumberOfInEdges fg pg _ m =
  let fn = fNode m
      pn = pNode m
      num_fg_edges = length $ inEdges fg fn
      num_pg_edges = length $ inEdges pg pn
  in if checkNumberOfInEdges fg fn && checkNumberOfInEdges pg pn
        then num_fg_edges == num_pg_edges
        else True

matchingNumberOfOutEdges :: Graph            -- ^ The function graph.
                            -> Graph         -- ^ The pattern graph.
                            -> Matchset Node -- ^ Current matchset state.
                            -> Mapping Node  -- ^ Candidate mapping.
                            -> Bool
matchingNumberOfOutEdges fg pg _ m =
  let fn = fNode m
      pn = pNode m
      num_fg_edges = length $ outEdges fg fn
      num_pg_edges = length $ outEdges pg pn
  in if checkNumberOfOutEdges fg fn && checkNumberOfOutEdges pg pn
        then num_fg_edges == num_pg_edges
        else True

checkNumberOfInEdges :: Graph -> Node -> Bool
checkNumberOfInEdges _ n
  | isActionNode n = True
  | otherwise = False

checkNumberOfOutEdges :: Graph -> Node -> Bool
checkNumberOfOutEdges _ n
  | isActionNode n = True
  | otherwise = False

checkOrderingOfInEdges :: Graph -> Node -> Bool
checkOrderingOfInEdges g n
  | isComputationNode n = not $ O.isOpCommutative $ compOpType $ nodeType n
  | isLabelNode n = (length $ inEdges g n) > 0 && (length $ outEdges g n) > 0
  | isPhiNode n = True
  | otherwise = False

checkOrderingOfOutEdges :: Graph -> Node -> Bool
checkOrderingOfOutEdges _ n
  | isControlNode n = f (nodeType n)
  | otherwise = False
  where f (ControlNode O.CondBranch) = True
        f _ = False

-- | If the pattern node requires an ordering on its inbound edges, check that
-- it is the same as that of the mapped function node.

matchingOrderingOfInEdges :: Graph            -- ^ The function graph.
                             -> Graph         -- ^ The pattern graph.
                             -> Matchset Node -- ^ Current matchset state.
                             -> Mapping Node  -- ^ Candidate mapping.
                             -> Bool
matchingOrderingOfInEdges fg pg st m =
  let fn = fNode m
      pn = pNode m
      pn_preds = filter (`elem` (pNodes st)) (predecessors pg pn)
      fn_preds = mappedNodesPToF st pn_preds
      es = zip (concatMap (flip (edges pg) pn) pn_preds)
               (concatMap (flip (edges fg) fn) fn_preds)
  in if checkOrderingOfInEdges pg pn
        then all (\(e, e') -> (inEdgeNr e) == (inEdgeNr e')) es
        else True

-- | Same as matchingOrderingOfInEdges but for outbound edges.

matchingOrderingOfOutEdges :: Graph            -- ^ The function graph.
                              -> Graph         -- ^ The pattern graph.
                              -> Matchset Node -- ^ Current matchset state.
                              -> Mapping Node  -- ^ Candidate mapping.
                              -> Bool
matchingOrderingOfOutEdges fg pg st m =
  let fn = fNode m
      pn = pNode m
      pn_succs = filter (`elem` (pNodes st)) (successors pg pn)
      fn_succs = mappedNodesPToF st pn_succs
      es = zip (concatMap (edges pg pn) pn_succs)
               (concatMap (edges fg fn) fn_succs)
  in if checkOrderingOfOutEdges pg pn
        then all (\(e, e') -> (outEdgeNr e) == (outEdgeNr e')) es
        else True

-- | Checks for the pattern node's predecessors which have already been mapped
-- and require an ordering on the outbound edges that the ordering is the same
-- as that in the function graph.

matchingOutEdgeOrderingOfPreds :: Graph            -- ^ The function graph.
                                  -> Graph         -- ^ The pattern graph.
                                  -> Matchset Node -- ^ Current matchset state.
                                  -> Mapping Node  -- ^ Candidate mapping.
                                  -> Bool
matchingOutEdgeOrderingOfPreds fg pg st m =
  let fn = fNode m
      pn = pNode m
      pn_preds = filter (`elem` (pNodes st)) (predecessors pg pn)
      pn_ord_preds =  filter (checkOrderingOfOutEdges pg) pn_preds
      fn_ord_preds = mappedNodesPToF st pn_ord_preds
      es = zip (concatMap (flip (edges pg) pn) pn_ord_preds)
               (concatMap (flip (edges fg) fn) fn_ord_preds)
  in all (\(e, e') -> (outEdgeNr e) == (outEdgeNr e')) es

-- | Same as matchingOutEdgeOrderingOfPreds but for successors and their inbound
-- edges.

matchingInEdgeOrderingOfSuccs :: Graph            -- ^ The function graph.
                                 -> Graph         -- ^ The pattern graph.
                                 -> Matchset Node -- ^ Current matchset state.
                                 -> Mapping Node  -- ^ Candidate mapping.
                                 -> Bool
matchingInEdgeOrderingOfSuccs fg pg st m =
  let fn = fNode m
      pn = pNode m
      pn_succs = filter (`elem` (pNodes st)) (successors pg pn)
      pn_ord_succs =  filter (checkOrderingOfInEdges pg) pn_succs
      fn_ord_succs = mappedNodesPToF st pn_ord_succs
      es = zip (concatMap (edges pg pn) pn_ord_succs)
               (concatMap (edges fg fn) fn_ord_succs)
  in all (\(e, e') -> (inEdgeNr e) == (inEdgeNr e')) es

-- | From a matchset and a list of function nodes, get the list of corresponding
-- pattern nodes for which there exists a mapping.

mappedNodesFToP :: (Eq n)
                   => Matchset n -- ^ The matchset.
                   -> [n]        -- ^ List of function nodes.
                   -> [n]        -- ^ List of corresponding pattern nodes.
mappedNodesFToP (Matchset m) fns =
  [ pn | (fn, pn) <- map fromMapping m, fn' <- fns, fn == fn' ]

-- | From a matchset and a list of pattern nodes, get the list of corresponding
-- function nodes for which there exists a mapping.

mappedNodesPToF :: (Eq n)
                   => Matchset n -- ^ The matchset.
                   -> [n]        -- ^ List of pattern nodes.
                   -> [n]        -- ^ List of corresponding function nodes.
mappedNodesPToF (Matchset m) pns =
  [ fn | (fn, pn) <- map fromMapping m, pn' <- pns, pn == pn' ]
