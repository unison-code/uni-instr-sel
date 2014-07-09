--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Graphs.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.Graphs.Base (
  BBLabel (..)
, Domset (..)
, Edge (..)
, EdgeLabel (..)
, EdgeNr (..)
, Graph (..)
, IntGraph
, Mapping (..)
, Matchset (..)
, Node (..)
, NodeID (..)
, NodeLabel (..)
, NodeType (..)
, addToMatchset
, allNodes
, allEdges
, addNewEdge
, addNewNode
, convertMatchsetN2ID
, convertMappingN2ID
, copyNodeLabel
, delEdge
, delNode
, delNodeKeepEdges
, edges
, mkEmpty
, extractCFG
, extractDomSet
, extractIDomSet
, fNode
, fNodes
, fromEdgeNr
, fromMapping
, fromMatchset
, fromNodeID
, inEdgeNr
, inEdges
, insertNewNodeAlongEdge
, isActionNode
, isComputationNode
, isControlNode
, isCopyNode
, isDataNode
, isEntityNode
, isInGraph
, isInMatchset
, isLabelNode
, isNullNode
, isOfComputationNodeType
, isOfControlNodeType
, isOfCopyNodeType
, isOfDataNodeType
, isOfLabelNodeType
, isOfNullNodeType
, isOfPhiNodeType
, isOfStateNodeType
, isPhiNode
, isRetControlNode
, isStateNode
, lastAddedNode
, mapF2P
, mapP2F
, mapFs2Ps
, mapPs2Fs
, matchingNodes
, mergeNodes
, mkGraph
, nodeID
, nodeIDs
, nodeID2Node
, nodeLabel
, nodesByNodeID
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
, rootInCFG
, sortEdgesByInNumbers
, sortEdgesByOutNumbers
, sourceOfEdge
, successors
, targetOfEdge
, toEdgeNr
, toMapping
, toMatchset
, toNodeID
, updateEdgeLabel
, updateEdgeSource
, updateEdgeTarget
, updateNodeID
, updateNodeLabel
) where

import qualified Language.InstructionSelection.DataTypes as D
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
    = Graph { intGraph :: IntGraph }
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

          NodeID

          -- | Type of node.

          NodeType

    deriving (Show, Eq)

-- | Node ID data type.

newtype NodeID
    = NodeID Natural
    deriving (Eq, Ord, Num, Enum)

instance Show NodeID where
  show (NodeID i) = show i

-- | The node type information.

data NodeType
    = ComputationNode { compOp :: O.CompOp }
    | ControlNode { contOp :: O.ControlOp }

      -- | Temporary and constant nodes (appearing in IR and pattern code), as
      -- well as register and immediate nodes (appearing only in pattern code),
      -- are all represented as data nodes. What distinguishes one from another
      -- are the constraints applied to it.

    | DataNode {

          dataType :: D.DataType

          -- | If the data node represents a particular temporary, variable or
          -- constant which is specified in the source code, then the name of
          -- that entity can be given here as a string. This will only be used
          -- for debugging and pretty-printing purposes.

        , dataOrigin :: Maybe String

      }

    | LabelNode { bbLabel :: BBLabel }
    | PhiNode
    | StateNode
    | CopyNode

      -- | A node which matches any other node, meaning that @n == m@ is always
      -- @True@ if either @n@ or @m@ is of type 'NullNode'. Only to be used
      -- within patterns (like in the generic phi patterns).

    | NullNode

    deriving (Show, Eq)

-- | Represents a distinct edge.

newtype Edge
    = Edge (I.LEdge EdgeLabel)
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

    deriving (Show, Eq)

-- | Edge number, used for ordering edges.

newtype EdgeNr
    = EdgeNr Natural
    deriving (Eq, Ord, Num, Enum)

instance Show EdgeNr where
  show (EdgeNr i) = show i

-- | Represents a basic block label.

newtype BBLabel
    = BBLabel String
    deriving (Eq)

instance Show BBLabel where
  show (BBLabel str) = show str

-- | Represents a matchset, which is a list of mappings.

newtype Matchset n
    = Matchset [Mapping n]
    deriving (Show)

-- | Represents a mapping between two entities (typically @Node@s or @NodeID@s).
-- The first value is the entity in the function, and the second value is the
-- entity in the pattern.

newtype Mapping n
    = Mapping (n, n)
    deriving (Show, Eq)

-- | Represents a dominator set. If the set represents an immediate-dominator
-- set, then only one node will appear in the set of dominated entities.

data Domset t
    = Domset {

          -- | The dominator entity.

          domNode :: t

          -- | The dominated entities.

        , domSet :: [t]

      }
    deriving (Show)





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

fromNodeID :: NodeID -> Natural
fromNodeID (NodeID i) = i

toNodeID :: (Integral i) => i -> NodeID
toNodeID = NodeID . toNatural

fromNode :: Node -> I.LNode NodeLabel
fromNode (Node n) = n

toEdge :: I.LEdge EdgeLabel -> Edge
toEdge = Edge

fromEdge :: Edge -> I.LEdge EdgeLabel
fromEdge (Edge e) = e

toEdgeNr :: (Integral i) => i -> EdgeNr
toEdgeNr = EdgeNr . toNatural

fromEdgeNr :: EdgeNr -> Natural
fromEdgeNr (EdgeNr n) = n

toMatchset :: [Mapping n] -> Matchset n
toMatchset = Matchset

fromMatchset :: Matchset n -> [Mapping n]
fromMatchset (Matchset m) = m

toMapping :: (n, n) -> Mapping n
toMapping = Mapping

fromMapping :: Mapping n -> (n, n)
fromMapping (Mapping m) = m

isActionNode :: Node -> Bool
isActionNode n =
     isComputationNode n
  || isControlNode n
  || isPhiNode n
  || isCopyNode n

isEntityNode :: Node -> Bool
isEntityNode n =
     isDataNode n
  || isStateNode n

isComputationNode :: Node -> Bool
isComputationNode n = isOfComputationNodeType $ nodeType n

isControlNode :: Node -> Bool
isControlNode n = isOfControlNodeType $ nodeType n

isRetControlNode :: Node -> Bool
isRetControlNode n = isControlNode n && (contOp $ nodeType n) == O.Ret

isDataNode :: Node -> Bool
isDataNode n = isOfDataNodeType $ nodeType n

isLabelNode :: Node -> Bool
isLabelNode n = isOfLabelNodeType $ nodeType n

isNullNode :: Node -> Bool
isNullNode n = isOfNullNodeType $ nodeType n

isPhiNode :: Node -> Bool
isPhiNode n = isOfPhiNodeType $ nodeType n

isStateNode :: Node -> Bool
isStateNode n = isOfStateNodeType $ nodeType n

isCopyNode :: Node -> Bool
isCopyNode n = isOfCopyNodeType $ nodeType n

isOfComputationNodeType :: NodeType -> Bool
isOfComputationNodeType (ComputationNode _) = True
isOfComputationNodeType _ = False

isOfControlNodeType :: NodeType -> Bool
isOfControlNodeType (ControlNode _) = True
isOfControlNodeType _ = False

isOfDataNodeType :: NodeType -> Bool
isOfDataNodeType (DataNode _ _) = True
isOfDataNodeType _ = False

isOfLabelNodeType :: NodeType -> Bool
isOfLabelNodeType (LabelNode _) = True
isOfLabelNodeType _ = False

isOfNullNodeType :: NodeType -> Bool
isOfNullNodeType NullNode = True
isOfNullNodeType _ = False

isOfPhiNodeType :: NodeType -> Bool
isOfPhiNodeType PhiNode = True
isOfPhiNodeType _ = False

isOfStateNodeType :: NodeType -> Bool
isOfStateNodeType StateNode = True
isOfStateNodeType _ = False

isOfCopyNodeType :: NodeType -> Bool
isOfCopyNodeType CopyNode = True
isOfCopyNodeType _ = False

-- | Creates an empty graph.

mkEmpty :: Graph
mkEmpty = Graph I.empty

-- | Makes a graph from a list of nodes and edges.

mkGraph :: [Node] -> [Edge] -> Graph
mkGraph ns es = Graph (I.mkGraph (map fromNode ns) (map fromEdge es))

-- | Gets the next internal node ID which does not already appear in the graph.

nextIntNodeID :: IntGraph -> I.Node
nextIntNodeID g =
  let existing_nodes = I.nodes g
  in if length existing_nodes > 0
        then 1 + maximum existing_nodes
        else 0

-- | Gets the next node ID which does not already appear in the graph.

nextNodeID :: IntGraph -> NodeID
nextNodeID g = toNodeID $ toInteger $ nextIntNodeID g

-- | Gets the node ID from a node.

nodeID :: Node -> NodeID
nodeID (Node (_, NodeLabel i _)) = i

-- | Gets the node label from a node.

nodeLabel :: Node -> NodeLabel
nodeLabel (Node (_, nl)) = nl

-- | Gets the node type from a node.

nodeType :: Node -> NodeType
nodeType (Node (_, NodeLabel _ nt)) = nt

-- | Gets the internal node ID from a node.

intNodeID :: Node -> I.Node
intNodeID (Node (nid, _)) = nid

-- | Gets the number of nodes.

numNodes :: Graph -> Int
numNodes g = length $ allNodes g

-- | Gets a list of all nodes.

allNodes :: Graph -> [Node]
allNodes (Graph g) = map Node (I.labNodes g)

-- | Deletes a node from the graph. Any edges involving the given node will be
-- removed.

delNode :: Node -> Graph -> Graph
delNode n (Graph g) = Graph (I.delNode (intNodeID n) g)

-- | Deletes an edge from the graph.

delEdge :: Edge -> Graph -> Graph
delEdge (Edge e) (Graph g) = Graph (I.delLEdge e g)

-- | Gets a list of nodes with the same node ID.

nodesByNodeID :: Graph -> NodeID -> [Node]
nodesByNodeID g i = filter ((i ==) . nodeID) $ allNodes g

-- | Updates the node label of an already existing node.

updateNodeLabel ::  NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
      new_n = Node (intNodeID n, new_label)
  in mkGraph (new_n:all_nodes_but_n) (allEdges g)

-- | Updates the node ID of an already existing node.

updateNodeID :: NodeID -> Node -> Graph -> Graph
updateNodeID new_id n g =
  let all_nodes_but_n = filter (/= n) (allNodes g)
  in mkGraph (Node (intNodeID n, NodeLabel new_id (nodeType n)):all_nodes_but_n)
             (allEdges g)

-- | Copies the node label from one node to another node. If the two nodes are
-- actually the same node, nothing happens.

copyNodeLabel :: Node     -- ^ Node to copy label to.
                 -> Node  -- ^ Node to copy label from.
                 -> Graph
                 -> Graph
copyNodeLabel to_n from_n g
  | (intNodeID from_n) == (intNodeID to_n) = g
  | otherwise = updateNodeLabel (nodeLabel from_n) to_n g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. If the two nodes are actually the same node, nothing
-- happens. Any edges already involving the two nodes will be removed.

mergeNodes :: Node     -- ^ Node to merge with (will be kept).
              -> Node  -- ^ Node to merge with (will be discarded).
              -> Graph
              -> Graph
mergeNodes n_to_keep n_to_discard g
  | (intNodeID n_to_keep) == (intNodeID n_to_discard) = g
  | otherwise = let edges_to_ignore = edges g n_to_discard n_to_keep ++
                                      edges g n_to_keep n_to_discard
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
  let new_target_id = intNodeID new_target
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
  let new_source_id = intNodeID new_source
      new_e = (new_source_id,
               target,
               EdgeLabel (nextOutEdgeNr g new_source_id) in_nr)
  in Graph (I.insEdge new_e $ I.delLEdge e g)

nextInEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextInEdgeNr g int =
  let existing_numbers = map inEdgeNr $ map toEdge $ I.inn g int
  in if length existing_numbers > 0
        then maximum existing_numbers + 1
        else 0

nextOutEdgeNr :: IntGraph -> I.Node -> EdgeNr
nextOutEdgeNr g int =
  let existing_numbers = map outEdgeNr $ map toEdge $ I.out g int
  in if length existing_numbers > 0
        then maximum existing_numbers + 1
        else 0

inEdgeNr :: Edge -> EdgeNr
inEdgeNr (Edge (_, _, EdgeLabel _ nr)) = nr

outEdgeNr :: Edge -> EdgeNr
outEdgeNr (Edge (_, _, EdgeLabel nr _)) = nr

-- | Adds a new node to the graph.

addNewNode :: NodeType -> Graph -> ( Graph -- ^ The new graph.
                                   , Node  -- ^ The newly added node.
                                   )
addNewNode nt (Graph g) =
  let new_n = (nextIntNodeID g, NodeLabel (nextNodeID g) nt)
      new_g = Graph (I.insNode new_n g)
  in (new_g, Node new_n)

-- | Adds a new edge between two nodes to the graph. The edge numberings will be
-- set accordingly.

addNewEdge :: ( Node -- ^ Source node (from).
              , Node -- ^ Destination node (to).
              )
              -> Graph
              -> ( Graph -- ^ The new graph.
                 , Edge  -- ^ The newly added edge.
                 )
addNewEdge (from_n, to_n) (Graph g) =
  let from_node_id = intNodeID from_n
      to_node_id = intNodeID to_n
      out_edge_nr = nextOutEdgeNr g from_node_id
      in_edge_nr = nextInEdgeNr g to_node_id
      new_e = (from_node_id, to_node_id, EdgeLabel out_edge_nr in_edge_nr)
      new_g = Graph (I.insEdge new_e g)
  in (new_g, Edge new_e)

-- | Inserts a new node along an existing edge in the graph. The existing edge
-- will be split into two edges which will be connected to the new node. The
-- edge numbers will be retained as appropriate.

insertNewNodeAlongEdge :: NodeType
                          -> Edge
                          -> Graph
                          -> ( Graph -- ^ The new graph.
                             , Node  -- ^ The new node.
                             )
insertNewNodeAlongEdge nt
                       e@(Edge (from_nid, to_nid, EdgeLabel out_e_nr in_e_nr))
                       (Graph g0) =
  let ((Graph g1), new_n) = addNewNode nt (Graph g0)
      (Graph g2) = delEdge e (Graph g1)
      new_e1 = (from_nid, intNodeID new_n, EdgeLabel out_e_nr 0)
      new_e2 = (intNodeID new_n, to_nid, EdgeLabel 0 in_e_nr)
      g3 = I.insEdge new_e1 g2
      g4 = I.insEdge new_e2 g3
  in (Graph g4, new_n)

-- | Updates the edge label of an already existing edge.

updateEdgeLabel ::  EdgeLabel -> Edge -> Graph -> Graph
updateEdgeLabel new_label e@(Edge (src, dst, _)) g =
  let all_edges_but_e = filter (/= e) (allEdges g)
      new_e = Edge (src, dst, new_label)
  in mkGraph (allNodes g) (new_e:all_edges_but_e)

-- | Gets the node that was last added to the graph, which is the node with the
-- highest internal node ID.

lastAddedNode :: Graph -> Maybe Node
lastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
        then intNodeID2Node g (maximum ns)
        else Nothing

-- | Gets the corresponding node from an internal node ID.

intNodeID2Node :: IntGraph -> I.Node -> Maybe Node
intNodeID2Node g nid = maybe Nothing (\l -> Just (Node (nid, l))) (I.lab g nid)

-- | Gets the predecessors (if any) of a given node. A node A is a predecessor
-- of another node B if there is a directed edge from B to A.

predecessors :: Graph -> Node -> [Node]
predecessors (Graph g) n = map (fromJust . intNodeID2Node g)
                               (I.pre g (intNodeID n))

-- | Gets the successors (if any) of a given node. A node A is a successor of
-- another node B if there is a directed edge from A to B.

successors :: Graph -> Node -> [Node]
successors (Graph g) n =
  map (fromJust . intNodeID2Node g) (I.suc g (intNodeID n))

-- | Checks if a given node is within the graph.

isInGraph :: Graph -> Node -> Bool
isInGraph (Graph g) n = isJust $ intNodeID2Node g (intNodeID n)

-- | Gets a list of all edges.

allEdges :: Graph -> [Edge]
allEdges (Graph g) = map toEdge $ I.labEdges g

-- | Gets all inbound edges to a particular node.

inEdges :: Graph -> Node -> [Edge]
inEdges (Graph g) n = map toEdge $ I.inn g (intNodeID n)

-- | Gets all outbound edges from a particular node.

outEdges :: Graph -> Node -> [Edge]
outEdges (Graph g) n = map toEdge $ I.out g (intNodeID n)

-- | Gets a particular edge or edges between two nodes.

edges :: Graph
        -> Node -- ^ The 'from' node.
        -> Node -- ^ The 'to' node.
        -> [Edge]
edges g from_n to_n =
  let out_edges = map fromEdge $ outEdges g from_n
      from_id = intNodeID from_n
      to_id = intNodeID to_n
      es = map toEdge
           $ filter (\(n1, n2, _) -> from_id == n1 && to_id == n2) out_edges
  in es

-- | Sorts a list of edges according to their in-edge numbers (in increasing
-- order).

sortEdgesByInNumbers :: [Edge] -> [Edge]
sortEdgesByInNumbers =
  sortBy (\n -> \m -> if inEdgeNr n < inEdgeNr m then LT else GT)

-- | Sorts a list of edges according to their out-edge numbers (in increasing
-- order).

sortEdgesByOutNumbers :: [Edge] -> [Edge]
sortEdgesByOutNumbers =
  sortBy (\n -> \m -> if outEdgeNr n < outEdgeNr m then LT else GT)

-- | Gets the source node of an edge.

sourceOfEdge :: Graph -> Edge -> Node
sourceOfEdge (Graph g) (Edge (n, _, _)) = fromJust $ intNodeID2Node g n

-- | Gets the target node of an edge.

targetOfEdge :: Graph -> Edge -> Node
targetOfEdge (Graph g) (Edge (_, n, _)) = fromJust $ intNodeID2Node g n

-- | Gets the nodes that matches a given node ID.

nodeID2Node :: Graph -> NodeID -> [Node]
nodeID2Node g nid = filter (\n -> nodeID n == nid) (allNodes g)

-- | Converts matchset of nodes into a matchset of node IDs. Duplicated entries
-- are removed.

convertMatchsetN2ID :: Matchset Node -> (Matchset NodeID)
convertMatchsetN2ID (Matchset m_nodes) =
  let m_ids = map convertMappingN2ID m_nodes
      m_unique_ids = removeDuplicates m_ids
  in Matchset m_unique_ids

-- | Converts a mapping of nodes into a mapping of node IDs.

convertMappingN2ID :: Mapping Node -> (Mapping NodeID)
convertMappingN2ID m = (Mapping (nodeID $ fNode m, nodeID $ pNode m))

-- | Gets the node IDs of a list of nodes. Duplicate node IDs are removed.

nodeIDs :: [Node] -> [NodeID]
nodeIDs = removeDuplicates . map nodeID

-- | Checks if a node matches another node.

matchingNodes :: Graph         -- ^ The function graph.
              -> Graph         -- ^ The pattern graph.
              -> Matchset Node -- ^ Current matchset state.
              -> Mapping Node  -- ^ Candidate mapping.
              -> Bool
matchingNodes fg pg st m =
     matchingNodeTypes (nodeType $ fNode m) (nodeType $ pNode m)
  && matchingEdges fg pg st m

-- | Checks if two node types are matching-compatible, meaning that they will
-- yield assembly code which is semantically equivalent.

matchingNodeTypes :: NodeType -> NodeType -> Bool
matchingNodeTypes (ComputationNode op1) (ComputationNode op2) =
  O.areComputationsCompatible op1 op2
matchingNodeTypes (ControlNode op1) (ControlNode op2) = op1 == op2
matchingNodeTypes (DataNode d1 _) (DataNode d2 _) =
  D.areDataTypesCompatible d1 d2
matchingNodeTypes (LabelNode _) (LabelNode _) = True
matchingNodeTypes PhiNode PhiNode = True
matchingNodeTypes StateNode StateNode = True
matchingNodeTypes CopyNode CopyNode = True
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
  | isComputationNode n = not $ O.isOpCommutative $ compOp $ nodeType n
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
      preds_pn = filter (`elem` (pNodes st)) (predecessors pg pn)
      preds_fn = mapPs2Fs st preds_pn
      es = zip (sortEdgesByInNumbers (concatMap (flip (edges pg) pn) preds_pn))
               (sortEdgesByInNumbers (concatMap (flip (edges fg) fn) preds_fn))
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
      succs_pn = filter (`elem` (pNodes st)) (successors pg pn)
      succs_fn = mapPs2Fs st succs_pn
      es = zip (sortEdgesByOutNumbers (concatMap (edges pg pn) succs_pn))
               (sortEdgesByOutNumbers (concatMap (edges fg fn) succs_fn))
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
      preds_pn = filter (`elem` (pNodes st)) (predecessors pg pn)
      pn_ord_preds =  filter (checkOrderingOfOutEdges pg) preds_pn
      fn_ord_preds = mapPs2Fs st pn_ord_preds
      es = zip (sortEdgesByOutNumbers
                 (concatMap (flip (edges pg) pn) pn_ord_preds))
               (sortEdgesByOutNumbers
                 (concatMap (flip (edges fg) fn) fn_ord_preds))
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
      succs_pn = filter (`elem` (pNodes st)) (successors pg pn)
      pn_ord_succs =  filter (checkOrderingOfInEdges pg) succs_pn
      fn_ord_succs = mapPs2Fs st pn_ord_succs
      es = zip (sortEdgesByInNumbers (concatMap (edges pg pn) pn_ord_succs))
               (sortEdgesByInNumbers (concatMap (edges fg fn) fn_ord_succs))
  in all (\(e, e') -> (inEdgeNr e) == (inEdgeNr e')) es

-- | From a matchset and a list of function nodes, get the list of corresponding
-- pattern nodes for which there exists a mapping.

mapFs2Ps :: (Eq n)
            => Matchset n -- ^ The matchset.
            -> [n]        -- ^ List of function nodes.
            -> [n]        -- ^ List of corresponding pattern nodes.
mapFs2Ps (Matchset m) fns =
  [ pn | (fn, pn) <- map fromMapping m, fn' <- fns, fn == fn' ]

-- | From a matchset and a list of pattern nodes, get the list of corresponding
-- function nodes for which there exists a mapping.

mapPs2Fs :: (Eq n)
            => Matchset n -- ^ The matchset.
            -> [n]        -- ^ List of pattern nodes.
            -> [n]        -- ^ List of corresponding function nodes.
mapPs2Fs (Matchset m) pns =
  [ fn | (fn, pn) <- map fromMapping m, pn' <- pns, pn == pn' ]

-- | From a matchset and a function node, get the corresponding pattern node if
-- there exists a such a mapping.

mapF2P :: (Eq n)
          => Matchset n -- ^ The matchset.
          -> n          -- ^ Function node.
          -> Maybe n    -- ^ Corresponding pattern node.
mapF2P (Matchset m) fn =
  let found = [ pn | (fn', pn) <- map fromMapping m, fn' == fn ]
  in if length found > 0
        then Just $ head found
        else Nothing

-- | From a matchset and a pattern node, get the corresponding function node if
-- there exists a such a mapping.

mapP2F :: (Eq n)
          => Matchset n -- ^ The matchset.
          -> n          -- ^ Pattern node.
          -> Maybe n    -- ^ Corresponding function node.
mapP2F (Matchset m) pn =
  let found = [ fn | (fn, pn') <- map fromMapping m, pn' == pn ]
  in if length found > 0
        then Just $ head found
        else Nothing

-- | Adds a mapping to an existing matchset.

addToMatchset :: Matchset n -> Mapping n -> Matchset n
addToMatchset (Matchset ms) m = Matchset (ms ++ [m])

-- | Checks if a mapping exists in a given matchset.

isInMatchset :: (Eq n) => Matchset n -> Mapping n -> Bool
isInMatchset (Matchset ms) m = m `elem` ms

-- | Gets a list of dominator sets, given a root node.

extractDomSet :: Graph
                 -> Node      -- ^ The root node.
                 -> [Domset Node]
extractDomSet (Graph g) n =
  let dom_sets = I.dom g (intNodeID n)
      f (n1, ns2) = Domset (fromJust $ intNodeID2Node g n1)
                           (map (fromJust . intNodeID2Node g) ns2)
  in map f dom_sets

-- | Gets a list of immediate-dominator mappings, given a root node.

extractIDomSet :: Graph
                  -> Node    -- ^ The root node.
                  -> [Domset Node]
extractIDomSet (Graph g) n =
  let idom_maps = I.iDom g (intNodeID n)
      f (n1, n2) = Domset (fromJust $ intNodeID2Node g n1)
                          [(fromJust $ intNodeID2Node g n2)]
  in map f idom_maps

-- | Extracts the control-flow graph from a graph. If there is no label node in
-- the graph, an empty graph is returned.

extractCFG :: Graph -> Graph
extractCFG g =
  let nodes_to_remove = filter (\n -> not (isLabelNode n || isControlNode n))
                        $ allNodes g
      cfg_with_ctrl_nodes = foldl (flip delNode) g nodes_to_remove
      cfg = foldl delNodeKeepEdges
                  cfg_with_ctrl_nodes
                  (filter isControlNode $ allNodes cfg_with_ctrl_nodes)
  in cfg

-- | Deletes a node from the graph, and redirects any edges involving the given
-- node such that all outbound edges will become outbound edges of the node's
-- parent. It is assumed the graph has at most one predecessor of the node to
-- remove (if there are more than one predecessor then the edges will be
-- redirected to one of them, but it is undefined which).

delNodeKeepEdges :: Graph -> Node -> Graph
delNodeKeepEdges g n =
  let preds = predecessors g n
  in if length preds > 0
        then mergeNodes (head preds) n g
        else delNode n g

-- | Gets the root from a control-flow graph. If there is no root, @Nothing@ is
-- returned. If there is more than one root, an error is produced.

rootInCFG :: Graph -> Maybe Node
rootInCFG g =
  let roots = filter (\n -> length (predecessors g n) == 0) (allNodes g)
  in if length roots > 0
        then if length roots == 1
                then Just $ head roots
                else error "More than one root in CFG"
        else Nothing
