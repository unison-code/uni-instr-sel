--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.Base
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
-- Since there exist operations that are not commutative, such as subtraction,
-- it must be possible to distinguish the operands. This is done by labeling the
-- input edges to a given node. Likewise it may be necessary to be able to
-- distinguish between produced values, and this is handled by labeling the
-- output edges to a given node. Hence every edge is labeled with two integer
-- values, which are called /edge numbers/: one for indicating which output edge
-- it is of the source node, and another for indicating which input edge it is
-- for the destination node. The edge numbers are only per edge type, which
-- means that the same pair of edge numbers can appear in multiple edges for
-- the same node provided that they are of different edge types.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstSel.Graphs.Base
  ( module Language.InstSel.Graphs.IDs
  , BBLabelID (..)
  , Domset (..)
  , DstNode
  , Edge (..)
  , EdgeLabel (..)
  , EdgeType (..)
  , EdgeNr (..)
  , Graph (..)
  , IntGraph
  , Mapping (..)
  , Match (..)
  , Node (..)
  , NodeLabel (..)
  , NodeType (..)
  , SrcNode
  , addNewCFEdge
  , addNewDFEdge
  , addNewDPEdge
  , addNewEdge
  , addNewSFEdge
  , addNewNode
  , convertMappingN2ID
  , convertMatchN2ID
  , copyNodeLabel
  , delEdge
  , delNode
  , delNodeKeepEdges
  , doNodesMatch
  , extractCFG
  , extractDomSet
  , extractIDomSet
  , findFNInMatch
  , findFNsInMatch
  , findPNInMatch
  , findPNsInMatch
  , fromEdgeNr
  , getAllNodes
  , getAllEdges
  , getEdgeType
  , getEdges
  , getInEdgeNr
  , getInEdges
  , getLastAddedNode
  , getNodeID
  , getNodeIDs
  , getNodeWithNodeID
  , getNodesWithNodeID
  , getNodeLabel
  , getNodeType
  , getNumNodes
  , getOutEdgeNr
  , getOutEdges
  , getPredecessors
  , getSourceNode
  , getSuccessors
  , getTargetNode
  , hasAnyPredecessors
  , hasAnySuccessors
  , insertNewNodeAlongEdge
  , isActionNode
  , isComputationNode
  , isControlNode
  , isCopyNode
  , isDataNode
  , isEntityNode
  , isInGraph
  , isLabelNode
  , isOfComputationNodeType
  , isOfControlNodeType
  , isOfCopyNodeType
  , isOfDataNodeType
  , isOfLabelNodeType
  , isOfPhiNodeType
  , isOfStateNodeType
  , isOfTypeConversionNodeType
  , isPhiNode
  , isRetControlNode
  , isStateNode
  , mergeNodes
  , mkEmpty
  , mkGraph
  , redirectEdges
  , redirectInEdges
  , redirectOutEdges
  , rootInCFG
  , sortEdgesByInNumbers
  , sortEdgesByOutNumbers
  , toEdgeNr
  , updateEdgeLabel
  , updateEdgeSource
  , updateEdgeTarget
  , updateNodeID
  , updateNodeLabel
  )
where

import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs.IDs
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachine.IDs
  ( BBLabelID (..) )
import Language.InstSel.Utils
  ( Natural
  , removeDuplicates
  , toNatural
  )
import qualified Data.Graph.Inductive as I
import Data.List
  ( sortBy )
import Data.Maybe



--------------
-- Data types
--------------

type IntGraph = I.Gr NodeLabel EdgeLabel

-- | The outer-most data type which contains the graph itself.
newtype Graph =
    Graph { intGraph :: IntGraph }
  deriving (Show)

-- | Represents a distinct node.
newtype Node =
    Node (I.LNode NodeLabel)
  deriving (Show, Eq)

-- | A synonym for indicating the source node of an edge.
type SrcNode = Node

-- | A synonym for indicating the destination node of an edge.
type DstNode = Node

-- | Node label, consisting of an ID that can be shared by multiple nodes (thus
-- representing that they are actually the same node) and node information which
-- denotes the type of node and other auxiliary information.
data NodeLabel =
    NodeLabel
    { nodeID :: NodeID
    , nodeType :: NodeType
    }
  deriving (Show, Eq)

-- | The node type information.
data NodeType =
    ComputationNode { compOp :: O.CompOp }

  | TypeConversionNode { convOp :: O.TypeConvOp }

  | ControlNode { contOp :: O.ControlOp }

    -- | Temporary and constant nodes (appearing in IR and pattern code), as
    -- well as register and immediate nodes (appearing only in pattern code),
    -- are all represented as data nodes. What distinguishes one from another
    -- are the constraints applied to it.
  | DataNode
    { dataType :: D.DataType
    , dataOrigin :: Maybe String
      -- ^ If the data node represents a particular temporary, variable or
      -- constant which is specified in the source code, then the name of that
      -- entity can be given here as a string. This will only be used for
      -- debugging and pretty-printing purposes.
    }

  | LabelNode { bbLabel :: BBLabelID }

  | PhiNode

  | StateNode

  | CopyNode
  deriving (Show, Eq)

-- | Represents a distinct edge.
newtype Edge =
    Edge (I.LEdge EdgeLabel)
  deriving (Show, Eq)

-- | Data type for describing how an edge relates to the two nodes.
data EdgeLabel =
    EdgeLabel
    { edgeType :: EdgeType
    , outEdgeNr :: EdgeNr
    , inEdgeNr :: EdgeNr
    }
  deriving (Show, Eq)

-- | Data type for determining the edge type.
data EdgeType =
    ControlFlowEdge
  | DataFlowEdge
  | StateFlowEdge
  | DefPlaceEdge
  deriving (Show, Eq)

-- | Edge number, used for ordering edges.
newtype EdgeNr =
    EdgeNr Natural
  deriving (Eq, Ord, Num, Enum)

instance Show EdgeNr where
  show (EdgeNr i) = show i

-- | Represents a mapping between two entities (typically @Node@s or @NodeID@s).
data Mapping n =
    Mapping
    { fNode :: n
      -- ^ The mapped node appearing in the function graph.

    , pNode :: n
      -- ^ The mapped node appearing in the pattern graph.
    }
  deriving (Show, Eq)

-- | Represents a match between two graphs.
newtype Match n = Match [Mapping n]

-- | Represents a dominator set. If the set represents an immediate-dominator
-- set, then only one node will appear in the set of dominated entities.
data Domset t =
    Domset
    { domNode :: t
      -- ^ The dominator entity.

    , domSet :: [t]
      -- ^ The dominated entities.
    }
  deriving (Show)



-------------
-- Functions
-------------

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

isActionNode :: Node -> Bool
isActionNode n =
     isComputationNode n
  || isTypeConversionNode n
  || isControlNode n
  || isPhiNode n
  || isCopyNode n

isEntityNode :: Node -> Bool
isEntityNode n =
     isDataNode n
  || isStateNode n

isComputationNode :: Node -> Bool
isComputationNode n = isOfComputationNodeType $ getNodeType n

isTypeConversionNode :: Node -> Bool
isTypeConversionNode n = isOfTypeConversionNodeType $ getNodeType n

isControlNode :: Node -> Bool
isControlNode n = isOfControlNodeType $ getNodeType n

isRetControlNode :: Node -> Bool
isRetControlNode n = isControlNode n && (contOp $ getNodeType n) == O.Ret

isDataNode :: Node -> Bool
isDataNode n = isOfDataNodeType $ getNodeType n

isLabelNode :: Node -> Bool
isLabelNode n = isOfLabelNodeType $ getNodeType n

isPhiNode :: Node -> Bool
isPhiNode n = isOfPhiNodeType $ getNodeType n

isStateNode :: Node -> Bool
isStateNode n = isOfStateNodeType $ getNodeType n

isCopyNode :: Node -> Bool
isCopyNode n = isOfCopyNodeType $ getNodeType n

isOfComputationNodeType :: NodeType -> Bool
isOfComputationNodeType (ComputationNode _) = True
isOfComputationNodeType _ = False

isOfTypeConversionNodeType :: NodeType -> Bool
isOfTypeConversionNodeType (TypeConversionNode _) = True
isOfTypeConversionNodeType _ = False

isOfControlNodeType :: NodeType -> Bool
isOfControlNodeType (ControlNode _) = True
isOfControlNodeType _ = False

isOfDataNodeType :: NodeType -> Bool
isOfDataNodeType (DataNode _ _) = True
isOfDataNodeType _ = False

isOfLabelNodeType :: NodeType -> Bool
isOfLabelNodeType (LabelNode _) = True
isOfLabelNodeType _ = False

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
getNextIntNodeID :: IntGraph -> I.Node
getNextIntNodeID g =
  let existing_nodes = I.nodes g
  in if length existing_nodes > 0
     then 1 + maximum existing_nodes
     else 0

-- | Gets the next node ID which does not already appear in the graph.
getNextNodeID :: IntGraph -> NodeID
getNextNodeID g = toNodeID $ toInteger $ getNextIntNodeID g

-- | Gets the node ID from a node.
getNodeID :: Node -> NodeID
getNodeID (Node (_, NodeLabel i _)) = i

-- | Gets the node label from a node.
getNodeLabel :: Node -> NodeLabel
getNodeLabel (Node (_, nl)) = nl

-- | Gets the node type from a node.
getNodeType :: Node -> NodeType
getNodeType (Node (_, NodeLabel _ nt)) = nt

-- | Gets the internal node ID from a node.
getIntNodeID :: Node -> I.Node
getIntNodeID (Node (nid, _)) = nid

-- | Gets the number of nodes.
getNumNodes :: Graph -> Int
getNumNodes g = length $ getAllNodes g

-- | Gets a list of all nodes.
getAllNodes :: Graph -> [Node]
getAllNodes (Graph g) = map Node (I.labNodes g)

-- | Deletes a node from the graph. Any edges involving the given node will be
-- removed.
delNode :: Node -> Graph -> Graph
delNode n (Graph g) = Graph (I.delNode (getIntNodeID n) g)

-- | Deletes an edge from the graph.
delEdge :: Edge -> Graph -> Graph
delEdge (Edge e) (Graph g) = Graph (I.delLEdge e g)

-- | Gets a list of nodes with the same node ID.
getNodesWithNodeID :: Graph -> NodeID -> [Node]
getNodesWithNodeID g i = filter ((i ==) . getNodeID) $ getAllNodes g

-- | Updates the node label of an already existing node.
updateNodeLabel ::  NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node (getIntNodeID n, new_label)
  in mkGraph (new_n:all_nodes_but_n) (getAllEdges g)

-- | Updates the node ID of an already existing node.
updateNodeID :: NodeID -> Node -> Graph -> Graph
updateNodeID new_id n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
  in mkGraph
     (Node (getIntNodeID n, NodeLabel new_id (getNodeType n)):all_nodes_but_n)
     (getAllEdges g)

-- | Copies the node label from one node to another node. If the two nodes are
-- actually the same node, nothing happens.
copyNodeLabel ::
     Node
     -- ^ Node to copy label to.
  -> Node
     -- ^ Node to copy label from.
  -> Graph
  -> Graph
copyNodeLabel to_n from_n g
  | (getIntNodeID from_n) == (getIntNodeID to_n) = g
  | otherwise = updateNodeLabel (getNodeLabel from_n) to_n g

-- | Merges two nodes by redirecting the edges to the node to merge to, and then
-- removes the merged node. If the two nodes are actually the same node, nothing
-- happens. Any edges already involving the two nodes will be removed.
mergeNodes ::
     Node
     -- ^ Node to merge with (will be kept).
  -> Node
     -- ^ Node to merge with (will be discarded).
  -> Graph
  -> Graph
mergeNodes n_to_keep n_to_discard g
  | (getIntNodeID n_to_keep) == (getIntNodeID n_to_discard) = g
  | otherwise = let edges_to_ignore = getEdges g n_to_discard n_to_keep
                                      ++
                                      getEdges g n_to_keep n_to_discard
                in delNode
                   n_to_discard
                   ( redirectEdges
                     n_to_keep
                     n_to_discard
                     (foldl (flip delEdge) g edges_to_ignore)
                   )

-- | Redirects all edges involving one node to another node.
redirectEdges ::
     Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectEdges to_n from_n g =
  redirectInEdges to_n from_n (redirectOutEdges to_n from_n g)

-- | Redirects all inbound edges to one node to another node.
redirectInEdges ::
     Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectInEdges to_n from_n g =
  foldr (updateEdgeTarget to_n) g (getInEdges g from_n)

-- | Updates the target of an edge.
updateEdgeTarget ::
     Node
     -- ^ New target.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> Graph
updateEdgeTarget new_trg (Edge e@(src, _, l)) (Graph g) =
  let new_trg_id = getIntNodeID new_trg
      new_e = ( src
              , new_trg_id
              , l { inEdgeNr = getNextInEdgeNr g new_trg_id }
              )
  in Graph (I.insEdge new_e (I.delLEdge e g))

-- | Redirects the outbound edges from one node to another.
redirectOutEdges ::
     Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectOutEdges to_n from_n g =
  foldr (updateEdgeSource to_n) g (getOutEdges g from_n)

-- | Updates the source of an edge.
updateEdgeSource ::
     Node
     -- ^ New source.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> Graph
updateEdgeSource new_src (Edge e@(_, trg, l)) (Graph g) =
  let new_src_id = getIntNodeID new_src
      new_e = ( new_src_id
              , trg
              , l { outEdgeNr = getNextOutEdgeNr g new_src_id }
              )
  in Graph (I.insEdge new_e (I.delLEdge e g))

getNextInEdgeNr :: IntGraph -> I.Node -> EdgeNr
getNextInEdgeNr g int =
  let existing_numbers = map getInEdgeNr (map toEdge (I.inn g int))
  in if length existing_numbers > 0
     then maximum existing_numbers + 1
     else 0

getNextOutEdgeNr :: IntGraph -> I.Node -> EdgeNr
getNextOutEdgeNr g int =
  let existing_numbers = map getOutEdgeNr (map toEdge (I.out g int))
  in if length existing_numbers > 0
     then maximum existing_numbers + 1
     else 0

getEdgeLabel :: Edge -> EdgeLabel
getEdgeLabel (Edge (_, _, l)) = l

getInEdgeNr :: Edge -> EdgeNr
getInEdgeNr = inEdgeNr . getEdgeLabel

getOutEdgeNr :: Edge -> EdgeNr
getOutEdgeNr = outEdgeNr . getEdgeLabel

getEdgeType :: Edge -> EdgeType
getEdgeType = edgeType . getEdgeLabel

-- | Adds a new node of a given node type to a graph, returning both the new
-- graph and the new node.
addNewNode :: NodeType -> Graph -> (Graph, Node)
addNewNode nt (Graph g) =
  let new_n = (getNextIntNodeID g, NodeLabel (getNextNodeID g) nt)
      new_g = Graph (I.insNode new_n g)
  in (new_g, Node new_n)

-- | Adds a new edge between two nodes to the graph, returning both the new
-- graph and the new edge. The edge numberings will be set accordingly.
addNewEdge :: EdgeType -> (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewEdge et (from_n, to_n) (Graph g) =
  let from_n_id = getIntNodeID from_n
      to_n_id = getIntNodeID to_n
      out_edge_nr = getNextOutEdgeNr g from_n_id
      in_edge_nr = getNextInEdgeNr g to_n_id
      new_e = ( from_n_id
              , to_n_id
              , EdgeLabel { edgeType = et
                          , outEdgeNr = out_edge_nr
                          , inEdgeNr = in_edge_nr
                          }
              )
      new_g = Graph (I.insEdge new_e g)
  in (new_g, Edge new_e)

addNewDFEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDFEdge = addNewEdge DataFlowEdge

addNewCFEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewCFEdge = addNewEdge ControlFlowEdge

addNewSFEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewSFEdge = addNewEdge StateFlowEdge

addNewDPEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDPEdge = addNewEdge DefPlaceEdge

-- | Inserts a new node along an existing edge in the graph, returning both the
-- new graph and the new node. The existing edge will be split into two edges
-- which will be connected to the new node. The edge numbers will be retained as
-- appropriate.
insertNewNodeAlongEdge :: NodeType -> Edge -> Graph -> (Graph, Node)
insertNewNodeAlongEdge
  nt
  e@(Edge (from_nid, to_nid, el))
  (Graph g0)
  =
  let ((Graph g1), new_n) = addNewNode nt (Graph g0)
      (Graph g2) = delEdge e (Graph g1)
      et = edgeType el
      new_e1 = (from_nid, getIntNodeID new_n, EdgeLabel et (outEdgeNr el) 0)
      new_e2 = (getIntNodeID new_n, to_nid, EdgeLabel et 0 (inEdgeNr el))
      g3 = I.insEdge new_e1 g2
      g4 = I.insEdge new_e2 g3
  in (Graph g4, new_n)

-- | Updates the edge label of an already existing edge.
updateEdgeLabel :: EdgeLabel -> Edge -> Graph -> Graph
updateEdgeLabel new_label e@(Edge (src, dst, _)) g =
  let all_edges_but_e = filter (/= e) (getAllEdges g)
      new_e = Edge (src, dst, new_label)
  in mkGraph (getAllNodes g) (new_e:all_edges_but_e)

-- | Gets the node that was last added to the graph, which is the node with the
-- highest internal node ID.
getLastAddedNode :: Graph -> Maybe Node
getLastAddedNode (Graph g) =
  let ns = I.nodes g
  in if not $ null ns
     then getNodeWithIntNodeID g (maximum ns)
     else Nothing

-- | Gets the corresponding node from an internal node ID.
getNodeWithIntNodeID :: IntGraph -> I.Node -> Maybe Node
getNodeWithIntNodeID g nid =
  maybe Nothing (\l -> Just (Node (nid, l))) (I.lab g nid)

-- | Gets the predecessors (if any) of a given node. A node A is a predecessor
-- of another node B if there is a directed edge from B to A.
getPredecessors :: Graph -> Node -> [Node]
getPredecessors (Graph g) n =
  map (fromJust . getNodeWithIntNodeID g) (I.pre g (getIntNodeID n))

-- | Gets the successors (if any) of a given node. A node A is a successor of
-- another node B if there is a directed edge from A to B.
getSuccessors :: Graph -> Node -> [Node]
getSuccessors (Graph g) n =
  map (fromJust . getNodeWithIntNodeID g) (I.suc g (getIntNodeID n))

-- | Checks if a given node is within the graph.
isInGraph :: Graph -> Node -> Bool
isInGraph (Graph g) n = isJust $ getNodeWithIntNodeID g (getIntNodeID n)

-- | Gets a list of all edges.
getAllEdges :: Graph -> [Edge]
getAllEdges (Graph g) = map toEdge $ I.labEdges g

-- | Gets all inbound edges to a particular node.
getInEdges :: Graph -> Node -> [Edge]
getInEdges (Graph g) n = map toEdge $ I.inn g (getIntNodeID n)

-- | Gets all outbound edges from a particular node.
getOutEdges :: Graph -> Node -> [Edge]
getOutEdges (Graph g) n = map toEdge $ I.out g (getIntNodeID n)

-- | Gets the edges between two nodes.
getEdges :: Graph -> SrcNode -> DstNode -> [Edge]
getEdges g from_n to_n =
  let out_edges = map fromEdge $ getOutEdges g from_n
      from_id = getIntNodeID from_n
      to_id = getIntNodeID to_n
      es = map
           toEdge
           (filter (\(n1, n2, _) -> from_id == n1 && to_id == n2) out_edges)
  in es

-- | Sorts a list of edges according to their in-edge numbers (in increasing
-- order).
sortEdgesByInNumbers :: [Edge] -> [Edge]
sortEdgesByInNumbers =
  sortBy (\n -> \m -> if getInEdgeNr n < getInEdgeNr m then LT else GT)

-- | Sorts a list of edges according to their out-edge numbers (in increasing
-- order).
sortEdgesByOutNumbers :: [Edge] -> [Edge]
sortEdgesByOutNumbers =
  sortBy (\n -> \m -> if getOutEdgeNr n < getOutEdgeNr m then LT else GT)

-- | Gets the source node of an edge.
getSourceNode :: Graph -> Edge -> Node
getSourceNode (Graph g) (Edge (n, _, _)) = fromJust $ getNodeWithIntNodeID g n

-- | Gets the target node of an edge.
getTargetNode :: Graph -> Edge -> Node
getTargetNode (Graph g) (Edge (_, n, _)) = fromJust $ getNodeWithIntNodeID g n

-- | Gets the nodes that matches a given node ID.
getNodeWithNodeID :: Graph -> NodeID -> [Node]
getNodeWithNodeID g nid = filter (\n -> getNodeID n == nid) (getAllNodes g)

-- | Converts a mapping of nodes into a mapping of node IDs.
convertMappingN2ID :: Mapping Node -> Mapping NodeID
convertMappingN2ID m =
  Mapping { fNode = getNodeID $ fNode m
          , pNode = getNodeID $ pNode m
          }

-- | Converts a match with nodes into a match with node IDs.
convertMatchN2ID :: Match Node -> Match NodeID
convertMatchN2ID (Match ms) = Match (map convertMappingN2ID ms)

-- | Gets the node IDs of a list of nodes. Duplicate node IDs are removed.
getNodeIDs :: [Node] -> [NodeID]
getNodeIDs = removeDuplicates . map getNodeID

-- | Checks if a node matches another node.
doNodesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doNodesMatch fg pg st m =
  doNodeTypesMatch (getNodeType $ fNode m) (getNodeType $ pNode m)
  &&
  doEdgesMatch fg pg st m

-- | Checks if two node types are matching-compatible, meaning that they will
-- yield assembly code which is semantically equivalent.
doNodeTypesMatch :: NodeType -> NodeType -> Bool
doNodeTypesMatch (ComputationNode op1) (ComputationNode op2) =
  O.areComputationsCompatible op1 op2
doNodeTypesMatch (ControlNode op1) (ControlNode op2) = op1 == op2
doNodeTypesMatch (DataNode d1 _) (DataNode d2 _) =
  D.areDataTypesCompatible d1 d2
doNodeTypesMatch (LabelNode _) (LabelNode _) = True
doNodeTypesMatch PhiNode PhiNode = True
doNodeTypesMatch StateNode StateNode = True
doNodeTypesMatch CopyNode CopyNode = True
doNodeTypesMatch _ _ = False

-- | Checks, for cases where it matters, that the nodes have a matching number
-- of edges, and that the already mapped predecessors and successors have a
-- matching edge ordering. At this point it can be assumed that the nodes in the
-- candidate mapping have matching node types.
doEdgesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doEdgesMatch fg pg st m =
  doNumbersOfInEdgesMatch fg pg st m
  &&
  doNumbersOfOutEdgesMatch fg pg st m
  &&
  doOrdersOfInEdgesMatch fg pg st m
  &&
  doOrdersOfOutEdgesMatch fg pg st m
  &&
  doOutEdgeOrdersOfPredsMatch fg pg st m
  &&
  doInEdgeOrdersOfSuccsMatch fg pg st m

doNumbersOfInEdgesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doNumbersOfInEdgesMatch fg pg _ m =
  let fn = fNode m
      pn = pNode m
      num_fg_edges = length $ getInEdges fg fn
      num_pg_edges = length $ getInEdges pg pn
  in if checkNumberOfInEdges fg fn && checkNumberOfInEdges pg pn
     then num_fg_edges == num_pg_edges
     else True

doNumbersOfOutEdgesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doNumbersOfOutEdgesMatch fg pg _ m =
  let fn = fNode m
      pn = pNode m
      num_fg_edges = length $ getOutEdges fg fn
      num_pg_edges = length $ getOutEdges pg pn
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
  | isComputationNode n = not $ O.isOpCommutative $ compOp $ getNodeType n
  | isLabelNode n =
    (length $ getInEdges g n) > 0 && (length $ getOutEdges g n) > 0
  | isPhiNode n = True
  | otherwise = False

checkOrderingOfOutEdges :: Graph -> Node -> Bool
checkOrderingOfOutEdges _ n
  | isControlNode n = f (getNodeType n)
  | otherwise = False
  where f (ControlNode O.CondBranch) = True
        f _ = False

-- | If the pattern node requires an ordering on its inbound edges, check that
-- it is the same as that of the mapped function node.
doOrdersOfInEdgesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doOrdersOfInEdgesMatch fg pg st m =
  let fn = fNode m
      pn = pNode m
      preds_pn = filter (`elem` (map pNode st)) (getPredecessors pg pn)
      preds_fn = mapPs2Fs st preds_pn
      es = zip
           (sortEdgesByInNumbers (concatMap (flip (getEdges pg) pn) preds_pn))
           (sortEdgesByInNumbers (concatMap (flip (getEdges fg) fn) preds_fn))
  in if checkOrderingOfInEdges pg pn
     then all (\(e, e') -> (getInEdgeNr e) == (getInEdgeNr e')) es
     else True

-- | Same as matchingOrderingOfInEdges but for outbound edges.
doOrdersOfOutEdgesMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doOrdersOfOutEdgesMatch fg pg st m =
  let fn = fNode m
      pn = pNode m
      succs_pn = filter (`elem` (map pNode st)) (getSuccessors pg pn)
      succs_fn = mapPs2Fs st succs_pn
      es = zip
           (sortEdgesByOutNumbers (concatMap (getEdges pg pn) succs_pn))
           (sortEdgesByOutNumbers (concatMap (getEdges fg fn) succs_fn))
  in if checkOrderingOfOutEdges pg pn
     then all (\(e, e') -> (getOutEdgeNr e) == (getOutEdgeNr e')) es
     else True

-- | Checks for the pattern node's predecessors which have already been mapped
-- and require an ordering on the outbound edges that the ordering is the same
-- as that in the function graph.
doOutEdgeOrdersOfPredsMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doOutEdgeOrdersOfPredsMatch fg pg st m =
  let fn = fNode m
      pn = pNode m
      preds_pn = filter (`elem` (map pNode st)) (getPredecessors pg pn)
      pn_ord_preds =  filter (checkOrderingOfOutEdges pg) preds_pn
      fn_ord_preds = mapPs2Fs st pn_ord_preds
      es = zip
           ( sortEdgesByOutNumbers
             (concatMap (flip (getEdges pg) pn) pn_ord_preds)
           )
           ( sortEdgesByOutNumbers
             (concatMap (flip (getEdges fg) fn) fn_ord_preds)
           )
  in all (\(e, e') -> (getOutEdgeNr e) == (getOutEdgeNr e')) es

-- | Same as matchingOutEdgeOrderingOfPreds but for successors and their inbound
-- edges.
doInEdgeOrdersOfSuccsMatch ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
doInEdgeOrdersOfSuccsMatch fg pg st m =
  let fn = fNode m
      pn = pNode m
      succs_pn = filter (`elem` (map pNode st)) (getSuccessors pg pn)
      pn_ord_succs =  filter (checkOrderingOfInEdges pg) succs_pn
      fn_ord_succs = mapPs2Fs st pn_ord_succs
      es = zip
           (sortEdgesByInNumbers (concatMap (getEdges pg pn) pn_ord_succs))
           (sortEdgesByInNumbers (concatMap (getEdges fg fn) fn_ord_succs))
  in all (\(e, e') -> (getInEdgeNr e) == (getInEdgeNr e')) es

-- | Same as `mapFs2Ps`.
findPNsInMatch ::
  (Eq n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of function nodes.
  -> [n]
     -- ^ List of corresponding pattern nodes.
findPNsInMatch (Match m) = mapFs2Ps m

-- | Same as `mapPs2Fs`.
findFNsInMatch ::
  (Eq n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of pattern nodes.
  -> [n]
     -- ^ List of corresponding function nodes.
findFNsInMatch (Match m) = mapPs2Fs m

-- | Same as `mapF2P`.
findPNInMatch ::
  (Eq n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> Maybe n
     -- ^ Corresponding pattern node.
findPNInMatch (Match m) = mapF2P m

-- | Same as `mapP2F`.
findFNInMatch ::
  (Eq n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> Maybe n
     -- ^ Corresponding pattern node.
findFNInMatch (Match m) = mapP2F m

-- | From a match and a list of function nodes, get the list of corresponding
-- pattern nodes for which there exists a mapping. The order of the list will be
-- conserved.
mapFs2Ps ::
  (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of function nodes.
  -> [n]
     -- ^ List of corresponding pattern nodes.
mapFs2Ps m fns = mapMaybe (mapF2P m) fns

-- | From a match and a list of pattern nodes, get the list of corresponding
-- function nodes for which there exists a mapping. The order of the list will
-- be conserved.
mapPs2Fs ::
  (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of pattern nodes.
  -> [n]
     -- ^ List of corresponding function nodes.
mapPs2Fs m pns = mapMaybe (mapP2F m) pns

-- | From a mapping state and a function node, get the corresponding pattern
-- node if there exists a such a mapping.
mapF2P ::
  (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> Maybe n
     -- ^ Corresponding pattern node.
mapF2P st fn =
  let found = [ pNode m | m <- st, fn == fNode m ]
  in if length found > 0
     then Just $ head found
     else Nothing

-- | From a mapping state and a pattern node, get the corresponding function
-- node if there exists a such a mapping.
mapP2F ::
  (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> Maybe n
     -- ^ Corresponding function node.
mapP2F st pn =
  let found = [ fNode m | m <- st, pn == pNode m ]
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Gets a list of dominator sets for a given graph and root node.
extractDomSet ::
     Graph
  -> Node
  -> [Domset Node]
extractDomSet (Graph g) n =
  let dom_sets = I.dom g (getIntNodeID n)
      f (n1, ns2) = Domset
                    (fromJust $ getNodeWithIntNodeID g n1)
                    (map (fromJust . getNodeWithIntNodeID g) ns2)
  in map f dom_sets

-- | Gets a list of immediate-dominator mappings for a given graph and root
-- node.
extractIDomSet ::
  Graph
  -> Node
  -> [Domset Node]
extractIDomSet (Graph g) n =
  let idom_maps = I.iDom g (getIntNodeID n)
      f (n1, n2) = Domset
                   (fromJust $ getNodeWithIntNodeID g n1)
                   [(fromJust $ getNodeWithIntNodeID g n2)]
  in map f idom_maps

-- | Extracts the control-flow graph from a graph. If there is no label node in
-- the graph, an empty graph is returned.
extractCFG :: Graph -> Graph
extractCFG g =
  let nodes_to_remove = filter
                        (\n -> not (isLabelNode n || isControlNode n))
                        (getAllNodes g)
      cfg_with_ctrl_nodes = foldl (flip delNode) g nodes_to_remove
      cfg = foldl
            delNodeKeepEdges
            cfg_with_ctrl_nodes
            (filter isControlNode $ getAllNodes cfg_with_ctrl_nodes)
  in cfg

-- | Deletes a node from the graph, and redirects any edges involving the given
-- node such that all outbound edges will become outbound edges of the node's
-- parent. It is assumed the graph has at most one predecessor of the node to
-- remove (if there are more than one predecessor then the edges will be
-- redirected to one of them, but it is undefined which).
delNodeKeepEdges :: Graph -> Node -> Graph
delNodeKeepEdges g n =
  let preds = getPredecessors g n
  in if length preds > 0
     then mergeNodes (head preds) n g
     else delNode n g

-- | Gets the root from a control-flow graph. If there is no root, @Nothing@ is
-- returned. If there is more than one root, an error is produced.
rootInCFG :: Graph -> Maybe Node
rootInCFG g =
  let roots = filter (\n -> length (getPredecessors g n) == 0) (getAllNodes g)
  in if length roots > 0
     then if length roots == 1
          then Just $ head roots
          else error "More than one root in CFG"
     else Nothing

-- | Checks if a given node has any predecessors.
hasAnyPredecessors :: Graph -> Node -> Bool
hasAnyPredecessors g n = length (getPredecessors g n) > 0

-- | Checks if a given node has any successors.
hasAnySuccessors :: Graph -> Node -> Bool
hasAnySuccessors g n = length (getSuccessors g n) > 0
