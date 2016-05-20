--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
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
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.Graphs.Base
  ( DomSet (..)
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
  , addNewEdge
  , addNewEdges
  , addNewCtrlFlowEdge
  , addNewCtrlFlowEdges
  , addNewDtFlowEdge
  , addNewDtFlowEdges
  , addNewDefEdge
  , addNewDefEdges
  , addNewStFlowEdge
  , addNewStFlowEdges
  , addNewNode
  , areInEdgesEquivalent
  , areOutEdgesEquivalent
  , computeDomSets
  , convertDomSetN2ID
  , convertMappingN2ID
  , convertMatchN2ID
  , copyNodeLabel
  , delEdge
  , delNode
  , delNodeKeepEdges
  , doEdgeListsMatch
  , doNodesMatch
  , extractCFG
  , extractSSA
  , findFNInMapping
  , findFNInMatch
  , findFNsInMapping
  , findFNsInMatch
  , findNodesWithNodeID
  , findPNInMapping
  , findPNInMatch
  , findPNsInMapping
  , findPNsInMatch
  , findCallNodesWithName
  , findBlockNodesWithName
  , findValueNodesWithOrigin
  , fromEdgeNr
  , getAllNodes
  , getAllEdges
  , getCtrlFlowInEdges
  , getCtrlFlowOutEdges
  , getDataTypeOfValueNode
  , getDtFlowInEdges
  , getDtFlowOutEdges
  , getDefInEdges
  , getDefOutEdges
  , getStFlowInEdges
  , getStFlowOutEdges
  , getEdgeType
  , getEdgesBetween
  , getEdgeLabel
  , getInEdgeNr
  , getInEdges
  , getLastAddedNode
  , getNeighbors
  , getNodeID
  , getNodeIDs
  , getNodeLabel
  , getNodeType
  , getNumNodes
  , getNameOfCallNode
  , getNameOfBlockNode
  , getOriginOfValueNode
  , getOutEdgeNr
  , getOutEdges
  , getPredecessors
  , getSourceNode
  , getSuccessors
  , getTargetNode
  , hasAnyPredecessors
  , hasAnySuccessors
  , insertNewNodeAlongEdge
  , isCallNode
  , isComputationNode
  , isControlFlowEdge
  , isControlNode
  , isCopyNode
  , isDataFlowEdge
  , isDatumNode
  , isValueNode
  , isValueNodeWithConstValue
  , isValueNodeWithOrigin
  , isDefEdge
  , isInGraph
  , isBlockNode
  , isBlockNodeAndIntermediate
  , isGraphEmpty
  , isNodeInGraph
  , isOperationNode
  , isStateFlowEdge
  , isOfCallNodeType
  , isOfComputationNodeType
  , isOfControlFlowEdgeType
  , isOfControlNodeType
  , isOfCopyNodeType
  , isOfDataFlowEdgeType
  , isOfValueNodeType
  , isOfDefEdgeType
  , isOfBlockNodeType
  , isOfPhiNodeType
  , isOfStateFlowEdgeType
  , isOfStateNodeType
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
  , sortByEdgeNr
  , toEdgeNr
  , fromMatch
  , toMatch
  , subGraph
  , componentsOf
  , isReachableComponent
  , updateDataTypeOfValueNode
  , updateEdgeLabel
  , updateEdgeSource
  , updateEdgeTarget
  , updateNameOfCallNode
  , updateNodeID
  , updateNodeLabel
  , updateNodeType
  , updateOriginOfValueNode
  )
where

import Language.InstrSel.PrettyShow
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Functions.IDs
  ( BlockName
  , FunctionName
  )
import Language.InstrSel.Graphs.IDs
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.JSON

import qualified Data.Graph.Inductive as I

import Data.List
  ( nub
  , nubBy
  , sortBy
  )
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V

import Control.DeepSeq
  ( NFData
  , rnf
  )

import Control.Arrow
  ( first )
import Data.List
  ( unfoldr
  , foldl'
  )



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
  deriving (Show)

instance Ord Node where
  (Node (n1, _)) <= (Node (n2, _)) = n1 <= n2

instance Eq Node where
  (Node (n1, _)) == (Node (n2, _)) = n1 == n2

-- | A synonym for indicating the source node of an edge.
type SrcNode = Node

-- | A synonym for indicating the destination node of an edge.
type DstNode = Node

-- | Node label, consisting of an ID that can be shared by multiple nodes (thus
-- representing that they are actually the same node) and node information which
-- denotes the type of node and other auxiliary information.
data NodeLabel
  = NodeLabel
      { nodeID :: NodeID
      , nodeType :: NodeType
      }
  deriving (Show)

-- | The node type information.
data NodeType
  = ComputationNode { compOp :: O.CompOp }
  | ControlNode { ctrlOp :: O.ControlOp }
  | CallNode { nameOfCall :: FunctionName }
    -- | Temporary and constant nodes (appearing in IR and pattern code), as
    -- well as register and immediate nodes (appearing only in pattern code),
    -- are all represented as value nodes. What distinguishes one from another
    -- are the constraints applied to it.
  | ValueNode
      { typeOfValue :: D.DataType
      , originOfValue :: Maybe String
        -- ^ If the value node represents a particular temporary or variable or
        -- which is specified in the source code, then the name of that item can
        -- be given here as a string. This will only be used for debugging and
        -- pretty-printing purposes.
      }
  | BlockNode { nameOfBlock :: BlockName }
  | PhiNode
  | StateNode
  | CopyNode
  deriving (Show)

instance PrettyShow NodeType where
  pShow (ComputationNode op) = "Computation node (" ++ pShow op ++ ")"
  pShow (ControlNode op) = "Control node (" ++ pShow op ++ ")"
  pShow (CallNode func) = "Call node (" ++ pShow func ++ ")"
  pShow (ValueNode dt origin) = "Value node (" ++ pShow dt ++ ", "
                                ++ pShow origin ++ ")"
  pShow (BlockNode name) = "Block node (" ++ pShow name ++ ")"
  pShow PhiNode = "Phi node"
  pShow StateNode = "State node"
  pShow CopyNode = "Copy node"

-- | Represents a distinct edge.
newtype Edge
  = Edge (I.LEdge EdgeLabel)
  deriving (Show, Eq)

-- | Data type for describing how an edge relates to the two nodes.
data EdgeLabel
  = EdgeLabel
      { edgeType :: EdgeType
      , outEdgeNr :: EdgeNr
      , inEdgeNr :: EdgeNr
      }
  deriving (Show, Eq)

-- | Data type for determining the edge type.
data EdgeType
  = ControlFlowEdge
  | DataFlowEdge
  | StateFlowEdge
  | DefEdge
  deriving (Show, Eq)

-- | Edge number, used for ordering edges.
newtype EdgeNr
  = EdgeNr Natural
  deriving (Show, Eq, Ord, Num, Enum)

instance PrettyShow EdgeNr where
  pShow (EdgeNr i) = pShow i

-- | Represents a mapping between two entities (typically 'Node's or 'NodeID's).
data Mapping n
  = Mapping
      { fNode :: n
        -- ^ The mapped node appearing in the function graph.
      , pNode :: n
        -- ^ The mapped node appearing in the pattern graph.
      }
  deriving (Show, Eq, Ord)

-- | Represents a match between two graphs.
newtype Match n
  = Match (S.Set (Mapping n))
  deriving (Show, Eq)

-- | Represents a dominator set.
data DomSet t
  = DomSet
      { domNode :: t
        -- ^ The item that this dominator set concerns.
      , domSet :: [t]
        -- ^ The items that dominate this item.
      }
  deriving (Show)



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON Graph where
  parseJSON v@(Object _) = Graph <$> parseJSON v
  parseJSON _ = mzero

instance ToJSON Graph where
  toJSON g = toJSON $ intGraph g

instance FromJSON IntGraph where
  parseJSON (Object v) =
    I.mkGraph
      <$> v .: "nodes"
      <*> v .: "edges"
  parseJSON _ = mzero

instance ToJSON IntGraph where
  toJSON g =
    object [ "nodes" .= (I.labNodes g)
           , "edges" .= (I.labEdges g)
           ]

instance FromJSON NodeLabel where
  parseJSON (Object v) =
    NodeLabel
      <$> v .: "id"
      <*> v .: "type"
  parseJSON _ = mzero

instance ToJSON NodeLabel where
  toJSON l =
    object [ "id"   .= (nodeID l)
           , "type" .= (nodeType l)
           ]

instance FromJSON NodeType where
  parseJSON (Object v) =
    do str <- v .: "ntype"
       let typ = unpack str
       case typ of "comp"  -> ComputationNode
                                <$> v .: "op"
                   "ctrl"  -> ControlNode
                                <$> v .: "op"
                   "call"  -> CallNode
                                <$> v .: "func"
                   "data"  -> ValueNode
                                <$> v .: "dtype"
                                <*> v .: "origin"
                   "lab"   -> BlockNode
                                <$> v .: "block-name"
                   "phi"   -> return PhiNode
                   "stat"  -> return StateNode
                   "copy"  -> return CopyNode
                   _       -> mzero
  parseJSON _ = mzero

instance ToJSON NodeType where
  toJSON n@(ComputationNode {}) =
    object [ "ntype" .= String "comp"
           , "op"    .= toJSON (compOp n)
           ]
  toJSON n@(ControlNode {}) =
    object [ "ntype" .= String "ctrl"
           , "op"    .= toJSON (ctrlOp n)
           ]
  toJSON n@(CallNode {}) =
    object [ "ntype" .= String "call"
           , "func"    .= toJSON (nameOfCall n)
           ]
  toJSON n@(ValueNode {}) =
    object [ "ntype"   .= String "data"
           , "dtype"   .= toJSON (typeOfValue n)
           , "origin"  .= toJSON (originOfValue n)
           ]
  toJSON n@(BlockNode {}) =
    object [ "ntype"    .= String "lab"
           , "block-name" .= toJSON (nameOfBlock n)
           ]
  toJSON (PhiNode {}) =
    object [ "ntype" .= String "phi" ]
  toJSON (StateNode {}) =
    object [ "ntype" .= String "stat" ]
  toJSON (CopyNode {}) =
    object [ "ntype" .= String "copy" ]

instance FromJSON EdgeLabel where
  parseJSON (Object v) =
    EdgeLabel
      <$> v .: "etype"
      <*> v .: "out-nr"
      <*> v .: "in-nr"
  parseJSON _ = mzero

instance ToJSON EdgeLabel where
  toJSON l =
    object [ "etype"   .= (edgeType l)
           , "out-nr" .= (outEdgeNr l)
           , "in-nr"  .= (inEdgeNr l)
           ]

instance FromJSON EdgeType where
  parseJSON (String str) =
    case str of "ctrl"  -> return ControlFlowEdge
                "data"  -> return DataFlowEdge
                "stat"  -> return StateFlowEdge
                "def"   -> return DefEdge
                _       -> mzero
  parseJSON _ = mzero

instance ToJSON EdgeType where
  toJSON ControlFlowEdge = "ctrl"
  toJSON DataFlowEdge    = "data"
  toJSON StateFlowEdge   = "stat"
  toJSON DefEdge         = "def"

instance FromJSON EdgeNr where
  parseJSON v = EdgeNr <$> parseJSON v

instance ToJSON EdgeNr where
  toJSON (EdgeNr nr) = toJSON nr

instance FromJSON (DomSet NodeID) where
  parseJSON (Object v) =
    DomSet
      <$> v .: "node"
      <*> v .: "dom-set"
  parseJSON _ = mzero

instance ToJSON (DomSet NodeID) where
  toJSON d =
    object [ "node"        .= (domNode d)
           , "dom-set"     .= (domSet d)
           ]

instance FromJSON (Match NodeID) where
  parseJSON v@(Array _) = Match <$> parseJSON v
  parseJSON _ = mzero

instance ToJSON (Match NodeID) where
  toJSON (Match set) = toJSON set

instance FromJSON (Mapping NodeID) where
  parseJSON v@(Array _) =
    do list <- parseJSON v
       when (length list /= 2) mzero
       return Mapping { fNode = head list
                      , pNode = last list
                      }
  parseJSON _ = mzero

instance ToJSON (Mapping NodeID) where
  toJSON m = Array (V.fromList [toJSON $ fNode m, toJSON $ pNode m])



----------------------------------------
-- DeepSeq-related type class instances
--
-- These are needed to be able to time
-- how long it takes to produce the
-- matchsets
----------------------------------------

instance NFData n => NFData (Mapping n) where
  rnf (Mapping a b) = rnf a `seq` rnf b

instance NFData n => NFData (Match n) where
  rnf (Match a) = rnf a



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

isOperationNode :: Node -> Bool
isOperationNode n =
     isComputationNode n
  || isControlNode n
  || isCallNode n
  || isPhiNode n
  || isCopyNode n

isDatumNode :: Node -> Bool
isDatumNode n =
     isValueNode n
  || isStateNode n

isNodeInGraph :: Graph -> Node -> Bool
isNodeInGraph g n = n `elem` getAllNodes g

isComputationNode :: Node -> Bool
isComputationNode n = isOfComputationNodeType $ getNodeType n

isControlNode :: Node -> Bool
isControlNode n = isOfControlNodeType $ getNodeType n

isCallNode :: Node -> Bool
isCallNode n = isOfCallNodeType $ getNodeType n

isRetControlNode :: Node -> Bool
isRetControlNode n = isControlNode n && (ctrlOp $ getNodeType n) == O.Ret

isValueNode :: Node -> Bool
isValueNode n = isOfValueNodeType $ getNodeType n

isValueNodeWithConstValue :: Node -> Bool
isValueNodeWithConstValue n =
  if isValueNode n
  then D.isTypeAConstValue $ getDataTypeOfValueNode n
  else False

isValueNodeWithOrigin :: Node -> Bool
isValueNodeWithOrigin n =
  if isValueNode n
  then isJust $ originOfValue $ getNodeType n
  else False

getOriginOfValueNode :: Node -> Maybe String
getOriginOfValueNode = originOfValue . getNodeType

getNameOfBlockNode :: Node -> BlockName
getNameOfBlockNode = nameOfBlock . getNodeType

getNameOfCallNode :: Node -> FunctionName
getNameOfCallNode = nameOfCall . getNodeType

isBlockNode :: Node -> Bool
isBlockNode n = isOfBlockNodeType $ getNodeType n

isPhiNode :: Node -> Bool
isPhiNode n = isOfPhiNodeType $ getNodeType n

isStateNode :: Node -> Bool
isStateNode n = isOfStateNodeType $ getNodeType n

isCopyNode :: Node -> Bool
isCopyNode n = isOfCopyNodeType $ getNodeType n

isOfComputationNodeType :: NodeType -> Bool
isOfComputationNodeType (ComputationNode _) = True
isOfComputationNodeType _ = False

isOfCallNodeType :: NodeType -> Bool
isOfCallNodeType (CallNode _) = True
isOfCallNodeType _ = False

isOfControlNodeType :: NodeType -> Bool
isOfControlNodeType (ControlNode _) = True
isOfControlNodeType _ = False

isOfValueNodeType :: NodeType -> Bool
isOfValueNodeType (ValueNode _ _) = True
isOfValueNodeType _ = False

isOfBlockNodeType :: NodeType -> Bool
isOfBlockNodeType (BlockNode _) = True
isOfBlockNodeType _ = False

isOfPhiNodeType :: NodeType -> Bool
isOfPhiNodeType PhiNode = True
isOfPhiNodeType _ = False

isOfStateNodeType :: NodeType -> Bool
isOfStateNodeType StateNode = True
isOfStateNodeType _ = False

isOfCopyNodeType :: NodeType -> Bool
isOfCopyNodeType CopyNode = True
isOfCopyNodeType _ = False

isDataFlowEdge :: Edge -> Bool
isDataFlowEdge = isOfDataFlowEdgeType . getEdgeType

isStateFlowEdge :: Edge -> Bool
isStateFlowEdge = isOfStateFlowEdgeType . getEdgeType

isControlFlowEdge :: Edge -> Bool
isControlFlowEdge = isOfControlFlowEdgeType . getEdgeType

isDefEdge :: Edge -> Bool
isDefEdge = isOfDefEdgeType . getEdgeType

isOfDataFlowEdgeType :: EdgeType -> Bool
isOfDataFlowEdgeType DataFlowEdge = True
isOfDataFlowEdgeType _ = False

isOfControlFlowEdgeType :: EdgeType -> Bool
isOfControlFlowEdgeType ControlFlowEdge = True
isOfControlFlowEdgeType _ = False

isOfStateFlowEdgeType :: EdgeType -> Bool
isOfStateFlowEdgeType StateFlowEdge = True
isOfStateFlowEdgeType _ = False

isOfDefEdgeType :: EdgeType -> Bool
isOfDefEdgeType DefEdge = True
isOfDefEdgeType _ = False

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

-- | Gets the data type from a value node.
getDataTypeOfValueNode :: Node -> D.DataType
getDataTypeOfValueNode n = typeOfValue $ getNodeType n

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
findNodesWithNodeID :: Graph -> NodeID -> [Node]
findNodesWithNodeID g i = filter ((i ==) . getNodeID) $ getAllNodes g

-- | Gets a list of value nodes with the same origin.
findValueNodesWithOrigin :: Graph -> String -> [Node]
findValueNodesWithOrigin g o =
  let vs = filter isValueNodeWithOrigin $ getAllNodes g
  in filter ((==) o . fromJust . getOriginOfValueNode) vs

-- | Gets a list of block nodes with the same block name.
findBlockNodesWithName :: Graph -> BlockName -> [Node]
findBlockNodesWithName g name =
  let bs = filter isBlockNode $ getAllNodes g
  in filter ((==) name . getNameOfBlockNode) bs

-- | Gets a list of call nodes with the same function name.
findCallNodesWithName :: Graph -> FunctionName -> [Node]
findCallNodesWithName g name =
  let bs = filter isCallNode $ getAllNodes g
  in filter ((==) name . getNameOfCallNode) bs

-- | Updates the data type of an already existing value node.
updateDataTypeOfValueNode :: D.DataType -> Node -> Graph -> Graph
updateDataTypeOfValueNode new_dt n g =
  let nt = getNodeType n
      new_nt = nt { typeOfValue = new_dt }
  in case nt of (ValueNode {}) -> updateNodeType new_nt n g
                _ -> error $ "updateDataTypeOfValueNode: node " ++ show n
                             ++ " is not a value node"

-- | Updates the function name of an already existing call node.
updateNameOfCallNode :: FunctionName -> Node -> Graph -> Graph
updateNameOfCallNode new_name n g =
  let nt = getNodeType n
      new_nt = nt { nameOfCall = new_name }
  in case nt of (CallNode {}) -> updateNodeType new_nt n g
                _ -> error $ "updateNameOfCallNode: node " ++ show n
                             ++ " is not a call node"

-- | Updates the origin of an already existing value node.
updateOriginOfValueNode :: String -> Node -> Graph -> Graph
updateOriginOfValueNode new_origin n g =
  let nt = getNodeType n
      new_nt = nt { originOfValue = Just new_origin }
  in case nt of (ValueNode {}) -> updateNodeType new_nt n g
                _ -> error $ "updateOriginOfValueNode: node " ++ show n
                             ++ " is not a value node"

-- | Updates the node label of an already existing node.
updateNodeLabel :: NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node (getIntNodeID n, new_label)
  in mkGraph (new_n:all_nodes_but_n) (getAllEdges g)

-- | Updates the node type of a node.
updateNodeType :: NodeType -> Node -> Graph -> Graph
updateNodeType new_type n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node ( getIntNodeID n
                   , NodeLabel { nodeID = getNodeID n
                               , nodeType = new_type
                               }
                   )
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
copyNodeLabel
  :: Node
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
mergeNodes
  :: Node
     -- ^ Node to merge with (will be kept).
  -> Node
     -- ^ Node to merge with (will be discarded).
  -> Graph
  -> Graph
mergeNodes n_to_keep n_to_discard g
  | (getIntNodeID n_to_keep) == (getIntNodeID n_to_discard) = g
  | otherwise =
      let edges_to_ignore = getEdgesBetween g n_to_discard n_to_keep
                            ++
                            getEdgesBetween g n_to_keep n_to_discard
      in delNode n_to_discard
                 ( redirectEdges n_to_keep
                                 n_to_discard
                                 (foldr delEdge g edges_to_ignore)
                 )

-- | Redirects all edges involving one node to another node.
redirectEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectEdges to_n from_n g =
  redirectInEdges to_n from_n (redirectOutEdges to_n from_n g)

-- | Redirects all inbound edges to one node to another node.
redirectInEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectInEdges to_n from_n g =
  foldr (updateEdgeTarget to_n) g (getInEdges g from_n)

-- | Updates the target of an edge.
updateEdgeTarget
  :: Node
     -- ^ New target.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> Graph
updateEdgeTarget new_trg (Edge e@(src, _, l)) (Graph g) =
  let new_trg_id = getIntNodeID new_trg
      new_e = ( src
              , new_trg_id
              , l
                { inEdgeNr = getNextInEdgeNr
                               g
                               new_trg_id
                               (\e' -> getEdgeType e' == edgeType l)
                }
              )
  in Graph (I.insEdge new_e (I.delLEdge e g))

-- | Redirects the outbound edges from one node to another.
redirectOutEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectOutEdges to_n from_n g =
  foldr (updateEdgeSource to_n) g (getOutEdges g from_n)

-- | Updates the source of an edge.
updateEdgeSource
  :: Node
     -- ^ New source.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> Graph
updateEdgeSource new_src (Edge e@(_, trg, l)) (Graph g) =
  let new_src_id = getIntNodeID new_src
      new_e = ( new_src_id
              , trg
              , l
                { outEdgeNr = getNextOutEdgeNr
                                g
                                new_src_id
                                (\e' -> getEdgeType e' == edgeType l)
                }
              )
  in Graph (I.insEdge new_e (I.delLEdge e g))

-- | Gets the next input edge number to use for a given node, only regarding the
-- edges that pass the user-provided check function. The function can be used to
-- the next edge number for a particular edge type.
getNextInEdgeNr :: IntGraph -> I.Node -> (Edge -> Bool) -> EdgeNr
getNextInEdgeNr g int f =
  let existing_numbers = map getInEdgeNr (filter f $ map toEdge (I.inn g int))
  in if length existing_numbers > 0
     then maximum existing_numbers + 1
     else 0

-- | Gets the next output edge number to use for a given node, only regarding
-- the edges that pass the user-provided check function. The function can be
-- used to the next edge number for a particular edge type.
getNextOutEdgeNr :: IntGraph -> I.Node -> (Edge -> Bool) -> EdgeNr
getNextOutEdgeNr g int f =
  let existing_numbers = map getOutEdgeNr (filter f $ map toEdge (I.out g int))
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
      out_edge_nr = getNextOutEdgeNr g from_n_id (\e -> et == getEdgeType e)
      in_edge_nr = getNextInEdgeNr g to_n_id (\e -> et == getEdgeType e)
      new_e = ( from_n_id
              , to_n_id
              , EdgeLabel { edgeType = et
                          , outEdgeNr = out_edge_nr
                          , inEdgeNr = in_edge_nr
                          }
              )
      new_g = Graph (I.insEdge new_e g)
  in (new_g, Edge new_e)

-- | Adds many new edges between two nodes to the graph. The edges will be
-- inserted in the order of the list, and the edge numberings will be set
-- accordingly.
addNewEdges :: EdgeType -> [(SrcNode, DstNode)] -> Graph -> Graph
addNewEdges et ps g = foldl (\g' p -> fst $ addNewEdge et p g') g ps

addNewDtFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDtFlowEdge = addNewEdge DataFlowEdge

addNewDtFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewDtFlowEdges = addNewEdges DataFlowEdge

addNewCtrlFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewCtrlFlowEdge = addNewEdge ControlFlowEdge

addNewCtrlFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewCtrlFlowEdges = addNewEdges ControlFlowEdge

addNewStFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewStFlowEdge = addNewEdge StateFlowEdge

addNewStFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewStFlowEdges = addNewEdges StateFlowEdge

addNewDefEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDefEdge = addNewEdge DefEdge

addNewDefEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewDefEdges = addNewEdges DefEdge

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

-- | Gets both the predecessors and successors of a given node.
getNeighbors :: Graph -> Node -> [Node]
getNeighbors g n = getPredecessors g n ++ getSuccessors g n

-- | Checks if a given node is within the graph.
isInGraph :: Graph -> Node -> Bool
isInGraph (Graph g) n = isJust $ getNodeWithIntNodeID g (getIntNodeID n)

-- | Gets a list of all edges.
getAllEdges :: Graph -> [Edge]
getAllEdges (Graph g) = map toEdge $ I.labEdges g

-- | Gets all inbound edges (regardless of type) to a particular node.
getInEdges :: Graph -> Node -> [Edge]
getInEdges (Graph g) n = map toEdge $ I.inn g (getIntNodeID n)

-- | Gets all inbound data-flow edges to a particular node.
getDtFlowInEdges :: Graph -> Node -> [Edge]
getDtFlowInEdges g n = filter isDataFlowEdge $ getInEdges g n

-- | Gets all inbound control-flow edges to a particular node.
getCtrlFlowInEdges :: Graph -> Node -> [Edge]
getCtrlFlowInEdges g n = filter isControlFlowEdge $ getInEdges g n

-- | Gets all inbound state flow edges to a particular node.
getStFlowInEdges :: Graph -> Node -> [Edge]
getStFlowInEdges g n = filter isStateFlowEdge $ getInEdges g n

-- | Gets all inbound definition edges to a particular node.
getDefInEdges :: Graph -> Node -> [Edge]
getDefInEdges g n = filter isDefEdge $ getInEdges g n

-- | Gets all outbound edges (regardless of type) from a particular node.
getOutEdges :: Graph -> Node -> [Edge]
getOutEdges (Graph g) n = map toEdge $ I.out g (getIntNodeID n)

-- | Gets all outbound data-flow edges to a particular node.
getDtFlowOutEdges :: Graph -> Node -> [Edge]
getDtFlowOutEdges g n = filter isDataFlowEdge $ getOutEdges g n

-- | Gets all outbound control-flow edges to a particular node.
getCtrlFlowOutEdges :: Graph -> Node -> [Edge]
getCtrlFlowOutEdges g n = filter isControlFlowEdge $ getOutEdges g n

-- | Gets all outbound state flow edges to a particular node.
getStFlowOutEdges :: Graph -> Node -> [Edge]
getStFlowOutEdges g n = filter isStateFlowEdge $ getOutEdges g n

-- | Gets all outbound definition edges to a particular node.
getDefOutEdges :: Graph -> Node -> [Edge]
getDefOutEdges g n = filter isDefEdge $ getOutEdges g n

-- | Gets the edges between two nodes.
getEdgesBetween :: Graph -> SrcNode -> DstNode -> [Edge]
getEdgesBetween g from_n to_n =
  let out_edges = map fromEdge $ getOutEdges g from_n
      from_id = getIntNodeID from_n
      to_id = getIntNodeID to_n
      es = map toEdge
               (filter (\(n1, n2, _) -> from_id == n1 && to_id == n2) out_edges)
  in es

-- | Sorts a list of edges according to their edge numbers (in increasing
-- order), which are provided by a given function.
sortByEdgeNr :: (Edge -> EdgeNr) -> [Edge] -> [Edge]
sortByEdgeNr f = sortBy (\e1 -> \e2 -> if f e1 < f e2 then LT else GT)

-- | Gets the source node of an edge.
getSourceNode :: Graph -> Edge -> Node
getSourceNode (Graph g) (Edge (n, _, _)) = fromJust $ getNodeWithIntNodeID g n

-- | Gets the target node of an edge.
getTargetNode :: Graph -> Edge -> Node
getTargetNode (Graph g) (Edge (_, n, _)) = fromJust $ getNodeWithIntNodeID g n

-- | Converts a dominator set of nodes into a dominator set of node IDs.
convertDomSetN2ID :: DomSet Node -> DomSet NodeID
convertDomSetN2ID d =
  DomSet { domNode = getNodeID $ domNode d
         , domSet = map getNodeID (domSet d)
         }

-- | Converts a mapping of nodes into a mapping of node IDs.
convertMappingN2ID :: Mapping Node -> Mapping NodeID
convertMappingN2ID m =
  Mapping { fNode = getNodeID $ fNode m
          , pNode = getNodeID $ pNode m
          }

-- | Converts a match with nodes into a match with node IDs.
convertMatchN2ID :: Match Node -> Match NodeID
convertMatchN2ID (Match ms) =
  Match (S.fromList $ map convertMappingN2ID (S.toList ms))

-- | Gets the node IDs of a list of nodes. Duplicate node IDs are removed.
getNodeIDs :: [Node] -> [NodeID]
getNodeIDs = nub . map getNodeID

-- | Checks if a node matches another node. Two nodes match if they are of
-- compatible node types and, depending on the node type, they have the same
-- number of edges of a specific edge type.
doNodesMatch
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> Node
     -- ^ A node from the function graph.
  -> Node
     -- ^ A node from the pattern graph.
  -> Bool
doNodesMatch fg pg fn pn =
  (getNodeType pn) `isNodeTypeCompatibleWith` (getNodeType fn)
  &&
  doNumEdgesMatch fg pg fn pn

-- | Checks if a node type is compatible with another node type. Note that this
-- function is not necessarily commutative.
isNodeTypeCompatibleWith :: NodeType -> NodeType -> Bool
isNodeTypeCompatibleWith (ComputationNode op1) (ComputationNode op2) =
  op1 `O.isCompOpCompatibleWith` op2
isNodeTypeCompatibleWith (ControlNode op1) (ControlNode op2) = op1 == op2
isNodeTypeCompatibleWith (CallNode {}) (CallNode {}) = True
isNodeTypeCompatibleWith (ValueNode d1 _) (ValueNode d2 _) =
  d1 `D.isDataTypeCompatibleWith` d2
isNodeTypeCompatibleWith (BlockNode {}) (BlockNode {}) = True
isNodeTypeCompatibleWith PhiNode PhiNode = True
isNodeTypeCompatibleWith StateNode StateNode = True
isNodeTypeCompatibleWith CopyNode CopyNode = True
isNodeTypeCompatibleWith _ _ = False

-- | Checks if a block node is an intermediate block node, meaning that it has
-- at least one in-edge to a control node, and at least one out-edge to another
-- control node.
isBlockNodeAndIntermediate :: Graph -> Node -> Bool
isBlockNodeAndIntermediate g n
  | ( isBlockNode n
      &&
      (length $ filter isControlFlowEdge $ getInEdges g n) > 0
      &&
      (length $ filter isControlFlowEdge $ getOutEdges g n) > 0
    ) = True
  | otherwise = False

-- | Checks if two matching nodes have matching number of edges of particular
-- edge type.
doNumEdgesMatch :: Graph -> Graph -> Node -> Node -> Bool
doNumEdgesMatch fg pg fn pn =
  let checkEdges f getENr es1 es2 =
        let areEdgeNrsSame e1 e2 = getENr e1 == getENr e2
            pruned_es1 = nubBy areEdgeNrsSame $ filter f es1
            pruned_es2 = nubBy areEdgeNrsSame $ filter f es2
        in length pruned_es1 == length pruned_es2
      f_in_es = getInEdges fg fn
      p_in_es = getInEdges pg pn
      f_out_es = getOutEdges fg fn
      p_out_es = getOutEdges pg pn
  in checkEdges (\e -> doesNumCFInEdgesMatter pg pn && isControlFlowEdge e)
                getInEdgeNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumCFOutEdgesMatter pg pn && isControlFlowEdge e)
                getOutEdgeNr
                f_out_es
                p_out_es
     &&
     checkEdges (\e -> doesNumDFInEdgesMatter pg pn && isDataFlowEdge e)
                getInEdgeNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumDFOutEdgesMatter pg pn && isDataFlowEdge e)
                getOutEdgeNr
                f_out_es
                p_out_es
     &&
     checkEdges (\e -> doesNumSFInEdgesMatter pg pn && isStateFlowEdge e)
                getInEdgeNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumSFOutEdgesMatter pg pn && isStateFlowEdge e)
                getOutEdgeNr
                f_out_es
                p_out_es

-- | Checks if the number of control-flow in-edges matters for a given pattern
-- node.
doesNumCFInEdgesMatter :: Graph -> Node -> Bool
doesNumCFInEdgesMatter g n
  | isControlNode n = True
  | isBlockNodeAndIntermediate g n = True
  | otherwise = False

-- | Checks if the number of control-flow out-edges matters for a given pattern
-- node.
doesNumCFOutEdgesMatter :: Graph -> Node -> Bool
doesNumCFOutEdgesMatter _ n
  | isControlNode n = True
  | otherwise = False

-- | Checks if the number of data-flow in-edges matters for a given pattern
-- node.
doesNumDFInEdgesMatter :: Graph -> Node -> Bool
doesNumDFInEdgesMatter _ n
  | isComputationNode n = True
  | isControlNode n = True
  | isCallNode n = True
  | isPhiNode n = True
  | otherwise = False

-- | Checks if the number of data-flow out-edges matters for a given pattern
-- node.
doesNumDFOutEdgesMatter :: Graph -> Node -> Bool
doesNumDFOutEdgesMatter _ n
  | isComputationNode n = True
  | isCallNode n = True
  | otherwise = False

-- | Checks if the number of state flow in-edges matters for a given pattern
-- node.
doesNumSFInEdgesMatter :: Graph -> Node -> Bool
doesNumSFInEdgesMatter _ n
  | isComputationNode n = True
  | isCallNode n = True
  | otherwise = False

-- | Checks if the number of state flow out-edges matters for a given pattern
-- node.
doesNumSFOutEdgesMatter :: Graph -> Node -> Bool
doesNumSFOutEdgesMatter _ n
  | isComputationNode n = True
  | isCallNode n = True
  | otherwise = False

-- | Checks if two lists of edges contain exactly the same edge numbers which
-- are provided by a given function. If the lists do not have the same number of
-- edges, an error is produced.
doEdgeNrsMatch :: (Edge -> EdgeNr) -> [Edge] -> [Edge] -> Bool
doEdgeNrsMatch f es1 es2 =
  if length es1 == length es2
  then all (\(e1, e2) -> (f e1) == (f e2))
           (zip (sortByEdgeNr f es1) (sortByEdgeNr f es2))
  else error "Edge lists not of equal length"

-- | Checks if a list of edges matches another list of edges. It is assumed that
-- the source and target nodes are the same for every edge in each list. It is
-- also assumed that, for each edge type, a in-edge number appears at most once
-- in the list (which should be the case if we are considering matches of
-- patterns on a function graph).
doEdgeListsMatch
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Edge]
     -- ^ In-edges from the function graph.
  -> [Edge]
     -- ^ In-edges from the pattern graph.
  -> Bool
doEdgeListsMatch _ _ [] [] = True
doEdgeListsMatch fg pg fes pes =
  let checkEdgeLengths f = (length $ filter f fes) == (length $ filter f pes)
  in checkEdgeLengths isControlFlowEdge
     &&
     checkEdgeLengths isDataFlowEdge
     &&
     checkEdgeLengths isStateFlowEdge
     &&
     doInEdgeListsMatch fg pg fes pes && doOutEdgeListsMatch fg pg fes pes

-- | Checks if a list of in-edges matches another list of in-edges. It is
-- assumed that the source and target nodes are the same for every edge in each
-- list, and that the lists are non-empty and has the same number of edges per
-- edge type. It is also assumed that, for each edge type, a in-edge number
-- appears at most once in the list (which should be the case if we are
-- considering matches of patterns on a function graph).
doInEdgeListsMatch
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Edge]
     -- ^ In-edges from the function graph.
  -> [Edge]
     -- ^ In-edges from the pattern graph.
  -> Bool
doInEdgeListsMatch _ pg fes pes =
  let checkEdges f = doEdgeNrsMatch getInEdgeNr (filter f fes) (filter f pes)
      pn = getTargetNode pg (head pes)
  in ((not $ doesOrderCFInEdgesMatter pg pn) || checkEdges isControlFlowEdge)
     &&
     ((not $ doesOrderDFInEdgesMatter pg pn) || checkEdges isDataFlowEdge)
     &&
     ((not $ doesOrderSFInEdgesMatter pg pn) || checkEdges isStateFlowEdge)

-- | Same as `doInEdgeListsMatch` but for out-edges.
doOutEdgeListsMatch
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Edge]
     -- ^ In-edges from the function graph.
  -> [Edge]
     -- ^ In-edges from the pattern graph.
  -> Bool
doOutEdgeListsMatch _ pg fes pes =
  let checkEdges f = doEdgeNrsMatch getOutEdgeNr (filter f fes) (filter f pes)
      pn = getSourceNode pg (head pes)
  in ((not $ doesOrderCFOutEdgesMatter pg pn) || checkEdges isControlFlowEdge)
     &&
     ((not $ doesOrderDFOutEdgesMatter pg pn) || checkEdges isDataFlowEdge)
     &&
     ((not $ doesOrderSFOutEdgesMatter pg pn) || checkEdges isStateFlowEdge)

-- | Checks if the order of control-flow in-edges matters for a given pattern
-- node.
doesOrderCFInEdgesMatter :: Graph -> Node -> Bool
doesOrderCFInEdgesMatter g n
  | isBlockNodeAndIntermediate g n = True
  | otherwise = False

-- | Checks if the order of control-flow out-edges matters for a given pattern
-- node.
doesOrderCFOutEdgesMatter :: Graph -> Node -> Bool
doesOrderCFOutEdgesMatter _ n
  | isControlNode n = True
  | otherwise = False

-- | Checks if the order of data-flow in-edges matters for a given pattern
-- node.
doesOrderDFInEdgesMatter :: Graph -> Node -> Bool
doesOrderDFInEdgesMatter _ n
  | isComputationNode n = True
  | isControlNode n = True
  | isPhiNode n = True
  | isCallNode n = True
  | otherwise = False

-- | Checks if the order of data-flow out-edges matters for a given pattern
-- node.
doesOrderDFOutEdgesMatter :: Graph -> Node -> Bool
doesOrderDFOutEdgesMatter _ n
  | isComputationNode n = True
  | otherwise = False

-- | Checks if the order of state flow in-edges matters for a given pattern
-- node.
doesOrderSFInEdgesMatter :: Graph -> Node -> Bool
doesOrderSFInEdgesMatter _ _ = False

-- | Checks if the order of state flow out-edges matters for a given pattern
-- node.
doesOrderSFOutEdgesMatter :: Graph -> Node -> Bool
doesOrderSFOutEdgesMatter _ _ = False

-- | Checks if two in-edges are equivalent, meaning they must be of the same
-- edge type, have target nodes with the same node ID, and have the same in-edge
-- numbers.
areInEdgesEquivalent
  :: Graph
     -- ^ The graph to which the edges belong.
  -> Edge
  -> Edge
  -> Bool
areInEdgesEquivalent g e1 e2 =
  getEdgeType e1 == getEdgeType e2
  &&
  (getNodeID $ getTargetNode g e1) == (getNodeID $ getTargetNode g e2)
  &&
  getInEdgeNr e1 == getInEdgeNr e2

-- | Checks if two out-edges are equivalent, meaning they must be of the same
-- edge type, have source nodes with the same node ID, and have the same
-- out-edge numbers.
areOutEdgesEquivalent
  :: Graph
     -- ^ The graph to which the edges belong.
  -> Edge
  -> Edge
  -> Bool
areOutEdgesEquivalent g e1 e2 =
  getEdgeType e1 == getEdgeType e2
  &&
  (getNodeID $ getSourceNode g e1) == (getNodeID $ getSourceNode g e2)
  &&
  getOutEdgeNr e1 == getOutEdgeNr e2

-- | Same as `findPNsInMapping`.
findPNsInMatch
  :: (Eq n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of function nodes.
  -> [n]
     -- ^ List of corresponding pattern nodes.
findPNsInMatch (Match m) = findPNsInMapping (S.toList m)

-- | Same as `findFNsInMapping`.
findFNsInMatch
  :: (Eq n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of pattern nodes.
  -> [n]
     -- ^ List of corresponding function nodes.
findFNsInMatch (Match m) = findFNsInMapping (S.toList m)

-- | Same as `findPNInMapping`.
findPNInMatch
  :: (Eq n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> Maybe n
     -- ^ Corresponding pattern node.
findPNInMatch (Match m) = findPNInMapping (S.toList m)

-- | Same as `findFNInMapping`.
findFNInMatch
  :: (Eq n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> Maybe n
     -- ^ Corresponding pattern node.
findFNInMatch (Match m) = findFNInMapping (S.toList m)

-- | From a match and a list of function nodes, get the list of corresponding
-- pattern nodes for which there exists a mapping. The order of the list will be
-- conserved.
findPNsInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of function nodes.
  -> [n]
     -- ^ List of corresponding pattern nodes.
findPNsInMapping m fns = mapMaybe (findPNInMapping m) fns

-- | From a match and a list of pattern nodes, get the list of corresponding
-- function nodes for which there exists a mapping. The order of the list will
-- be conserved.
findFNsInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of pattern nodes.
  -> [n]
     -- ^ List of corresponding function nodes.
findFNsInMapping m pns = mapMaybe (findFNInMapping m) pns

-- | From a mapping state and a function node, get the corresponding pattern
-- node if there exists a such a mapping.
findPNInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> Maybe n
     -- ^ Corresponding pattern node.
findPNInMapping st fn =
  let found = [ pNode m | m <- st, fn == fNode m ]
  in if length found > 0
     then Just $ head found
     else Nothing

-- | From a mapping state and a pattern node, get the corresponding function
-- node if there exists a such a mapping.
findFNInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> Maybe n
     -- ^ Corresponding function node.
findFNInMapping st pn =
  let found = [ fNode m | m <- st, pn == pNode m ]
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Computes the dominator sets for a given graph and root node.
computeDomSets :: Graph -> Node -> [DomSet Node]
computeDomSets (Graph g) n =
  let toNode = fromJust . getNodeWithIntNodeID g
      doms = map ( \(n1, ns2) ->
                   DomSet { domNode = toNode n1
                          , domSet = map toNode ns2
                          }
                 )
                 (I.dom g (getIntNodeID n))
  in doms

-- | Checks whether the given graph is empty. A graph is empty if it contains no
-- nodes.
isGraphEmpty :: Graph -> Bool
isGraphEmpty = I.isEmpty . intGraph

-- | Extracts the control-flow graph from a graph. If there are no block nodes
-- in the graph, an empty graph is returned.
extractCFG :: Graph -> Graph
extractCFG g =
  let nodes_to_remove = filter (\n -> not (isBlockNode n || isControlNode n))
                               (getAllNodes g)
      cfg_with_ctrl_nodes = foldr delNode g nodes_to_remove
      cfg = foldr delNodeKeepEdges
                  cfg_with_ctrl_nodes
                  (filter isControlNode $ getAllNodes cfg_with_ctrl_nodes)
  in cfg

-- | Extracts the SSA graph (including nodes which represent data) from a
-- graph. If there are no operation nodes in the graph, an empty graph is
-- returned.
extractSSA :: Graph -> Graph
extractSSA g =
  let nodes_to_remove =
        filter
          ( \n -> not (  isOperationNode n
                      || isDatumNode n
                      )
                  ||
                  (isControlNode n && not (isRetControlNode n))
          )
          (getAllNodes g)
      ssa = foldr delNode g nodes_to_remove
  in ssa

-- | Deletes a node from the graph, and redirects any edges involving the given
-- node such that all outbound edges will become outbound edges of the node's
-- parent. It is assumed the graph has at most one predecessor of the node to
-- remove (if there are more than one predecessor then the edges will be
-- redirected to one of them, but it is undefined which).
delNodeKeepEdges :: Node -> Graph -> Graph
delNodeKeepEdges n g =
  let preds = getPredecessors g n
  in if length preds > 0
     then mergeNodes (head preds) n g
     else delNode n g

-- | Gets the root from a control-flow graph. If there is no root, 'Nothing' is
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

-- | Converts a list of mappings to a match.
toMatch :: Ord n => [Mapping n] -> Match n
toMatch m = Match (S.fromList m)

-- | Converts a match to a list of mappings.
fromMatch :: Ord n => Match n -> [Mapping n]
fromMatch (Match s) = S.toList s

-- | Gives the subgraph induced by a given list of nodes
subGraph :: Graph -> [Node] -> Graph
subGraph g ns =
    let sns = filter (\n -> n `elem` ns) $ getAllNodes g
        ses = filter (\e -> getSourceNode g e `elem` ns &&
                            getTargetNode g e `elem` ns) $
              getAllEdges g
    in mkGraph sns ses

-- | Gives the connected components of the given node.
componentsOf :: Graph -> [Graph]
componentsOf (Graph g) =
    let gs = componentsOf' g
    in map Graph gs

-- TODO: this is taken from Graphalyze

-- | Find all connected components of a graph.
componentsOf' :: (I.DynGraph g) => g a b -> [g a b]
componentsOf' = unfoldr splitComponent

-- | Find the next component and split it off from the graph.
splitComponent :: (I.DynGraph g) => g a b -> Maybe (g a b, g a b)
splitComponent g
    | I.isEmpty g = Nothing
    | otherwise = Just .          -- Get the type right
                  first I.buildGr . -- Create the subgraph
                  extractNode .   -- Extract components of subgraph
                  first Just .    -- Getting the types right
                  I.matchAny $ g    -- Choose an arbitrary node to begin with

-- | Extract the given node and all nodes it is transitively
--   connected to from the graph.
extractNode :: (I.DynGraph g) => I.Decomp g a b -> ([I.Context a b], g a b)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | I.isEmpty gr = ([ctxt], I.empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
      nbrs = I.neighbors' ctxt

-- | Helper function for 'extractNode' above.
nodeExtractor :: (I.DynGraph g) => ([I.Context a b], g a b) -> I.Node
              -> ([I.Context a b], g a b)
nodeExtractor cg@(cs,g) n
    | I.gelem n g = first (++ cs) . extractNode $ I.match n g
    | otherwise = cg

-- | Tests whether there is a path in g from a node in c1 to a node in c2
isReachableComponent :: Graph -> Graph -> Graph -> Bool
isReachableComponent g c1 c2 =
    or [isReachable g n1 n2 | n1 <- getAllNodes c1, n2 <- getAllNodes c2,
                                    n1 /= n2]

-- | Tests whether there is a path in g from a node n1 to a node n2
isReachable :: Graph -> Node -> Node -> Bool
isReachable (Graph g) n1 n2 =
    let rns = I.reachable (getIntNodeID n1) g
    in getIntNodeID n2 `elem` rns
