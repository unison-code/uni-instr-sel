{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

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
  , addMappingToMatch
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
  , addOriginToValueNode
  , areInEdgesEquivalent
  , areOutEdgesEquivalent
  , computeDomSets
  , convertDomSetN2ID
  , convertMappingN2ID
  , convertMatchN2ID
  , copyNodeLabel
  , customPatternMatchingSemanticsCheck
  , delEdge
  , delNode
  , delFNodeInMatch
  , delNodeKeepEdges
  , delPNodeInMatch
  , doEdgeListsMatch
  , doNodesMatch
  , extractCFG
  , extractSSAG
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
  , findDefEdgeOfDtInEdge
  , findDefEdgeOfDtOutEdge
  , getCtrlFlowInEdges
  , getCtrlFlowOutEdges
  , getCopiesOfValue
  , getCopyRelatedValues
  , getDataTypeOfValueNode
  , getDtFlowInEdges
  , getDtFlowOutEdges
  , getDefInEdges
  , getDefOutEdges
  , getStFlowInEdges
  , getStFlowOutEdges
  , getEdgeType
  , getEdges
  , getEdgesBetween
  , getEdgeLabel
  , getEdgeInNr
  , getEdgeOutNr
  , getInEdges
  , getNeighbors
  , getNodeID
  , getNodeLabel
  , getNodeType
  , getNumNodes
  , getNameOfCallNode
  , getNameOfBlockNode
  , getOpOfComputationNode
  , getOriginOfValueNode
  , getOutEdges
  , getPredecessors
  , getSourceNode
  , getSuccessors
  , getTargetNode
  , groupNodesByID
  , hasAnyPredecessors
  , hasAnySuccessors
  , haveSameInEdgeNrs
  , haveSameOutEdgeNrs
  , insertNewNodeAlongEdge
  , isBrControlNode
  , isCondBrControlNode
  , isCallNode
  , isIndirCallNode
  , isComputationNode
  , isControlFlowEdge
  , isControlNode
  , isCopyNode
  , isDataFlowEdge
  , isDatumNode
  , isValueNode
  , isValueNodeWithConstValue
  , isValueNodeWithOrigin
  , isValueNodeWithPointerDataType
  , isDefEdge
  , isInGraph
  , isBlockNode
  , isGraphEmpty
  , isNodeInGraph
  , isOperationNode
  , isStateFlowEdge
  , isOfCallNodeType
  , isOfIndirCallNodeType
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
  , mergeMatches
  , mergeNodes
  , mkEmpty
  , mkGraph
  , redirectEdges
  , redirectInEdges
  , redirectOutEdges
  , redirectEdgesWhen
  , redirectInEdgesWhen
  , redirectOutEdgesWhen
  , rootInCFG
  , sortByEdgeNr
  , toEdgeNr
  , fromMatch
  , toMatch
  , subGraph
  , updateOpOfComputationNode
  , updateDataTypeOfValueNode
  , updateEdgeLabel
  , updateEdgeSource
  , updateEdgeTarget
  , updateEdgeInNr
  , updateEdgeOutNr
  , updateFNodeInMatch
  , updateNameOfCallNode
  , updateNodeID
  , updateNodeLabel
  , updateNodeType
  , updatePNodeInMatch
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
import Language.InstrSel.Utils
  ( groupBy )
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.JSON

import qualified Data.Graph.Inductive as I

import Data.List
  ( nubBy
  , sortBy
  )
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V

import Control.DeepSeq
  ( NFData
  , rnf
  )



--------------
-- Data types
--------------

-- | Alias for the internal graph representation.
type IntGraph = I.Gr NodeLabel EdgeLabel

-- | The outer-most data type which contains the graph itself. It also caches
-- all nodes in a map with node IDs as keys for efficient access.
data Graph
  = Graph { intGraph :: IntGraph
          , intNodeMap :: M.Map NodeID [Node]
          , entryBlockNode :: Maybe Node
          }
  deriving (Show)

-- | Represents a distinct node.
newtype Node
  = Node (I.LNode NodeLabel)
  deriving (Show)

instance Ord Node where
  (Node (n1, _)) <= (Node (n2, _)) = n1 <= n2

instance Eq Node where
  (Node (n1, _)) == (Node (n2, _)) = n1 == n2

instance PrettyShow Node where
  pShow n = "{ID: " ++ pShow (getNodeID n) ++ ", " ++ pShow (getNodeType n) ++
            "}"

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
    -- | An indirect call to a function whose address is provided through a
    -- value node.
  | IndirCallNode
    -- | Temporary and constant nodes (appearing in IR and pattern code), as
    -- well as register and immediate nodes (appearing only in pattern code),
    -- are all represented as value nodes. What distinguishes one from another
    -- are the constraints applied to it.
  | ValueNode
      { typeOfValue :: D.DataType
      , originOfValue :: [String]
        -- ^ If the value node represents a particular temporary or variable or
        -- which is specified in the source code, then the name of that item can
        -- be given here as a string. A value node is allowed to have any number
        -- of origins, but the most current origin should always be placed first
        -- in the list.
      }
  | BlockNode { nameOfBlock :: BlockName }
  | PhiNode
  | StateNode
  | CopyNode
  deriving (Show)

instance PrettyShow NodeType where
  pShow (ComputationNode op) = "computation node (" ++ pShow op ++ ")"
  pShow (ControlNode op) = "control node (" ++ pShow op ++ ")"
  pShow (CallNode func) = "call node (" ++ pShow func ++ ")"
  pShow IndirCallNode = "indirect call node"
  pShow (ValueNode dt origin) = "value node (" ++ pShow dt ++ ", "
                                ++ pShow origin ++ ")"
  pShow (BlockNode name) = "block node (" ++ pShow name ++ ")"
  pShow PhiNode = "phi node"
  pShow StateNode = "state node"
  pShow CopyNode = "copy node"

-- | Represents a distinct edge.
newtype Edge
  = Edge (I.LEdge EdgeLabel)
  deriving (Show, Eq)

instance PrettyShow Edge where
  pShow (Edge (s, t, l)) =
    "<" ++ pShow (edgeType l) ++ ", " ++ pShow s ++
    ": " ++ pShow (outEdgeNr l) ++ " -> " ++ pShow (inEdgeNr l) ++
    ": " ++ pShow t ++ ">"

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

instance PrettyShow EdgeType where
  pShow ControlFlowEdge = "control-flow"
  pShow DataFlowEdge = "data-flow"
  pShow StateFlowEdge = "state-flow"
  pShow DefEdge = "definition"

-- | Edge number, used for ordering edges.
newtype EdgeNr
  = EdgeNr Natural
  deriving (Show, Eq, Ord, Num, Enum, Integral, Real)

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

instance (PrettyShow a) => PrettyShow (Mapping a) where
  -- | The mapping is shown as a tuple, where the 'fNode is the first element
  -- and the 'pNode' is the second element.
  pShow m = pShow (fNode m, pNode m)

-- | Represents a match between a function graph and a pattern graph. Note that
-- it is allowed that a node in the pattern graph may be mapped to multiple
-- nodes in the function graph, and vice versa.
--
-- For efficiency, the mappings are stored in two forms: one as mappings from
-- function nodes to pattern nodes, and another as mappings from pattern nodes
-- to function nodes.
data Match n
  = Match { f2pMaps :: M.Map n [n]
            -- ^ Mappings from function nodes to pattern nodes.
          , p2fMaps :: M.Map n [n]
            -- ^ Mappings from pattern nodes to function nodes.
          }
  deriving (Show, Eq, Ord)

instance (PrettyShow a, Ord a) => PrettyShow (Match a) where
  pShow m = pShow $ fromMatch m

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
  parseJSON (Object v) =
    do g <- v .: "graph"
       entry <- v .:? "entry-block-node"
       let ns = map Node $ I.labNodes g
       return $ Graph { intGraph = g
                      , intNodeMap = M.fromList $ groupNodesByID ns
                      , entryBlockNode = if isJust entry
                                         then Just $ toNode $ fromJust entry
                                         else Nothing
                      }
  parseJSON _ = mzero

instance ToJSON Graph where
  toJSON g =
    let entry = entryBlockNode g
    in object [ "graph" .= (intGraph g)
              , "entry-block-node" .= ( if isJust entry
                                        then Just $ fromNode $ fromJust entry
                                        else Nothing
                                      )
              ]

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
       case typ of "comp"       -> ComputationNode
                                     <$> v .: "op"
                   "ctrl"       -> ControlNode
                                     <$> v .: "op"
                   "call"       -> CallNode
                                     <$> v .: "func"
                   "indir-call" -> return IndirCallNode
                   "data"       -> ValueNode
                                     <$> v .: "dtype"
                                     <*> v .: "origin"
                   "lab"        -> BlockNode
                                     <$> v .: "block-name"
                   "phi"        -> return PhiNode
                   "stat"       -> return StateNode
                   "copy"       -> return CopyNode
                   _            -> mzero
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
  toJSON IndirCallNode =
    object [ "ntype" .= String "indir-call" ]
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
  parseJSON v@(Array _) =
    do list <- parseJSON v
       return $ toMatch list
  parseJSON _ = mzero

instance ToJSON (Match NodeID) where
  toJSON m = toJSON $ fromMatch m

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
  rnf (Match a b) = rnf a `seq` rnf b



-------------
-- Functions
-------------

toNode :: I.LNode NodeLabel -> Node
toNode = Node

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

-- | Checks if a given node is an operation.
isOperationNode :: Node -> Bool
isOperationNode n =
  isComputationNode n ||
  isControlNode n ||
  isCallNode n ||
  isIndirCallNode n ||
  isPhiNode n ||
  isCopyNode n

-- | Checks if a given node is a datum.
isDatumNode :: Node -> Bool
isDatumNode n =
  isValueNode n ||
  isStateNode n

-- | Checks if a node exists inside a graph.
isNodeInGraph :: Graph -> Node -> Bool
isNodeInGraph g n = n `elem` getAllNodes g

-- | Checks if a given node is a computation node.
isComputationNode :: Node -> Bool
isComputationNode n = isOfComputationNodeType $ getNodeType n

-- | Checks if a given node is a control node.
isControlNode :: Node -> Bool
isControlNode n = isOfControlNodeType $ getNodeType n

-- | Checks if a given node is a call node.
isCallNode :: Node -> Bool
isCallNode n = isOfCallNodeType $ getNodeType n

-- | Checks if a given node is an indirect-call node.
isIndirCallNode :: Node -> Bool
isIndirCallNode n = isOfIndirCallNodeType $ getNodeType n

-- | Checks if a given node is a return control node.
isRetControlNode :: Node -> Bool
isRetControlNode n = isControlNode n && (ctrlOp $ getNodeType n) == O.Ret

-- | Checks if a given node is an unconditional-branch control node.
isBrControlNode :: Node -> Bool
isBrControlNode n = isControlNode n && (ctrlOp $ getNodeType n) == O.Br

-- | Checks if a given node is an conditional-branch control node.
isCondBrControlNode :: Node -> Bool
isCondBrControlNode n = isControlNode n && (ctrlOp $ getNodeType n) == O.CondBr

-- | Checks if a given node is a value node.
isValueNode :: Node -> Bool
isValueNode n = isOfValueNodeType $ getNodeType n

-- | Checks if a given node is a value node representing a constant value.
isValueNodeWithConstValue :: Node -> Bool
isValueNodeWithConstValue n =
  if isValueNode n
  then D.isTypeAConstValue $ getDataTypeOfValueNode n
  else False

-- | Checks if a given node is a value node representing a pointer.
isValueNodeWithPointerDataType :: Node -> Bool
isValueNodeWithPointerDataType n =
  if isValueNode n
  then D.isTypeAPointer $ getDataTypeOfValueNode n
  else False

-- | Checks if a given node is a value node whose value has an origin (name) in
-- the source code.
isValueNodeWithOrigin :: Node -> Bool
isValueNodeWithOrigin n =
  if isValueNode n
  then length (originOfValue $ getNodeType n) > 0
  else False

-- | Gets the origin of a given value node. Note that a value may have more
-- than one origin.
getOriginOfValueNode :: Node -> [String]
getOriginOfValueNode = originOfValue . getNodeType

-- | Gets the name of a given block node.
getNameOfBlockNode :: Node -> BlockName
getNameOfBlockNode = nameOfBlock . getNodeType

-- | Gets the name of a given call node.
getNameOfCallNode :: Node -> FunctionName
getNameOfCallNode = nameOfCall . getNodeType

-- | Checks if a given node is a block node.
isBlockNode :: Node -> Bool
isBlockNode n = isOfBlockNodeType $ getNodeType n

-- | Checks if a given node is a phi node.
isPhiNode :: Node -> Bool
isPhiNode n = isOfPhiNodeType $ getNodeType n

-- | Checks if a given node is a state node.
isStateNode :: Node -> Bool
isStateNode n = isOfStateNodeType $ getNodeType n

-- | Checks if a given node is a copy node.
isCopyNode :: Node -> Bool
isCopyNode n = isOfCopyNodeType $ getNodeType n

-- | Checks if a given node type represents a computation node.
isOfComputationNodeType :: NodeType -> Bool
isOfComputationNodeType (ComputationNode _) = True
isOfComputationNodeType _ = False

-- | Checks if a given node type represents a call node.
isOfCallNodeType :: NodeType -> Bool
isOfCallNodeType (CallNode _) = True
isOfCallNodeType _ = False

-- | Checks if a given node type represents an indirect-call node.
isOfIndirCallNodeType :: NodeType -> Bool
isOfIndirCallNodeType IndirCallNode = True
isOfIndirCallNodeType _ = False

-- | Checks if a given node type represents a control node.
isOfControlNodeType :: NodeType -> Bool
isOfControlNodeType (ControlNode _) = True
isOfControlNodeType _ = False

-- | Checks if a given node type represents a value node.
isOfValueNodeType :: NodeType -> Bool
isOfValueNodeType (ValueNode _ _) = True
isOfValueNodeType _ = False

-- | Checks if a given node type represents a block node.
isOfBlockNodeType :: NodeType -> Bool
isOfBlockNodeType (BlockNode _) = True
isOfBlockNodeType _ = False

-- | Checks if a given node type represents a phi node.
isOfPhiNodeType :: NodeType -> Bool
isOfPhiNodeType PhiNode = True
isOfPhiNodeType _ = False

-- | Checks if a given node type represents a state node.
isOfStateNodeType :: NodeType -> Bool
isOfStateNodeType StateNode = True
isOfStateNodeType _ = False

-- | Checks if a given node type represents a copy node.
isOfCopyNodeType :: NodeType -> Bool
isOfCopyNodeType CopyNode = True
isOfCopyNodeType _ = False

-- | Checks if a given edge is a data-flow edge.
isDataFlowEdge :: Edge -> Bool
isDataFlowEdge = isOfDataFlowEdgeType . getEdgeType

-- | Checks if a given edge is a state-flow edge.
isStateFlowEdge :: Edge -> Bool
isStateFlowEdge = isOfStateFlowEdgeType . getEdgeType

-- | Checks if a given edge is a control-flow edge.
isControlFlowEdge :: Edge -> Bool
isControlFlowEdge = isOfControlFlowEdgeType . getEdgeType

-- | Checks if a given edge is a definition edge.
isDefEdge :: Edge -> Bool
isDefEdge = isOfDefEdgeType . getEdgeType

-- | Checks if a given edge type represents a data-flow edge.
isOfDataFlowEdgeType :: EdgeType -> Bool
isOfDataFlowEdgeType DataFlowEdge = True
isOfDataFlowEdgeType _ = False

-- | Checks if a given edge type represents a control-flow edge.
isOfControlFlowEdgeType :: EdgeType -> Bool
isOfControlFlowEdgeType ControlFlowEdge = True
isOfControlFlowEdgeType _ = False

-- | Checks if a given edge type represents a state-flow edge.
isOfStateFlowEdgeType :: EdgeType -> Bool
isOfStateFlowEdgeType StateFlowEdge = True
isOfStateFlowEdgeType _ = False

-- | Checks if a given edge type represents a definition edge.
isOfDefEdgeType :: EdgeType -> Bool
isOfDefEdgeType DefEdge = True
isOfDefEdgeType _ = False

-- | Creates an empty graph.
mkEmpty :: Graph
mkEmpty = Graph { intGraph = I.empty
                , intNodeMap = M.empty
                , entryBlockNode = Nothing
                }

-- | Makes a graph from a list of nodes and edges.
mkGraph :: [Node] -> [Edge] -> Maybe Node -> Graph
mkGraph ns es entry =
  Graph { intGraph = I.mkGraph (map fromNode ns) (map fromEdge es)
        , intNodeMap = M.fromList $ groupNodesByID ns
        , entryBlockNode = entry
        }

-- | Gets the next internal node ID which does not already appear in the graph.
getNextIntNodeID :: IntGraph -> I.Node
getNextIntNodeID g =
  let existing_nodes = I.nodes g
  in if length existing_nodes > 0
     then 1 + maximum existing_nodes
     else 0

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

-- | Gets the operation from a computation node.
getOpOfComputationNode :: Node -> O.CompOp
getOpOfComputationNode n = compOp $ getNodeType n

-- | Gets the internal node ID from a node.
getIntNodeID :: Node -> I.Node
getIntNodeID (Node (nid, _)) = nid

-- | Gets the number of nodes.
getNumNodes :: Graph -> Int
getNumNodes g = length $ getAllNodes g

-- | Gets a list of all nodes.
getAllNodes :: Graph -> [Node]
getAllNodes g = map Node $
                I.labNodes $
                intGraph g

-- | Deletes a node from the graph. Any edges involving the given node will be
-- removed.
delNode :: Node -> Graph -> Graph
delNode n g =
  let new_int_g = I.delNode (getIntNodeID n) (intGraph g)
      new_nmap = M.update ( \ns -> let new_ns = filter (/= n) ns
                                   in if not (null new_ns)
                                      then Just new_ns
                                      else Nothing
                          )
                          (getNodeID n)
                          (intNodeMap g)
      entry = entryBlockNode g
      new_entry = if isJust entry && (fromJust entry) == n
                  then Nothing
                  else entry
  in Graph { intGraph = new_int_g
           , intNodeMap = new_nmap
           , entryBlockNode = new_entry
           }

-- | Deletes an edge from the graph.
delEdge :: Edge -> Graph -> Graph
delEdge (Edge e) g = g { intGraph = I.delLEdge e (intGraph g) }

-- | Gets a list of nodes with the same node ID.
findNodesWithNodeID :: Graph -> NodeID -> [Node]
findNodesWithNodeID g i =
  let ns = M.lookup i (intNodeMap g)
  in if isJust ns then fromJust ns else []

-- | Gets a list of value nodes with the same origin.
findValueNodesWithOrigin :: Graph -> String -> [Node]
findValueNodesWithOrigin g o =
  let vs = filter isValueNodeWithOrigin $ getAllNodes g
  in filter (\v -> o `elem` getOriginOfValueNode v) vs

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
                _ -> error $ "updateDataTypeOfValueNode: node " ++ show n ++
                             " is not a value node"

-- | Updates the function name of an already existing call node.
updateNameOfCallNode :: FunctionName -> Node -> Graph -> Graph
updateNameOfCallNode new_name n g =
  let nt = getNodeType n
      new_nt = nt { nameOfCall = new_name }
  in case nt of (CallNode {}) -> updateNodeType new_nt n g
                _ -> error $ "updateNameOfCallNode: node " ++ show n ++
                             " is not a call node"

-- | Adds a new origin to an already existing value node.
addOriginToValueNode :: String -> Node -> Graph -> Graph
addOriginToValueNode new_origin n g =
  let nt = getNodeType n
      new_nt = nt { originOfValue = (new_origin:originOfValue nt) }
  in case nt of (ValueNode {}) -> updateNodeType new_nt n g
                _ -> error $ "addOriginToValueNode: node " ++ show n ++
                             " is not a value node"

-- | Updates the operation of an already existing computation node.
updateOpOfComputationNode :: O.CompOp -> Node -> Graph -> Graph
updateOpOfComputationNode new_op n g =
  let nt = getNodeType n
      new_nt = nt { compOp = new_op }
  in case nt of (ComputationNode {}) -> updateNodeType new_nt n g
                _ -> error $ "updateOpOfComputationNode: node " ++ show n ++
                             " is not a computation node"

-- | Updates the node label of an already existing node.
updateNodeLabel :: NodeLabel -> Node -> Graph -> Graph
updateNodeLabel new_label n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node (getIntNodeID n, new_label)
      entry = entryBlockNode g
      new_entry = if isJust entry && (fromJust entry) == n
                  then Just new_n
                  else entry
  in mkGraph (new_n:all_nodes_but_n) (getAllEdges g) new_entry

-- | Updates the node type of a node.
updateNodeType :: NodeType -> Node -> Graph -> Graph
updateNodeType new_type n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node ( getIntNodeID n
                   , NodeLabel { nodeID = getNodeID n
                               , nodeType = new_type
                               }
                   )
      entry = entryBlockNode g
      new_entry = if isJust entry && (fromJust entry) == n
                  then Just new_n
                  else entry
  in mkGraph (new_n:all_nodes_but_n) (getAllEdges g) new_entry

-- | Updates the node ID of an already existing node.
updateNodeID :: NodeID -> Node -> Graph -> Graph
updateNodeID new_id n g =
  let all_nodes_but_n = filter (/= n) (getAllNodes g)
      new_n = Node (getIntNodeID n, NodeLabel new_id (getNodeType n))
      entry = entryBlockNode g
      new_entry = if isJust entry && (fromJust entry) == n
                  then Just new_n
                  else entry
  in mkGraph (new_n:all_nodes_but_n) (getAllEdges g) new_entry

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
-- happens. Any edges already involving the two nodes will be removed. Edge
-- number invariants between data-flow and definition edges are maintained.
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
      let edges_to_ignore = getEdgesBetween g n_to_discard n_to_keep ++
                            getEdgesBetween g n_to_keep n_to_discard
      in delNode n_to_discard
                 ( redirectEdges n_to_keep
                                 n_to_discard
                                 (foldr delEdge g edges_to_ignore)
                 )

-- | Redirects all edges involving one node to another node. Edge number
-- invariants between data-flow and definition edges are maintained.
redirectEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectEdges = redirectEdgesWhen (\_ -> True)

-- | Redirects all inbound edges to one node to another node.
redirectInEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectInEdges = redirectInEdgesWhen (\_ -> True)

-- | Redirects the outbound edges from one node to another. Edge number
-- invariants between data-flow and definition edges are maintained.
redirectOutEdges
  :: Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectOutEdges = redirectOutEdgesWhen (\_ -> True)

-- | Same as 'redirectEdges' but takes a predicate for which edges to redirect.
redirectEdgesWhen
  :: (Edge -> Bool)
     -- ^ Predicate.
  -> Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectEdgesWhen p to_n from_n g =
  redirectInEdgesWhen p to_n from_n $
  redirectOutEdgesWhen p to_n from_n g

-- | Same as 'redirectInEdges' but takes a predicate for which edges to
-- redirect.
redirectInEdgesWhen
  :: (Edge -> Bool)
     -- ^ Predicate.
  -> Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectInEdgesWhen p to_n from_n g0 =
  let es = filter p $ getInEdges g0 from_n
      df_def_es =
        if isValueNode from_n
        then map ( \e ->
                   let df_es = filter ( \e' ->
                                        getEdgeInNr e == getEdgeInNr e'
                                      ) $
                               filter isDataFlowEdge $
                               es
                   in if length df_es == 1
                      then (head df_es, e)
                      else if length df_es == 0
                           then error $ "redirectInEdgesWhen: no data-flow " ++
                                        "edge to redirect to redirect that " ++
                                        "matches definition edge " ++ pShow e
                           else error $ "redirectInEdgesWhen: multiple data-" ++
                                        "flow edges to redirect that " ++
                                        "matches definition edge " ++ pShow e
                 ) $
             filter isDefEdge $
             es
        else []
      -- Redirect all edges not related to the definition edges
      g1 = foldr (\e g -> fst $ updateEdgeTarget to_n e g) g0 $
           filter ( \e -> e `notElem` map fst df_def_es &&
                          e `notElem` map snd df_def_es
                  ) $
           es

      -- Redirect data-flow and related definition edge, making sure the edge
      -- numbers are consistent
      (g2, new_df_es) = foldr ( \e (g, new_es) ->
                                let (g', e') = updateEdgeTarget to_n e g
                                in (g', (e':new_es))
                              )
                              (g1, []) $
                        map fst df_def_es
      g3 = foldr ( \(df_e, e) g ->
                   let (g', e') = updateEdgeTarget to_n e g
                       (g'', _) = updateEdgeInNr (getEdgeInNr df_e) e' g'
                   in g''
                 )
                 g2 $
           zip new_df_es $
           map snd df_def_es
  in g3

-- | Same as 'redirectOutEdges' but takes a predicate for which edges to
-- redirect.
redirectOutEdgesWhen
  :: (Edge -> Bool)
     -- ^ Predicate.
  -> Node
     -- ^ Node to redirect edges to.
  -> Node
     -- ^ Node to redirect edges from.
  -> Graph
  -> Graph
redirectOutEdgesWhen p to_n from_n g0 =
  let es = filter p $ getOutEdges g0 from_n
      df_def_es =
        if isValueNode from_n
        then map ( \e ->
                   let df_es = filter ( \e' ->
                                        getEdgeOutNr e == getEdgeOutNr e'
                                      ) $
                               filter isDataFlowEdge $
                               es
                   in if length df_es == 1
                      then (head df_es, e)
                      else if length df_es == 0
                           then error $ "redirectOutEdgesWhen: no data-flow " ++
                                        "edge that to redirect that matches " ++
                                        "definition edge " ++ pShow e
                           else error $ "redirectOutEdgesWhen: multiple " ++
                                        "data-flow edges to redirect that " ++
                                        "matches definition edge " ++ pShow e
                 ) $
             filter isDefEdge $
             es
        else []

      -- Redirect all edges not related to the definition edges
      g1 = foldr (\e g -> fst $ updateEdgeSource to_n e g) g0 $
           filter ( \e -> e `notElem` map fst df_def_es &&
                          e `notElem` map snd df_def_es
                  ) $
           es

      -- Redirect data-flow and related definition edge, making sure the edge
      -- numbers are consistent
      (g2, new_df_es) = foldr ( \e (g, new_es) ->
                                let (g', e') = updateEdgeSource to_n e g
                                in (g', (e':new_es))
                              )
                              (g1, []) $
                        map fst df_def_es
      g3 = foldr ( \(df_e, e) g ->
                   let (g', e') = updateEdgeSource to_n e g
                       (g'', _) = updateEdgeOutNr (getEdgeOutNr df_e) e' g'
                   in g''
                 )
                 g2 $
           zip new_df_es $
           map snd df_def_es
  in g3

-- | Updates the target of an edge. The edge-in number is also set to the next,
-- unused edge number.
updateEdgeTarget
  :: Node
     -- ^ New target.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> (Graph, Edge)
     -- ^ The new graph and the updated edge.
updateEdgeTarget new_trg (Edge e@(src, _, l)) g =
  let int_g = intGraph g
      new_trg_id = getIntNodeID new_trg
      new_e = ( src
              , new_trg_id
              , l { inEdgeNr = getNextInEdgeNr int_g
                                               new_trg_id
                                               ( \e' ->
                                                 getEdgeType e' == edgeType l
                                               )
                  }
              )
  in ( g { intGraph = I.insEdge new_e (I.delLEdge e int_g) }
     , Edge new_e
     )

-- | Updates the source of an edge. The edge-out number is also set to the next,
-- unused edge number.
updateEdgeSource
  :: Node
     -- ^ New source.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> (Graph, Edge)
     -- ^ The new graph and the updated edge.
updateEdgeSource new_src (Edge e@(_, trg, l)) g =
  let int_g = intGraph g
      new_src_id = getIntNodeID new_src
      new_e = ( new_src_id
              , trg
              , l { outEdgeNr = getNextOutEdgeNr int_g
                                                 new_src_id
                                                 ( \e' ->
                                                   getEdgeType e' == edgeType l
                                                 )
                  }
              )
  in ( g { intGraph = I.insEdge new_e (I.delLEdge e int_g) }
     , Edge new_e
     )

-- | Updates the in number of an edge.
updateEdgeInNr
  :: EdgeNr
     -- ^ New number.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> (Graph, Edge)
     -- ^ The new graph and the updated edge.
updateEdgeInNr new_nr e@(Edge (_, _, l)) g =
  let new_l = l { inEdgeNr = new_nr }
  in updateEdgeLabel new_l e g

-- | Updates the out number of an edge.
updateEdgeOutNr
  :: EdgeNr
     -- ^ New number.
  -> Edge
     -- ^ The edge to update.
  -> Graph
  -> (Graph, Edge)
     -- ^ The new graph and the updated edge.
updateEdgeOutNr new_nr e@(Edge (_, _, l)) g =
  let new_l = l { outEdgeNr = new_nr }
  in updateEdgeLabel new_l e g

-- | Gets the next input edge number to use for a given node, only regarding the
-- edges that pass the user-provided check function. The function can be used to
-- the next edge number for a particular edge type.
getNextInEdgeNr :: IntGraph -> I.Node -> (Edge -> Bool) -> EdgeNr
getNextInEdgeNr g int f =
  let existing_numbers = map getEdgeInNr (filter f $ map toEdge (I.inn g int))
  in if length existing_numbers > 0
     then maximum existing_numbers + 1
     else 0

-- | Gets the next output edge number to use for a given node, only regarding
-- the edges that pass the user-provided check function. The function can be
-- used to the next edge number for a particular edge type.
getNextOutEdgeNr :: IntGraph -> I.Node -> (Edge -> Bool) -> EdgeNr
getNextOutEdgeNr g int f =
  let existing_numbers = map getEdgeOutNr (filter f $ map toEdge (I.out g int))
  in if length existing_numbers > 0
     then maximum existing_numbers + 1
     else 0

-- | Gets the edge label from an edge.
getEdgeLabel :: Edge -> EdgeLabel
getEdgeLabel (Edge (_, _, l)) = l

-- | Gets the in-edge number component from an edge.
getEdgeInNr :: Edge -> EdgeNr
getEdgeInNr = inEdgeNr . getEdgeLabel

-- | Gets the out-edge number component from an edge.
getEdgeOutNr :: Edge -> EdgeNr
getEdgeOutNr = outEdgeNr . getEdgeLabel

-- | Gets the edge type from an edge.
getEdgeType :: Edge -> EdgeType
getEdgeType = edgeType . getEdgeLabel

-- | Adds a new node of a given node type to a graph, returning both the new
-- graph and the new node.
addNewNode :: NodeType -> Graph -> (Graph, Node)
addNewNode nt g =
  let int_g = intGraph g
      new_int_id = getNextIntNodeID int_g
      new_id = toNodeID new_int_id
      new_int_n = (new_int_id, NodeLabel new_id nt)
      new_int_g = I.insNode new_int_n int_g
      new_n = Node new_int_n
      new_nmap = M.alter ( \ns -> if isJust ns
                                  then Just $ (new_n:fromJust ns)
                                  else Just $ [new_n]
                         )
                         new_id
                         (intNodeMap g)
  in (g { intGraph = new_int_g, intNodeMap = new_nmap }, new_n)

-- | Adds a new edge between two nodes to the graph, returning both the new
-- graph and the new edge. The edge numberings will be set accordingly.
addNewEdge :: EdgeType -> (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewEdge et (from_n, to_n) g =
  let int_g = intGraph g
      from_n_id = getIntNodeID from_n
      to_n_id = getIntNodeID to_n
      out_edge_nr = getNextOutEdgeNr int_g from_n_id (\e -> et == getEdgeType e)
      in_edge_nr = getNextInEdgeNr int_g to_n_id (\e -> et == getEdgeType e)
      new_e = ( from_n_id
              , to_n_id
              , EdgeLabel { edgeType = et
                          , outEdgeNr = out_edge_nr
                          , inEdgeNr = in_edge_nr
                          }
              )
      new_int_g = I.insEdge new_e int_g
  in (g { intGraph = new_int_g }, Edge new_e)

-- | Adds many new edges between two nodes to the graph. The edges will be
-- inserted in the order of the list, and the edge numberings will be set
-- accordingly.
addNewEdges :: EdgeType -> [(SrcNode, DstNode)] -> Graph -> Graph
addNewEdges et ps g = foldl (\g' p -> fst $ addNewEdge et p g') g ps

-- | Adds a new data-flow edge to the graph.
--
-- @see 'addNewEdge'
addNewDtFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDtFlowEdge = addNewEdge DataFlowEdge

-- | Adds multiple new data-flow edges to the graph.
--
-- @see 'addNewEdges'
addNewDtFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewDtFlowEdges = addNewEdges DataFlowEdge

-- | Adds a new control-flow edge to the graph.
--
-- @see 'addNewEdge'
addNewCtrlFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewCtrlFlowEdge = addNewEdge ControlFlowEdge

-- | Adds multiple new control-flow edges to the graph.
--
-- @see 'addNewEdges'
addNewCtrlFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewCtrlFlowEdges = addNewEdges ControlFlowEdge

-- | Adds a new state-flow edge to the graph.
--
-- @see 'addNewEdge'
addNewStFlowEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewStFlowEdge = addNewEdge StateFlowEdge

-- | Adds multiple new state-flow edges to the graph.
--
-- @see 'addNewEdges'
addNewStFlowEdges :: [(SrcNode, DstNode)] -> Graph -> Graph
addNewStFlowEdges = addNewEdges StateFlowEdge

-- | Adds a new definition edge to the graph.
--
-- @see 'addNewEdge'
addNewDefEdge :: (SrcNode, DstNode) -> Graph -> (Graph, Edge)
addNewDefEdge = addNewEdge DefEdge

-- | Adds multiple new definition edges to the graph.
--
-- @see 'addNewEdges'
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
  g0
  =
  let (g1, new_n) = addNewNode nt g0
      g2 = delEdge e g1
      et = edgeType el
      new_e1 = (from_nid, getIntNodeID new_n, EdgeLabel et (outEdgeNr el) 0)
      new_e2 = (getIntNodeID new_n, to_nid, EdgeLabel et 0 (inEdgeNr el))
      int_g2 = intGraph g2
      int_g3 = I.insEdge new_e2 $
               I.insEdge new_e1 int_g2
      g3 = g2 { intGraph = int_g3 }
  in (g3, new_n)

-- | Updates the edge label of an already existing edge.
updateEdgeLabel
  :: EdgeLabel
  -> Edge
  -> Graph
  -> (Graph, Edge)
     -- ^ The new graph and the updated edge.
updateEdgeLabel new_label e@(Edge (src, dst, _)) g =
  let all_edges_but_e = filter (/= e) (getAllEdges g)
      new_e = Edge (src, dst, new_label)
  in (mkGraph (getAllNodes g) (new_e:all_edges_but_e) (entryBlockNode g), new_e)

-- | Gets the corresponding node from an internal node ID.
getNodeWithIntNodeID :: IntGraph -> I.Node -> Maybe Node
getNodeWithIntNodeID g nid =
  maybe Nothing (\l -> Just (Node (nid, l))) (I.lab g nid)

-- | Gets the predecessors (if any) of a given node. A node @n@ is a predecessor
-- of another node @m@ if there is a directed edge from @m@ to @n@.
getPredecessors :: Graph -> Node -> [Node]
getPredecessors g n =
  let int_g = intGraph g
  in map (fromJust . getNodeWithIntNodeID int_g) (I.pre int_g (getIntNodeID n))

-- | Gets the successors (if any) of a given node. A node @n@ is a successor of
-- another node @m@ if there is a directed edge from @n@ to @m@.
getSuccessors :: Graph -> Node -> [Node]
getSuccessors g n =
  let int_g = intGraph g
  in map (fromJust . getNodeWithIntNodeID int_g) (I.suc int_g (getIntNodeID n))

-- | Gets both the predecessors and successors of a given node.
getNeighbors :: Graph -> Node -> [Node]
getNeighbors g n = getPredecessors g n ++ getSuccessors g n

-- | Checks if a given node is within the graph.
isInGraph :: Graph -> Node -> Bool
isInGraph g n = isJust $ getNodeWithIntNodeID (intGraph g) (getIntNodeID n)

-- | Gets a list of all edges.
getAllEdges :: Graph -> [Edge]
getAllEdges g = map toEdge $ I.labEdges (intGraph g)

-- | Gets all inbound edges (regardless of type) to a particular node.
getInEdges :: Graph -> Node -> [Edge]
getInEdges g n = map toEdge $ I.inn (intGraph g) (getIntNodeID n)

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
getOutEdges g n = map toEdge $ I.out (intGraph g) (getIntNodeID n)

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

-- | Gets the edges involving a given node.
getEdges :: Graph -> Node -> [Edge]
getEdges g n =
  filter (\e -> getSourceNode g e == n || getTargetNode g e == n) $
  getAllEdges g

-- | Gets the edges between two nodes.
getEdgesBetween :: Graph -> SrcNode -> DstNode -> [Edge]
getEdgesBetween g from_n to_n =
  let out_edges = map fromEdge $ getOutEdges g from_n
      from_id = getIntNodeID from_n
      to_id = getIntNodeID to_n
      es = map toEdge $
           filter (\(n1, n2, _) -> from_id == n1 && to_id == n2) $
           out_edges
  in es

-- | Sorts a list of edges according to their edge numbers (in increasing
-- order), which are provided by a given function.
sortByEdgeNr :: (Edge -> EdgeNr) -> [Edge] -> [Edge]
sortByEdgeNr f = sortBy (\e1 -> \e2 -> if f e1 < f e2 then LT else GT)

-- | Gets the source node of an edge.
getSourceNode :: Graph -> Edge -> Node
getSourceNode g (Edge (n, _, _)) =
  fromJust $ getNodeWithIntNodeID (intGraph g) n

-- | Gets the target node of an edge.
getTargetNode :: Graph -> Edge -> Node
getTargetNode g (Edge (_, n, _)) =
  fromJust $ getNodeWithIntNodeID (intGraph g) n

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
convertMatchN2ID m =
  let convert k a = M.insert (getNodeID k) (map getNodeID a)
  in Match { f2pMaps = M.foldrWithKey convert M.empty (f2pMaps m)
           , p2fMaps = M.foldrWithKey convert M.empty (p2fMaps m)
           }

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
  (getNodeType pn) `isNodeTypeCompatibleWith` (getNodeType fn) &&
  doNumEdgesMatch fg pg fn pn

-- | Checks if a node type is compatible with another node type. Note that this
-- function is not necessarily commutative.
isNodeTypeCompatibleWith :: NodeType -> NodeType -> Bool
isNodeTypeCompatibleWith (ComputationNode op1) (ComputationNode op2) =
  op1 `O.isCompatibleWith` op2
isNodeTypeCompatibleWith (ControlNode op1) (ControlNode op2) =
  op1 `O.isCompatibleWith` op2
isNodeTypeCompatibleWith (CallNode {}) (CallNode {}) = True
isNodeTypeCompatibleWith IndirCallNode IndirCallNode = True
isNodeTypeCompatibleWith (ValueNode d1 _) (ValueNode d2 _) =
  d1 `D.isCompatibleWith` d2
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
  | ( isBlockNode n &&
      ( not ( isJust (entryBlockNode g) &&
              n == fromJust (entryBlockNode g)
            )
      ) &&
      (length $ getCtrlFlowInEdges g n) > 0 &&
      (length $ getCtrlFlowOutEdges g n) > 0
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
                getEdgeInNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumCFOutEdgesMatter pg pn && isControlFlowEdge e)
                getEdgeOutNr
                f_out_es
                p_out_es
     &&
     checkEdges (\e -> doesNumDFInEdgesMatter pg pn && isDataFlowEdge e)
                getEdgeInNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumDFOutEdgesMatter pg pn && isDataFlowEdge e)
                getEdgeOutNr
                f_out_es
                p_out_es
     &&
     checkEdges (\e -> doesNumSFInEdgesMatter pg pn && isStateFlowEdge e)
                getEdgeInNr
                f_in_es
                p_in_es
     &&
     checkEdges (\e -> doesNumSFOutEdgesMatter pg pn && isStateFlowEdge e)
                getEdgeOutNr
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
doesNumDFInEdgesMatter g n
  | isOperationNode n = True
  | isValueNode n = (length $ getDtFlowInEdges g n) > 0
  | otherwise = False

-- | Checks if the number of data-flow out-edges matters for a given pattern
-- node.
doesNumDFOutEdgesMatter :: Graph -> Node -> Bool
doesNumDFOutEdgesMatter _ n
  | isOperationNode n = True
  | otherwise = False

-- | Checks if the number of state flow in-edges matters for a given pattern
-- node.
doesNumSFInEdgesMatter :: Graph -> Node -> Bool
doesNumSFInEdgesMatter _ n
  | isOperationNode n = True
  | otherwise = False

-- | Checks if the number of state flow out-edges matters for a given pattern
-- node.
doesNumSFOutEdgesMatter :: Graph -> Node -> Bool
doesNumSFOutEdgesMatter _ n
  | isOperationNode n = True
  | otherwise = False

-- | Checks if every edge in a list of edges from the pattern graph has at least
-- one edge in a list of edges from the function graph with matching edge number
-- (which is retrieved from a predicate function). It is assumed that each edge
-- number in the list from the pattern graph appears at most once.
doEdgeNrsMatch
  :: (Edge -> EdgeNr)
  -> [Edge]
     -- ^ In-edges from the function graph.
  -> [Edge]
     -- ^ In-edges from the pattern graph.
  -> Bool
doEdgeNrsMatch f es1 es2 =
  all (\e -> length (filter (\e' -> f e == f e') es1) > 0) es2

-- | Checks if a list of edges from the pattern graph matches a list of edges
-- from the function graph. It is assumed that the source and target nodes are
-- the same for every edge in each list. The lists match if all edges from the
-- pattern graph have a corresponding edge in the list from the function graph
-- (hence it is allowed that the latter list contain edges that have no
-- corresponding edge in the former list).
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
  doInEdgeListsMatch fg pg fes pes && doOutEdgeListsMatch fg pg fes pes

-- | Checks if a list of in-edges from the pattern graph matches list of
-- in-edges from the function graph. It is assumed that the source and target
-- nodes are the same for every edge in each list.
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
  let checkEdges f = doEdgeNrsMatch getEdgeInNr (filter f fes) (filter f pes)
      pn = getTargetNode pg (head pes)
  in (not (doesOrderCFInEdgesMatter pg pn) || checkEdges isControlFlowEdge) &&
     (not (doesOrderDFInEdgesMatter pg pn) || checkEdges isDataFlowEdge) &&
     (not (doesOrderSFInEdgesMatter pg pn) || checkEdges isStateFlowEdge)

-- | Same as 'doInEdgeListsMatch' but for out-edges.
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
  let checkEdges f = doEdgeNrsMatch getEdgeOutNr (filter f fes) (filter f pes)
      pn = getSourceNode pg (head pes)
  in (not (doesOrderCFOutEdgesMatter pg pn) || checkEdges isControlFlowEdge) &&
     (not (doesOrderDFOutEdgesMatter pg pn) || checkEdges isDataFlowEdge) &&
     (not (doesOrderSFOutEdgesMatter pg pn) || checkEdges isStateFlowEdge)

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
  | isComputationNode n = not $ O.isCommutative $ getOpOfComputationNode n
  | isOperationNode n = True
  | otherwise = False

-- | Checks if the order of data-flow out-edges matters for a given pattern
-- node.
doesOrderDFOutEdgesMatter :: Graph -> Node -> Bool
doesOrderDFOutEdgesMatter _ n
  | isOperationNode n = True
  | otherwise = False

-- | Checks if the order of state flow in-edges matters for a given pattern
-- node.
doesOrderSFInEdgesMatter :: Graph -> Node -> Bool
doesOrderSFInEdgesMatter _ _ = False

-- | Checks if the order of state flow out-edges matters for a given pattern
-- node.
doesOrderSFOutEdgesMatter :: Graph -> Node -> Bool
doesOrderSFOutEdgesMatter _ _ = False

-- | If the pattern contains phi nodes, check that there is a matching
-- definition edge for each value-phi and phi-value edge.
customPatternMatchingSemanticsCheck
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
customPatternMatchingSemanticsCheck fg pg st c =
  let pn = pNode c
  in if isPhiNode pn
     then let es = filter isDataFlowEdge $ getInEdges pg pn
              val_es = filter (isValueNode . getSourceNode pg) es
          in all (checkPhiValBlockMappings fg pg (c:st)) val_es
     else if isValueNode pn
          then let es = filter isDataFlowEdge $ getOutEdges pg pn
                   phi_es = filter (isPhiNode . getTargetNode pg) es
               in all (checkPhiValBlockMappings fg pg (c:st)) phi_es
          else if isBlockNode pn
               then let es = filter isDefEdge $ getInEdges pg pn
                        v_ns = map (getSourceNode pg) es
               in all ( \n ->
                        let es' = filter isDataFlowEdge $ getOutEdges pg n
                            phi_es = filter (isPhiNode . getTargetNode pg) es'
                        in all (checkPhiValBlockMappings fg pg (c:st)) phi_es
                      ) v_ns
               else True

-- | For a given data-flow edge between a phi node and a value node in the
-- pattern graph, check that the function graph has a matching definition edge.
checkPhiValBlockMappings
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state together with the candidate mapping.
  -> Edge
     -- ^ The pattern data-flow edge between the phi node and the value node to
     -- check.
  -> Bool
checkPhiValBlockMappings fg pg st pe =
  let findSingleFNInSt pn =
        let n = findFNInMapping st pn
        in if length n == 1
           then Just $ head n
           else if length n == 0
                then Nothing
                else error $ "checkPhiValBlockMappings: multiple mappings " ++
                             "for pattern node " ++ show pn
      v_pn = getSourceNode pg pe
      v_fn = findSingleFNInSt v_pn
      p_pn = getTargetNode pg pe
      p_fn = findSingleFNInSt p_pn
      def_pes = filter (haveSameOutEdgeNrs pe) $
                filter isDefEdge $
                getOutEdges pg v_pn
      def_pe = if length def_pes == 1
               then head def_pes
               else if length def_pes == 0
                    then error $ "checkPhiValBlockMappings: data-flow edge " ++
                                 show pe ++ " in pattern graph has no " ++
                                 " matching definition edge"
                    else error $ "checkPhiValBlockMappings: data-flow edge " ++
                                 show pe ++ " in pattern graph has more " ++
                                 "than one matching definition edge"
      b_pn = getTargetNode pg def_pe
      b_fn = findSingleFNInSt b_pn
  in if isJust p_fn && isJust v_fn && isJust b_fn
        -- Check if all necessary nodes have been mapped
     then let df_fes = filter isDataFlowEdge $
                       getEdgesBetween fg (fromJust v_fn) (fromJust p_fn)
              hasMatchingDefEdge fe =
                let def_fes = filter (haveSameOutEdgeNrs fe) $
                              filter isDefEdge $
                              getOutEdges fg (getSourceNode fg fe)
                in if length def_fes == 1
                   then getTargetNode fg (head def_fes) == fromJust b_fn
                   else False
          in any hasMatchingDefEdge df_fes
     else True

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
  getEdgeType e1 == getEdgeType e2 &&
  (getNodeID $ getTargetNode g e1) == (getNodeID $ getTargetNode g e2) &&
  getEdgeInNr e1 == getEdgeInNr e2

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
  getEdgeType e1 == getEdgeType e2 &&
  (getNodeID $ getSourceNode g e1) == (getNodeID $ getSourceNode g e2) &&
  getEdgeOutNr e1 == getEdgeOutNr e2

-- | Same as 'findPNsInMapping'.
findPNsInMatch
  :: (Eq n, Ord n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of function nodes.
  -> [[n]]
     -- ^ List of corresponding pattern nodes.
findPNsInMatch m ns = map (findPNInMatch m) ns

-- | Same as 'findFNsInMapping'.
findFNsInMatch
  :: (Eq n, Ord n)
  => Match n
     -- ^ The match.
  -> [n]
     -- ^ List of pattern nodes.
  -> [[n]]
     -- ^ List of corresponding function nodes.
findFNsInMatch m ns = map (findFNInMatch m) ns

-- | Same as 'findPNInMapping'.
findPNInMatch
  :: (Eq n, Ord n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> [n]
     -- ^ Corresponding pattern nodes.
findPNInMatch m fn = M.findWithDefault [] fn (f2pMaps m)

-- | Same as 'findFNInMapping'.
findFNInMatch
  :: (Eq n, Ord n)
  => Match n
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> [n]
     -- ^ Corresponding function nodes.
findFNInMatch m pn = M.findWithDefault [] pn (p2fMaps m)

-- | From a match and a list of function nodes, get the list of corresponding
-- pattern nodes for which there exists a mapping. The order of the list will be
-- conserved.
findPNsInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of function nodes.
  -> [[n]]
     -- ^ List of corresponding pattern nodes.
findPNsInMapping m fns = map (findPNInMapping m) fns

-- | From a match and a list of pattern nodes, get the list of corresponding
-- function nodes for which there exists a mapping. The order of the list will
-- be conserved.
findFNsInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> [n]
     -- ^ List of pattern nodes.
  -> [[n]]
     -- ^ List of corresponding function nodes.
findFNsInMapping m pns = map (findFNInMapping m) pns

-- | From a mapping state and a function node, get the corresponding pattern
-- nodes if there exist such mappings.
findPNInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Function node.
  -> [n]
     -- ^ Corresponding pattern nodes.
findPNInMapping st fn = [ pNode m | m <- st, fn == fNode m ]

-- | From a mapping state and a pattern node, get the corresponding function
-- nodes if there exist such mappings.
findFNInMapping
  :: (Eq n)
  => [Mapping n]
     -- ^ The current mapping state.
  -> n
     -- ^ Pattern node.
  -> [n]
     -- ^ Corresponding function nodes.
findFNInMapping st pn = [ fNode m | m <- st, pn == pNode m ]

-- | Computes the dominator sets for a given graph and root node.
computeDomSets :: Graph -> Node -> [DomSet Node]
computeDomSets g n =
  let int_g = intGraph g
      mkNode = fromJust . getNodeWithIntNodeID int_g
      doms = map ( \(n1, ns2) -> DomSet { domNode = mkNode n1
                                        , domSet = map mkNode ns2
                                        }
                 )
                 (I.dom int_g (getIntNodeID n))
  in doms

-- | Checks whether the given graph is empty. A graph is empty if it contains no
-- nodes.
isGraphEmpty :: Graph -> Bool
isGraphEmpty = I.isEmpty . intGraph

-- | Extracts the control-flow graph from a graph. If there are no block nodes
-- in the graph, an empty graph is returned.
extractCFG :: Graph -> Graph
extractCFG g =
  let nodes_to_remove = filter (\n -> not (isBlockNode n || isControlNode n)) $
                        getAllNodes g
      cfg_with_ctrl_nodes = foldr delNode g nodes_to_remove
      cfg = foldr delNodeKeepEdges
                  cfg_with_ctrl_nodes
                  (filter isControlNode $ getAllNodes cfg_with_ctrl_nodes)
  in cfg

-- | Extracts the SSA graph (including nodes which represent data) from a
-- graph. If there are no operation nodes in the graph, an empty graph is
-- returned.
extractSSAG :: Graph -> Graph
extractSSAG g =
  let nodes_to_remove =
        filter ( \n -> not (isOperationNode n || isDatumNode n) ||
                       (isControlNode n && not (isRetControlNode n))
               ) $
        getAllNodes g
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
toMatch ms =
  let insert (n1, n2) m = M.insertWith (++) n1 [n2] m
  in Match { f2pMaps = foldr insert M.empty $
                       map (\m -> (fNode m, pNode m)) ms
           , p2fMaps = foldr insert M.empty $
                       map (\m -> (pNode m, fNode m)) ms
           }

-- | Converts a match to a list of mappings.
fromMatch :: Ord n => Match n -> [Mapping n]
fromMatch m =
  M.foldrWithKey
    (\fn pns ms -> (ms ++ map (\pn -> Mapping { fNode = fn, pNode = pn }) pns))
    []
    (f2pMaps m)

-- | Gives the subgraph induced by a given list of nodes
subGraph :: Graph -> [Node] -> Graph
subGraph g ns =
    let sns = filter (\n -> n `elem` ns) $ getAllNodes g
        ses = filter ( \e -> getSourceNode g e `elem` ns &&
                             getTargetNode g e `elem` ns
                     ) $
              getAllEdges g
        entry = entryBlockNode g
        new_entry = if isJust entry && (fromJust entry) `elem` sns
                    then entry
                    else Nothing
    in mkGraph sns ses new_entry

-- | Checks if two edges have the same in-edge numbers.
haveSameInEdgeNrs :: Edge -> Edge -> Bool
haveSameInEdgeNrs e1 e2 = getEdgeInNr e1 == getEdgeInNr e2

-- | Checks if two edges have the same out-edge numbers.
haveSameOutEdgeNrs :: Edge -> Edge -> Bool
haveSameOutEdgeNrs e1 e2 = getEdgeOutNr e1 == getEdgeOutNr e2

-- | Groups a list of nodes into groups according to their node IDs.
groupNodesByID :: [Node] -> [(NodeID, [Node])]
groupNodesByID ns =
  let ns_by_id = groupBy (\n1 n2 -> getNodeID n1 == getNodeID n2) ns
  in map (\ns' -> (getNodeID $ head ns', ns')) ns_by_id

-- | Returns the definition edges with matching edge-in number as the given
-- edge.
findDefEdgeOfDtInEdge :: Graph -> Edge -> [Edge]
findDefEdgeOfDtInEdge g e =
  let v = getTargetNode g e
      nr = getEdgeInNr e
      def_es = filter (\e' -> getEdgeInNr e' == nr) $
               getDefInEdges g v
  in def_es

-- | Returns the definition edges with matching edge-out number as the given
-- edge.
findDefEdgeOfDtOutEdge :: Graph -> Edge -> [Edge]
findDefEdgeOfDtOutEdge g e =
  let v = getSourceNode g e
      nr = getEdgeOutNr e
      def_es = filter (\e' -> getEdgeOutNr e' == nr) $
               getDefOutEdges g v
  in def_es

-- | Removes a function node from a given 'Match'.
delFNodeInMatch :: (Eq n, Ord n) => n -> Match n -> Match n
delFNodeInMatch fn m =
  let pns = M.findWithDefault [] fn (f2pMaps m)
      new_f2p_maps = M.delete fn (f2pMaps m)
      new_p2f_maps = foldr (\pn m' -> M.update (Just . filter (/= fn)) pn m')
                           (p2fMaps m)
                           pns
  in Match { f2pMaps = new_f2p_maps, p2fMaps = new_p2f_maps }

-- | Removes a pattern node from a given 'Match'.
delPNodeInMatch :: (Eq n, Ord n) => n -> Match n -> Match n
delPNodeInMatch pn m =
  let fns = M.findWithDefault [] pn (p2fMaps m)
      new_p2f_maps = M.delete pn (p2fMaps m)
      new_f2p_maps = foldr (\fn m' -> M.update (Just . filter (/= pn)) fn m')
                           (f2pMaps m)
                           fns
  in Match { f2pMaps = new_f2p_maps, p2fMaps = new_p2f_maps }

-- | Merges a list of matches into a single match. If there is an overlap in the
-- mappings, then the mapping lists are simply concatenated.
mergeMatches :: (Eq n, Ord n) => [Match n] -> Match n
mergeMatches [] = error "mergeMatches: empty list"
mergeMatches ms =
  let new_f2p_maps = M.unionsWith (++) $ map f2pMaps ms
      new_p2f_maps = M.unionsWith (++) $ map p2fMaps ms
  in Match { f2pMaps = new_f2p_maps, p2fMaps = new_p2f_maps }

-- | Adds a new mapping to the given match.
addMappingToMatch :: (Eq n, Ord n) => Mapping n -> Match n -> Match n
addMappingToMatch m match =
  let fn = fNode m
      pn = pNode m
      new_f2p_maps = M.insertWith (++) fn [pn] $ f2pMaps match
      new_p2f_maps = M.insertWith (++) pn [fn] $ p2fMaps match
  in Match { f2pMaps = new_f2p_maps, p2fMaps = new_p2f_maps }

-- | Replaces a function node in the given match with another function node.
updateFNodeInMatch
  :: (Eq n, Ord n)
  => n
     -- ^ Old node.
  -> n
     -- ^ New node.
  -> Match n
  -> Match n
updateFNodeInMatch old_fn new_fn match =
  let f2p_maps0 = f2pMaps match
      (maybe_pns, f2p_maps1) = M.updateLookupWithKey (\_ _ -> Nothing)
                                                     old_fn
                                                     f2p_maps0
                         -- Do a lookup and delete in one go
      pns = maybe [] id maybe_pns
      f2p_maps2 = M.insert new_fn pns f2p_maps1
      p2f_maps0 = p2fMaps match
      p2f_maps1 = foldr (M.adjust (\pns' -> (new_fn:filter (/= old_fn) pns')))
                        p2f_maps0
                        pns
  in Match { f2pMaps = f2p_maps2, p2fMaps = p2f_maps1 }

-- | Replaces a pattern node in the given match with another pattern node.
updatePNodeInMatch
  :: (Eq n, Ord n)
  => n
     -- ^ Old node.
  -> n
     -- ^ New node.
  -> Match n
  -> Match n
updatePNodeInMatch old_pn new_pn match =
  let p2f_maps0 = p2fMaps match
      (maybe_fns, p2f_maps1) = M.updateLookupWithKey (\_ _ -> Nothing)
                                                     old_pn
                                                     p2f_maps0
                         -- Do a lookup and delete in one go
      fns = maybe [] id maybe_fns
      p2f_maps2 = M.insert new_pn fns p2f_maps1
      f2p_maps0 = f2pMaps match
      f2p_maps1 = foldr (M.adjust (\fns' -> (new_pn:filter (/= old_pn) fns')))
                        f2p_maps0
                        fns
  in Match { f2pMaps = f2p_maps1, p2fMaps = p2f_maps2 }

-- | Gets all sets of copy-related values. A value is copy-related to another
-- value if both are copies of the same other value.
getCopyRelatedValues :: Graph -> [[Node]]
getCopyRelatedValues g =
  let v_ns = filter isValueNode $ getAllNodes g
      copy_related_vs = filter ((> 1) . length) $
                        concat $
                        map ( groupBy ( \v1 v2 -> getDataTypeOfValueNode v1 ==
                                                  getDataTypeOfValueNode v2
                                      )
                            ) $
                        map (getCopiesOfValue g) v_ns
  in copy_related_vs

-- | Given a graph and value node, returns all value nodes that are copies of
-- the given value node.
getCopiesOfValue :: Graph -> Node -> [Node]
getCopiesOfValue g n =
  let es = getDtFlowOutEdges g n
      copies = filter isCopyNode $ map (getTargetNode g) es
      cp_vs = map ( \n' ->
                    let es' = getDtFlowOutEdges g n'
                    in if length es' == 1
                       then getTargetNode g (head es')
                       else if length es' == 0
                            then error $
                                 "getCopiesOfValue: " ++ show n' ++
                                 " has no data-flow edges"
                            else error $
                                 "getCopiesOfValue: " ++ show n' ++
                                 " has multiple data-flow edges"
                  ) $
              copies
  in cp_vs
