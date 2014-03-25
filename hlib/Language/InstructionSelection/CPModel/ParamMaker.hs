--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.ParamMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructs the parameters which are used to create the CP model. These
-- include the function data, such as the IDs of the various nodes, and the
-- pattern data, which also contain the matchsets.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ParamMaker (
  mkParams
) where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns ( InstanceId
                                              , InstProperties (..)
                                              )
import Data.Maybe



-------------
-- Functions
-------------

-- | Takes a function and a list of pattern instance data and transforms it into
-- a CP model parameters object.

mkParams :: OpStructure -- ^ Function data.
            -> [( OpStructure
                , [(Matchset Node, InstanceId)]
                , InstProperties
                )] -- ^ Pattern instance data.
            -> CPModelParams
mkParams f ps =
  CPModelParams (mkFunctionGraphData f)
                (concatMap mkPatternInstanceData ps)
                -- TODO: fix building of machine data
                MachineData

mkFunctionGraphData :: OpStructure -> FunctionGraphData
mkFunctionGraphData os =
  let g = osGraph os
      nodeIdsByType f = nodeIds $ filter f $ allNodes g
  in FunctionGraphData (nodeIdsByType isActionNode)
                       (nodeIdsByType isEntityNode)
                       (computeLabelDoms g)
                       (osConstraints os)

mkPatternInstanceData :: ( OpStructure
                         , [(Matchset Node, InstanceId)]
                         , InstProperties
                         )
                      -> [PatternInstanceData]
mkPatternInstanceData (os, matchsets, props) =
  let g = osGraph os
      a_ns = (nodeIds $ filter isActionNode $ allNodes g)
      entityNodes p = nodeIds
                      $ filter (\n -> length (p g n) > 0)
                      $ filter isEntityNode
                      $ allNodes g
      e_def_ns = entityNodes predecessors
      e_use_ns = entityNodes successors
      f (m, id) = mkPatternInstanceData' a_ns
                                         e_def_ns
                                         e_use_ns
                                         (osConstraints os)
                                         (convertMatchsetNToId m)
                                         id
                                         props
  in map f matchsets

mkPatternInstanceData' :: [NodeId]    -- ^ Action nodes in the pattern.
                          -> [NodeId] -- ^ Defined entity nodes.
                          -> [NodeId] -- ^ Used entity nodes.
                          -> [Constraint]
                          -> Matchset NodeId
                          -> InstanceId
                          -> InstProperties
                          -> PatternInstanceData
mkPatternInstanceData' a_ns e_def_ns e_use_ns cs matchset id props =
  PatternInstanceData id
                      (mappedNodesPToF matchset a_ns)
                      (mappedNodesPToF matchset e_def_ns)
                      (mappedNodesPToF matchset e_use_ns)
                      (replaceNodeIdsPToFInConstraints matchset cs)
                      (codeSize props)
                      (latency props)

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

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.

computeLabelDoms g =
  let nodes_to_remove = filter (\n -> not (isLabelNode n || isControlNode n))
                        $ allNodes g
      cfg_with_ctrl_nodes = foldl (flip delNode) g nodes_to_remove
      cfg = foldl delNodeKeepEdges
                  cfg_with_ctrl_nodes
                  (filter isControlNode $ allNodes cfg_with_ctrl_nodes)
      root = head $ filter (\n -> length (predecessors g n) == 0) (allNodes cfg)
      node_domsets = dom cfg root
      node_id_domsets = map (\(n, ns) -> (nodeId n, map nodeId ns)) node_domsets
  in node_id_domsets

-- | Replaces the node IDs used in the constraints from matched pattern node IDs
-- to the corresponding function node IDs.

replaceNodeIdsPToFInConstraints :: Matchset NodeId
                                  -> [Constraint]
                                  -> [Constraint]
replaceNodeIdsPToFInConstraints m cs =
  map (toConstraint . (replaceInBoolExpr m) . fromConstraint) cs

replaceInBoolExpr :: Matchset NodeId -> BoolExpr -> BoolExpr
replaceInBoolExpr m (EqExpr  lhs rhs) =
  EqExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (NeqExpr lhs rhs) =
  NeqExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (GTExpr  lhs rhs) =
  GTExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (GEExpr  lhs rhs) =
  GEExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (LTExpr  lhs rhs) =
  LTExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (LEExpr  lhs rhs) =
  LEExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (AndExpr lhs rhs) =
  AndExpr (replaceInBoolExpr m lhs) (replaceInBoolExpr m rhs)
replaceInBoolExpr m (OrExpr  lhs rhs) =
  OrExpr (replaceInBoolExpr m lhs) (replaceInBoolExpr m rhs)
replaceInBoolExpr m (ImpExpr lhs rhs) =
  ImpExpr (replaceInBoolExpr m lhs) (replaceInBoolExpr m rhs)
replaceInBoolExpr m (EqvExpr lhs rhs) =
  EqvExpr (replaceInBoolExpr m lhs) (replaceInBoolExpr m rhs)
replaceInBoolExpr m (NotExpr e) = NotExpr (replaceInBoolExpr m e)

replaceInNumExpr :: Matchset NodeId -> NumExpr -> NumExpr
replaceInNumExpr m (PlusExpr  lhs rhs) =
  PlusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (MinusExpr lhs rhs) =
  MinusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr _ (AnIntegerExpr i) = AnIntegerExpr i
replaceInNumExpr m (NodeId2NumExpr e) =
  NodeId2NumExpr (replaceInNodeIdExpr m e)
replaceInNumExpr m (InstanceId2NumExpr e) =
  InstanceId2NumExpr (replaceInInstanceIdExpr m e)
replaceInNumExpr m (InstructionId2NumExpr e) =
  InstructionId2NumExpr (replaceInInstructionIdExpr m e)
replaceInNumExpr m (PatternId2NumExpr e) =
  PatternId2NumExpr (replaceInPatternIdExpr m e)
replaceInNumExpr m (LabelId2NumExpr e) =
  LabelId2NumExpr (replaceInLabelIdExpr m e)
replaceInNumExpr m (RegisterId2NumExpr e) =
  RegisterId2NumExpr (replaceInRegisterIdExpr m e)

replaceInNodeIdExpr :: Matchset NodeId -> NodeIdExpr -> NodeIdExpr
replaceInNodeIdExpr m (ANodeIdExpr i) =
  ANodeIdExpr $ fromJust $ mappedNodePToF m i

replaceInInstanceIdExpr :: Matchset NodeId -> InstanceIdExpr -> InstanceIdExpr
replaceInInstanceIdExpr _ (AnInstanceIdExpr i) = AnInstanceIdExpr i
replaceInInstanceIdExpr m (CovererOfActionNodeExpr e) =
  CovererOfActionNodeExpr (replaceInNodeIdExpr m e)
replaceInInstanceIdExpr m (DefinerOfEntityNodeExpr e) =
  DefinerOfEntityNodeExpr (replaceInNodeIdExpr m e)
replaceInInstanceIdExpr _ ThisInstanceIdExpr = ThisInstanceIdExpr

replaceInInstructionIdExpr :: Matchset NodeId
                              -> InstructionIdExpr
                              -> InstructionIdExpr
replaceInInstructionIdExpr _ (AnInstructionIdExpr i) = AnInstructionIdExpr i
replaceInInstructionIdExpr m (InstructionIdOfPatternExpr e) =
  InstructionIdOfPatternExpr (replaceInPatternIdExpr m e)

replaceInPatternIdExpr :: Matchset NodeId -> PatternIdExpr -> PatternIdExpr
replaceInPatternIdExpr _ (APatternIdExpr i) = APatternIdExpr i
replaceInPatternIdExpr m (PatternIdOfInstanceExpr e) =
  PatternIdOfInstanceExpr (replaceInInstanceIdExpr m e)

replaceInLabelIdExpr :: Matchset NodeId -> LabelIdExpr -> LabelIdExpr
replaceInLabelIdExpr m (LabelIdAllocatedToInstanceExpr e) =
  LabelIdAllocatedToInstanceExpr (replaceInInstanceIdExpr m e)
replaceInLabelIdExpr m (LabelIdOfLabelNodeExpr e) =
  LabelIdOfLabelNodeExpr (replaceInNodeIdExpr m e)

replaceInRegisterIdExpr :: Matchset NodeId -> RegisterIdExpr -> RegisterIdExpr
replaceInRegisterIdExpr _ (ARegisterIdExpr i) = ARegisterIdExpr i
replaceInRegisterIdExpr m (RegisterIdAllocatedToDataNodeExpr e) =
  RegisterIdAllocatedToDataNodeExpr (replaceInNodeIdExpr m e)
