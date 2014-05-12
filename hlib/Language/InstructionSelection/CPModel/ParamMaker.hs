--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.CPModel.ParamMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Constructs the parameters which are used to create the CP model. These
-- include the function data, such as the IDs of the various nodes, and the
-- pattern data, which also contain the matchsets.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ParamMaker (
  NoUseDefDomConstraintSetting
, mkParams
) where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns ( InstanceId
                                              , InstProperties (..)
                                              )
import Language.InstructionSelection.TargetMachine
import Data.Maybe



-------------
-- Type defs
-------------

type NoUseDefDomConstraintSetting = Bool



-------------
-- Functions
-------------

-- | Takes a function and a list of pattern instance data and transforms it into
-- a CP model parameters object.

mkParams :: OpStructure      -- ^ Function data.
            -> TargetMachine -- ^ Machine data.
            -> [( OpStructure
                , [(Matchset Node, InstanceId)]
                , InstProperties
                , NoUseDefDomConstraintSetting
                )] -- ^ Pattern instance data.
            -> CPModelParams
mkParams f m ps =
  CPModelParams (mkFunctionGraphData f)
                (concatMap mkPatternInstanceData ps)
                -- TODO: fix building of machine data
                (mkMachineData m)

mkFunctionGraphData :: OpStructure -> FunctionGraphData
mkFunctionGraphData os =
  let g = osGraph os
      nodeIdsByType f = nodeIds $ filter f $ allNodes g
      cfg = extractCFG g
  in FunctionGraphData (nodeIdsByType isActionNode)
                       (nodeIdsByType isDataNode)
                       (nodeIdsByType isStateNode)
                       (computeLabelDoms cfg)
                       (nodeId $ fromJust $ rootInCFG cfg)
                       (osConstraints os)

mkMachineData :: TargetMachine
                 -> MachineData
mkMachineData m =
  MachineData (map snd (tmRegisters m))

mkPatternInstanceData :: ( OpStructure
                         , [(Matchset Node, InstanceId)]
                         , InstProperties
                         , NoUseDefDomConstraintSetting
                         )
                      -> [PatternInstanceData]
mkPatternInstanceData (os, matchsets, props, b_usedef) =
  let g = osGraph os
      a_ns = (nodeIds $ filter isActionNode $ allNodes g)
      getNodes t k = nodeIds
                     $ filter (\n -> length (k g n) > 0)
                     $ filter t
                     $ allNodes g
      d_def_ns = getNodes isDataNode predecessors
      d_use_ns = getNodes isDataNode successors
      s_def_ns = getNodes isStateNode predecessors
      s_use_ns = getNodes isStateNode successors
      l_ref_ns = getNodes isLabelNode predecessors
      f (m, pid) = mkPatternInstanceData' a_ns
                                          d_def_ns
                                          d_use_ns
                                          s_def_ns
                                          s_use_ns
                                          l_ref_ns
                                          (osConstraints os)
                                          b_usedef
                                          (convertMatchsetNToId m)
                                          pid
                                          props
  in map f matchsets

mkPatternInstanceData' :: [NodeId]    -- ^ Action nodes covered by the pattern.
                          -> [NodeId] -- ^ Data nodes defined.
                          -> [NodeId] -- ^ Data nodes used.
                          -> [NodeId] -- ^ State nodes defined.
                          -> [NodeId] -- ^ State nodes used.
                          -> [NodeId] -- ^ Label nodes referred to.
                          -> [Constraint]
                          -> NoUseDefDomConstraintSetting
                          -> Matchset NodeId
                          -> InstanceId
                          -> InstProperties
                          -> PatternInstanceData
mkPatternInstanceData' a_ns
                       d_def_ns
                       d_use_ns
                       s_def_ns
                       s_use_ns
                       l_ref_ns
                       cs
                       b_usedef
                       matchset
                       pid
                       props =
  PatternInstanceData pid
                      (mappedNodesPToF matchset a_ns)
                      (mappedNodesPToF matchset d_def_ns)
                      (mappedNodesPToF matchset d_use_ns)
                      (mappedNodesPToF matchset s_def_ns)
                      (mappedNodesPToF matchset s_use_ns)
                      (mappedNodesPToF matchset l_ref_ns)
                      (replaceNodeIdsPToFInConstraints matchset cs)
                      b_usedef
                      (instCodeSize props)
                      (instLatency props)

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.

computeLabelDoms :: Graph                   -- ^ The CFG.
                    -> [(NodeId, [NodeId])] -- ^ Dominator sets.
computeLabelDoms cfg =
  let root = fromJust $ rootInCFG cfg
      node_domsets = extractDomSet cfg root
      node_id_domsets = map (\(n, ns) -> (nodeId n, map nodeId ns)) node_domsets
  in node_id_domsets

-- | Replaces the node IDs used in the constraints from matched pattern node IDs
-- to the corresponding function node IDs.

replaceNodeIdsPToFInConstraints :: Matchset NodeId
                                  -> [Constraint]
                                  -> [Constraint]
replaceNodeIdsPToFInConstraints m cs =
  map (replaceFunc m) cs
  where replaceFunc m (BoolExprConstraint e) = BoolExprConstraint $
                                               replaceInBoolExpr m e
        replaceFunc _ c = c

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
replaceInBoolExpr m (InSetExpr lhs rhs) =
  InSetExpr (replaceInSetElemExpr m lhs) (replaceInSetExpr m rhs)

replaceInNumExpr :: Matchset NodeId -> NumExpr -> NumExpr
replaceInNumExpr m (PlusExpr  lhs rhs) =
  PlusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (MinusExpr lhs rhs) =
  MinusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (Int2NumExpr e) =
  Int2NumExpr (replaceInIntExpr m e)
replaceInNumExpr m (Bool2NumExpr e) =
  Bool2NumExpr (replaceInBoolExpr m e)
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
replaceInNumExpr m (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  DistanceBetweenInstanceAndLabelExpr (replaceInInstanceIdExpr m pat_e)
                                      (replaceInLabelIdExpr m lab_e)

replaceInIntExpr :: Matchset NodeId -> IntExpr -> IntExpr
replaceInIntExpr _ (AnIntegerExpr i) = AnIntegerExpr i
replaceInIntExpr m (IntConstValueOfDataNodeExpr e) =
  IntConstValueOfDataNodeExpr $ replaceInNodeIdExpr m e

replaceInNodeIdExpr :: Matchset NodeId -> NodeIdExpr -> NodeIdExpr
replaceInNodeIdExpr m (ANodeIdExpr i) =
  ANodeIdExpr $ fromJust $ mappedNodePToF m i

replaceInInstanceIdExpr :: Matchset NodeId -> InstanceIdExpr -> InstanceIdExpr
replaceInInstanceIdExpr _ (AnInstanceIdExpr i) = AnInstanceIdExpr i
replaceInInstanceIdExpr m (CovererOfActionNodeExpr e) =
  CovererOfActionNodeExpr (replaceInNodeIdExpr m e)
replaceInInstanceIdExpr m (DefinerOfDataNodeExpr e) =
  DefinerOfDataNodeExpr (replaceInNodeIdExpr m e)
replaceInInstanceIdExpr m (DefinerOfStateNodeExpr e) =
  DefinerOfStateNodeExpr (replaceInNodeIdExpr m e)
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

replaceInSetElemExpr :: Matchset NodeId -> SetElemExpr -> SetElemExpr
replaceInSetElemExpr m (LabelId2SetElemExpr e) =
  LabelId2SetElemExpr (replaceInLabelIdExpr m e)
replaceInSetElemExpr m (RegisterId2SetElemExpr e) =
  RegisterId2SetElemExpr (replaceInRegisterIdExpr m e)

replaceInSetExpr :: Matchset NodeId -> SetExpr -> SetExpr
replaceInSetExpr m (UnionSetExpr lhs rhs) =
  UnionSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (IntersectSetExpr lhs rhs) =
  IntersectSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DiffSetExpr lhs rhs) =
  DiffSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DomSetOfLabelIdExpr e) =
  DomSetOfLabelIdExpr (replaceInLabelIdExpr m e)
replaceInSetExpr m (RegisterClassExpr es) =
  RegisterClassExpr (map (replaceInRegisterIdExpr m) es)
