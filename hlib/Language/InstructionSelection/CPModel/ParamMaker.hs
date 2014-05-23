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
import Language.InstructionSelection.Patterns ( PatternInstanceID
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
                , [(Matchset Node, PatternInstanceID)]
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
      nodeIDsByType f = nodeIDs $ filter f $ allNodes g
      cfg = extractCFG g
  in FunctionGraphData (nodeIDsByType isActionNode)
                       (nodeIDsByType isDataNode)
                       (nodeIDsByType isStateNode)
                       (computeLabelDoms cfg)
                       (nodeID $ fromJust $ rootInCFG cfg)
                       (osConstraints os)

mkMachineData :: TargetMachine
                 -> MachineData
mkMachineData m =
  MachineData (map snd (tmRegisters m))

mkPatternInstanceData :: ( OpStructure
                         , [(Matchset Node, PatternInstanceID)]
                         , InstProperties
                         , NoUseDefDomConstraintSetting
                         )
                      -> [PatternInstanceData]
mkPatternInstanceData (os, matchsets, props, b_usedef) =
  let g = osGraph os
      a_ns = (nodeIDs $ filter isActionNode $ allNodes g)
      getNodes t k = nodeIDs
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
                                          (convertMatchsetNToID m)
                                          pid
                                          props
  in map f matchsets

mkPatternInstanceData' :: [NodeID]    -- ^ Action nodes covered by the pattern.
                          -> [NodeID] -- ^ Data nodes defined.
                          -> [NodeID] -- ^ Data nodes used.
                          -> [NodeID] -- ^ State nodes defined.
                          -> [NodeID] -- ^ State nodes used.
                          -> [NodeID] -- ^ Label nodes referred to.
                          -> [Constraint]
                          -> NoUseDefDomConstraintSetting
                          -> Matchset NodeID
                          -> PatternInstanceID
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
                      (replaceNodeIDsPToFInConstraints matchset cs)
                      b_usedef
                      (instCodeSize props)
                      (instLatency props)

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.

computeLabelDoms :: Graph                   -- ^ The CFG.
                    -> [(NodeID, [NodeID])] -- ^ Dominator sets.
computeLabelDoms cfg =
  let root = fromJust $ rootInCFG cfg
      node_domsets = extractDomSet cfg root
      node_id_domsets = map (\(n, ns) -> (nodeID n, map nodeID ns)) node_domsets
  in node_id_domsets

-- | Replaces the node IDs used in the constraints from matched pattern node IDs
-- to the corresponding function node IDs.

replaceNodeIDsPToFInConstraints :: Matchset NodeID
                                  -> [Constraint]
                                  -> [Constraint]
replaceNodeIDsPToFInConstraints m cs =
  map (replaceFunc m) cs
  where replaceFunc m' (BoolExprConstraint e) = BoolExprConstraint $
                                                replaceInBoolExpr m' e
        replaceFunc _ c = c

replaceInBoolExpr :: Matchset NodeID -> BoolExpr -> BoolExpr
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

replaceInNumExpr :: Matchset NodeID -> NumExpr -> NumExpr
replaceInNumExpr m (PlusExpr  lhs rhs) =
  PlusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (MinusExpr lhs rhs) =
  MinusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (Int2NumExpr e) =
  Int2NumExpr (replaceInIntExpr m e)
replaceInNumExpr m (Bool2NumExpr e) =
  Bool2NumExpr (replaceInBoolExpr m e)
replaceInNumExpr m (NodeID2NumExpr e) =
  NodeID2NumExpr (replaceInNodeIDExpr m e)
replaceInNumExpr m (PatternInstanceID2NumExpr e) =
  PatternInstanceID2NumExpr (replaceInPatternInstanceIDExpr m e)
replaceInNumExpr m (InstructionID2NumExpr e) =
  InstructionID2NumExpr (replaceInInstructionIDExpr m e)
replaceInNumExpr m (PatternID2NumExpr e) =
  PatternID2NumExpr (replaceInPatternIDExpr m e)
replaceInNumExpr m (LabelID2NumExpr e) =
  LabelID2NumExpr (replaceInLabelIDExpr m e)
replaceInNumExpr m (RegisterID2NumExpr e) =
  RegisterID2NumExpr (replaceInRegisterIDExpr m e)
replaceInNumExpr m (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  DistanceBetweenInstanceAndLabelExpr (replaceInPatternInstanceIDExpr m pat_e)
                                      (replaceInLabelIDExpr m lab_e)

replaceInIntExpr :: Matchset NodeID -> IntExpr -> IntExpr
replaceInIntExpr _ (AnIntegerExpr i) = AnIntegerExpr i
replaceInIntExpr m (IntConstValueOfDataNodeExpr e) =
  IntConstValueOfDataNodeExpr $ replaceInNodeIDExpr m e

replaceInNodeIDExpr :: Matchset NodeID -> NodeIDExpr -> NodeIDExpr
replaceInNodeIDExpr m (ANodeIDExpr i) =
  ANodeIDExpr $ fromJust $ mappedNodePToF m i

replaceInPatternInstanceIDExpr :: Matchset NodeID -> PatternInstanceIDExpr -> PatternInstanceIDExpr
replaceInPatternInstanceIDExpr _ (APatternInstanceIDExpr i) =
  APatternInstanceIDExpr i
replaceInPatternInstanceIDExpr m (CovererOfActionNodeExpr e) =
  CovererOfActionNodeExpr (replaceInNodeIDExpr m e)
replaceInPatternInstanceIDExpr m (DefinerOfDataNodeExpr e) =
  DefinerOfDataNodeExpr (replaceInNodeIDExpr m e)
replaceInPatternInstanceIDExpr m (DefinerOfStateNodeExpr e) =
  DefinerOfStateNodeExpr (replaceInNodeIDExpr m e)
replaceInPatternInstanceIDExpr _ ThisPatternInstanceIDExpr = ThisPatternInstanceIDExpr

replaceInInstructionIDExpr :: Matchset NodeID
                              -> InstructionIDExpr
                              -> InstructionIDExpr
replaceInInstructionIDExpr _ (AnInstructionIDExpr i) = AnInstructionIDExpr i
replaceInInstructionIDExpr m (InstructionIDOfPatternExpr e) =
  InstructionIDOfPatternExpr (replaceInPatternIDExpr m e)

replaceInPatternIDExpr :: Matchset NodeID -> PatternIDExpr -> PatternIDExpr
replaceInPatternIDExpr _ (APatternIDExpr i) = APatternIDExpr i
replaceInPatternIDExpr m (PatternIDOfPatternInstanceExpr e) =
  PatternIDOfPatternInstanceExpr (replaceInPatternInstanceIDExpr m e)

replaceInLabelIDExpr :: Matchset NodeID -> LabelIDExpr -> LabelIDExpr
replaceInLabelIDExpr m (LabelIDAllocatedToPatternInstanceExpr e) =
  LabelIDAllocatedToPatternInstanceExpr (replaceInPatternInstanceIDExpr m e)
replaceInLabelIDExpr m (LabelIDOfLabelNodeExpr e) =
  LabelIDOfLabelNodeExpr (replaceInNodeIDExpr m e)

replaceInRegisterIDExpr :: Matchset NodeID -> RegisterIDExpr -> RegisterIDExpr
replaceInRegisterIDExpr _ (ARegisterIDExpr i) = ARegisterIDExpr i
replaceInRegisterIDExpr m (RegisterIDAllocatedToDataNodeExpr e) =
  RegisterIDAllocatedToDataNodeExpr (replaceInNodeIDExpr m e)

replaceInSetElemExpr :: Matchset NodeID -> SetElemExpr -> SetElemExpr
replaceInSetElemExpr m (LabelID2SetElemExpr e) =
  LabelID2SetElemExpr (replaceInLabelIDExpr m e)
replaceInSetElemExpr m (RegisterID2SetElemExpr e) =
  RegisterID2SetElemExpr (replaceInRegisterIDExpr m e)

replaceInSetExpr :: Matchset NodeID -> SetExpr -> SetExpr
replaceInSetExpr m (UnionSetExpr lhs rhs) =
  UnionSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (IntersectSetExpr lhs rhs) =
  IntersectSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DiffSetExpr lhs rhs) =
  DiffSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DomSetOfLabelIDExpr e) =
  DomSetOfLabelIDExpr (replaceInLabelIDExpr m e)
replaceInSetExpr m (RegisterClassExpr es) =
  RegisterClassExpr (map (replaceInRegisterIDExpr m) es)
