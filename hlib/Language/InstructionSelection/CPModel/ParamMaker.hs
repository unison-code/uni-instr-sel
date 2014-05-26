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
replaceInBoolExpr m (DataNodeIsAnIntConstantExpr e) =
  DataNodeIsAnIntConstantExpr (replaceInNodeExpr m e)

replaceInNumExpr :: Matchset NodeID -> NumExpr -> NumExpr
replaceInNumExpr m (PlusExpr  lhs rhs) =
  PlusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (MinusExpr lhs rhs) =
  MinusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (Int2NumExpr e) =
  Int2NumExpr (replaceInIntExpr m e)
replaceInNumExpr m (Bool2NumExpr e) =
  Bool2NumExpr (replaceInBoolExpr m e)
replaceInNumExpr m (Node2NumExpr e) =
  Node2NumExpr (replaceInNodeExpr m e)
replaceInNumExpr m (PatternInstance2NumExpr e) =
  PatternInstance2NumExpr (replaceInPatternInstanceExpr m e)
replaceInNumExpr m (Instruction2NumExpr e) =
  Instruction2NumExpr (replaceInInstructionExpr m e)
replaceInNumExpr m (Pattern2NumExpr e) =
  Pattern2NumExpr (replaceInPatternExpr m e)
replaceInNumExpr m (Label2NumExpr e) =
  Label2NumExpr (replaceInLabelExpr m e)
replaceInNumExpr m (Register2NumExpr e) =
  Register2NumExpr (replaceInRegisterExpr m e)
replaceInNumExpr m (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  DistanceBetweenInstanceAndLabelExpr (replaceInPatternInstanceExpr m pat_e)
                                      (replaceInLabelExpr m lab_e)

replaceInIntExpr :: Matchset NodeID -> IntExpr -> IntExpr
replaceInIntExpr _ (AnIntegerExpr i) = AnIntegerExpr i
replaceInIntExpr m (IntConstValueOfDataNodeExpr e) =
  IntConstValueOfDataNodeExpr $ replaceInNodeExpr m e

replaceInNodeExpr :: Matchset NodeID -> NodeExpr -> NodeExpr
replaceInNodeExpr m (ANodeIDExpr i) =
  ANodeIDExpr $ fromJust $ mappedNodePToF m i

replaceInPatternInstanceExpr :: Matchset NodeID
                                -> PatternInstanceExpr
                                -> PatternInstanceExpr
replaceInPatternInstanceExpr _ (APatternInstanceIDExpr i) =
  APatternInstanceIDExpr i
replaceInPatternInstanceExpr m (CovererOfActionNodeExpr e) =
  CovererOfActionNodeExpr (replaceInNodeExpr m e)
replaceInPatternInstanceExpr m (DefinerOfDataNodeExpr e) =
  DefinerOfDataNodeExpr (replaceInNodeExpr m e)
replaceInPatternInstanceExpr m (DefinerOfStateNodeExpr e) =
  DefinerOfStateNodeExpr (replaceInNodeExpr m e)
replaceInPatternInstanceExpr _ ThisPatternInstanceExpr = ThisPatternInstanceExpr

replaceInInstructionExpr :: Matchset NodeID
                            -> InstructionExpr
                            -> InstructionExpr
replaceInInstructionExpr _ (AnInstructionIDExpr i) = AnInstructionIDExpr i
replaceInInstructionExpr m (InstructionOfPatternExpr e) =
  InstructionOfPatternExpr (replaceInPatternExpr m e)

replaceInPatternExpr :: Matchset NodeID -> PatternExpr -> PatternExpr
replaceInPatternExpr _ (APatternIDExpr i) = APatternIDExpr i
replaceInPatternExpr m (PatternOfPatternInstanceExpr e) =
  PatternOfPatternInstanceExpr (replaceInPatternInstanceExpr m e)

replaceInLabelExpr :: Matchset NodeID -> LabelExpr -> LabelExpr
replaceInLabelExpr m (LabelAllocatedToPatternInstanceExpr e) =
  LabelAllocatedToPatternInstanceExpr (replaceInPatternInstanceExpr m e)
replaceInLabelExpr m (LabelOfLabelNodeExpr e) =
  LabelOfLabelNodeExpr (replaceInNodeExpr m e)

replaceInRegisterExpr :: Matchset NodeID -> RegisterExpr -> RegisterExpr
replaceInRegisterExpr _ (ARegisterIDExpr i) = ARegisterIDExpr i
replaceInRegisterExpr m (RegisterAllocatedToDataNodeExpr e) =
  RegisterAllocatedToDataNodeExpr (replaceInNodeExpr m e)

replaceInSetElemExpr :: Matchset NodeID -> SetElemExpr -> SetElemExpr
replaceInSetElemExpr m (Label2SetElemExpr e) =
  Label2SetElemExpr (replaceInLabelExpr m e)
replaceInSetElemExpr m (Register2SetElemExpr e) =
  Register2SetElemExpr (replaceInRegisterExpr m e)

replaceInSetExpr :: Matchset NodeID -> SetExpr -> SetExpr
replaceInSetExpr m (UnionSetExpr lhs rhs) =
  UnionSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (IntersectSetExpr lhs rhs) =
  IntersectSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DiffSetExpr lhs rhs) =
  DiffSetExpr (replaceInSetExpr m lhs) (replaceInSetExpr m rhs)
replaceInSetExpr m (DomSetOfLabelExpr e) =
  DomSetOfLabelExpr (replaceInLabelExpr m e)
replaceInSetExpr m (RegisterClassExpr es) =
  RegisterClassExpr (map (replaceInRegisterExpr m) es)
