--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.ParamMaker
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

module Language.InstSel.CPModel.ParamMaker
  ( mkParams )
where

import Language.InstSel.Constraints
import Language.InstSel.CPModel
  hiding
  ( bbLabel
  , mAssIDMaps
  , mADDUC
  )
import qualified Language.InstSel.CPModel as C
  ( bbLabel )
import Language.InstSel.Graphs
  hiding
  ( bbLabel )
import qualified Language.InstSel.Graphs as G
  ( bbLabel )
import Language.InstSel.Graphs.PatternMatching.CP
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
  ( Function (..)
  , getExecFreqOfBBInFunction
  )
import Language.InstSel.TargetMachine
import Data.Maybe
  ( fromJust )



-------------
-- Functions
-------------

-- | Takes a function and machine data to generate the corresponding parameters
-- to the constraint model. This will also perform pattern matching of all
-- patterns over the function graph.
mkParams :: TargetMachine -> Function -> CPModelParams
mkParams m f =
  CPModelParams
    (mkFunctionGraphData f)
    (mkMatchData f (tmInstructions m))
    (mkMachineData m)

mkFunctionGraphData :: Function -> FunctionGraphData
mkFunctionGraphData f =
  let g = osGraph $ functionOS f
      nodeIDsByType f' = getNodeIDs $ filter f' (getAllNodes g)
      essential_op_nodes =
        filter
          (\n -> isOperationNode n && (not $ isCopyNode n))
          (getAllNodes g)
      cfg = extractCFG g
      dp_edge_data =
        map
          (\e -> (getNodeID $ getSourceNode g e, getNodeID $ getTargetNode g e))
          (filter isDefPlaceEdge (getAllEdges g))
      getExecFreq n =
        fromJust $ getExecFreqOfBBInFunction f (G.bbLabel $ getNodeType n)
  in FunctionGraphData
       (nodeIDsByType isOperationNode)
       (map getNodeID essential_op_nodes)
       (nodeIDsByType isDataNode)
       (nodeIDsByType isStateNode)
       (computeLabelDoms cfg)
       dp_edge_data
       (getNodeID $ fromJust $ rootInCFG cfg)
       ( map
           ( \n -> BasicBlockData
                     { C.bbLabel = (G.bbLabel $ getNodeType n)
                     , bbLabelNode = (getNodeID n)
                     , bbExecFrequency = getExecFreq n
                     }
           )
           (filter isLabelNode (getAllNodes g))
       )
       (osConstraints $ functionOS f)

mkMachineData :: TargetMachine -> MachineData
mkMachineData m =
  MachineData
  (tmID m)
  (map fst (tmRegisters m))

mkMatchData ::
     Function
  -> [Instruction]
  -> [MatchData]
mkMatchData f is =
  fst $ foldr (processInstruction f) ([], 0) is

processInstruction ::
     Function
  -> Instruction
  -> ([MatchData], MatchID)
  -> ([MatchData], MatchID)
processInstruction f i t =
  foldr (processInstPattern f i) t (instPatterns i)

processInstPattern ::
     Function
  -> Instruction
  -> InstPattern
  -> ([MatchData], MatchID)
  -> ([MatchData], MatchID)
processInstPattern f i p t =
  let fg = osGraph $ functionOS f
      pg = osGraph $ patOS p
      ms = map convertMatchN2ID (findMatches fg pg)
  in foldr (processMatch i p) t ms

processMatch ::
     Instruction
  -> InstPattern
  -> Match NodeID
  -> ([MatchData], MatchID)
  -> ([MatchData], MatchID)
processMatch i p m (mids, next_mid) =
  let g = osGraph $ patOS p
      ns = getAllNodes g
      a_ns = filter isOperationNode ns
      d_ns = filter isDataNode ns
      s_ns = filter isStateNode ns
      l_ns = filter isLabelNode ns
      c_ns = filter isControlNode ns
      d_def_ns = filter (hasAnyPredecessors g) d_ns
      d_use_ns = filter (hasAnySuccessors g) d_ns
      d_use_by_phi_ns = filter
                          (\n -> any isPhiNode (getSuccessors g n))
                          d_use_ns
      s_def_ns = filter (hasAnyPredecessors g) s_ns
      s_use_ns = filter (hasAnySuccessors g) s_ns
      l_ref_ns = filter (hasAnyPredecessors g) l_ns
      i_props = instProps i
      cfg = extractCFG g
      root_label_node_id =
        maybe Nothing ((findFNInMatch m) . getNodeID) (rootInCFG cfg)
      new_mid = MatchData
                  (instID i)
                  (patID p)
                  next_mid
                  (findFNsInMatch m (getNodeIDs a_ns))
                  (findFNsInMatch m (getNodeIDs d_def_ns))
                  (findFNsInMatch m (getNodeIDs d_use_ns))
                  (findFNsInMatch m (getNodeIDs d_use_by_phi_ns))
                  (findFNsInMatch m (getNodeIDs s_def_ns))
                  (findFNsInMatch m (getNodeIDs s_use_ns))
                  root_label_node_id
                  (findFNsInMatch m (getNodeIDs l_ref_ns))
                  (mapPs2FsInConstraints m (osConstraints $ patOS p))
                  (patAUDDC p)
                  (length c_ns > 0)
                  (instCodeSize i_props)
                  (instLatency i_props)
                  (findFNsInMatch m (patAssIDMaps p))
  in (new_mid:mids, next_mid + 1)

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.
computeLabelDoms ::
     Graph
     -- ^ The CFG.
  -> [Domset NodeID]
     -- ^ Dominator sets.
computeLabelDoms cfg =
  let root = fromJust $ rootInCFG cfg
      node_domsets = extractDomSet cfg root
      node_id_domsets = map
                          ( \d -> Domset
                                    { domNode = (getNodeID $ domNode d)
                                    , domSet = (map getNodeID (domSet d))
                                    }
                          )
                          node_domsets
  in node_id_domsets

-- | Replaces the node IDs used in the constraints from matched pattern node IDs
-- to the corresponding function node IDs.
mapPs2FsInConstraints :: Match NodeID -> [Constraint] -> [Constraint]
mapPs2FsInConstraints m cs =
  map (replaceFunc m) cs
  where replaceFunc m' (BoolExprConstraint e) =
          BoolExprConstraint $ replaceInBoolExpr m' e

replaceInBoolExpr :: Match NodeID -> BoolExpr -> BoolExpr
replaceInBoolExpr m (EqExpr lhs rhs) =
  EqExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (NeqExpr lhs rhs) =
  NeqExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (GTExpr lhs rhs) =
  GTExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (GEExpr lhs rhs) =
  GEExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (LTExpr lhs rhs) =
  LTExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (LEExpr lhs rhs) =
  LEExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInBoolExpr m (AndExpr lhs rhs) =
  AndExpr (replaceInBoolExpr m lhs) (replaceInBoolExpr m rhs)
replaceInBoolExpr m (OrExpr lhs rhs) =
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
replaceInBoolExpr m (DataNodeIsIntermediateExpr e) =
  DataNodeIsIntermediateExpr (replaceInNodeExpr m e)

replaceInNumExpr :: Match NodeID -> NumExpr -> NumExpr
replaceInNumExpr m (PlusExpr lhs rhs) =
  PlusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (MinusExpr lhs rhs) =
  MinusExpr (replaceInNumExpr m lhs) (replaceInNumExpr m rhs)
replaceInNumExpr m (Int2NumExpr e) =
  Int2NumExpr (replaceInIntExpr m e)
replaceInNumExpr m (Bool2NumExpr e) =
  Bool2NumExpr (replaceInBoolExpr m e)
replaceInNumExpr m (Node2NumExpr e) =
  Node2NumExpr (replaceInNodeExpr m e)
replaceInNumExpr m (Match2NumExpr e) =
  Match2NumExpr (replaceInMatchExpr m e)
replaceInNumExpr m (Instruction2NumExpr e) =
  Instruction2NumExpr (replaceInInstructionExpr m e)
replaceInNumExpr m (Pattern2NumExpr e) =
  Pattern2NumExpr (replaceInPatternExpr m e)
replaceInNumExpr m (Label2NumExpr e) =
  Label2NumExpr (replaceInLabelExpr m e)
replaceInNumExpr m (Register2NumExpr e) =
  Register2NumExpr (replaceInRegisterExpr m e)
replaceInNumExpr m (DistanceBetweenMatchAndLabelExpr pat_e lab_e) =
  DistanceBetweenMatchAndLabelExpr
    (replaceInMatchExpr m pat_e)
    (replaceInLabelExpr m lab_e)

replaceInIntExpr :: Match NodeID -> IntExpr -> IntExpr
replaceInIntExpr _ (AnIntegerExpr i) = AnIntegerExpr i
replaceInIntExpr m (IntConstValueOfDataNodeExpr e) =
  IntConstValueOfDataNodeExpr $ replaceInNodeExpr m e

replaceInNodeExpr :: Match NodeID -> NodeExpr -> NodeExpr
replaceInNodeExpr m (ANodeIDExpr i) =
  ANodeIDExpr $ fromJust $ findFNInMatch m i

replaceInMatchExpr ::
     Match NodeID
  -> MatchExpr
  -> MatchExpr
replaceInMatchExpr _ (AMatchIDExpr i) =
  AMatchIDExpr i
replaceInMatchExpr m (CovererOfOperationNodeExpr e) =
  CovererOfOperationNodeExpr (replaceInNodeExpr m e)
replaceInMatchExpr m (DefinerOfDataNodeExpr e) =
  DefinerOfDataNodeExpr (replaceInNodeExpr m e)
replaceInMatchExpr m (DefinerOfStateNodeExpr e) =
  DefinerOfStateNodeExpr (replaceInNodeExpr m e)
replaceInMatchExpr _ ThisMatchExpr = ThisMatchExpr

replaceInInstructionExpr ::
     Match NodeID
  -> InstructionExpr
  -> InstructionExpr
replaceInInstructionExpr _ (AnInstructionIDExpr i) = AnInstructionIDExpr i
replaceInInstructionExpr m (InstructionOfPatternExpr e) =
  InstructionOfPatternExpr (replaceInPatternExpr m e)

replaceInPatternExpr :: Match NodeID -> PatternExpr -> PatternExpr
replaceInPatternExpr _ (APatternIDExpr i) = APatternIDExpr i
replaceInPatternExpr m (PatternOfMatchExpr e) =
  PatternOfMatchExpr (replaceInMatchExpr m e)

replaceInLabelExpr :: Match NodeID -> LabelExpr -> LabelExpr
replaceInLabelExpr m (LabelAllocatedToMatchExpr e) =
  LabelAllocatedToMatchExpr (replaceInMatchExpr m e)
replaceInLabelExpr m (LabelOfLabelNodeExpr e) =
  LabelOfLabelNodeExpr (replaceInNodeExpr m e)

replaceInRegisterExpr :: Match NodeID -> RegisterExpr -> RegisterExpr
replaceInRegisterExpr _ (ARegisterIDExpr i) = ARegisterIDExpr i
replaceInRegisterExpr m (RegisterAllocatedToDataNodeExpr e) =
  RegisterAllocatedToDataNodeExpr (replaceInNodeExpr m e)

replaceInSetElemExpr :: Match NodeID -> SetElemExpr -> SetElemExpr
replaceInSetElemExpr m (Label2SetElemExpr e) =
  Label2SetElemExpr (replaceInLabelExpr m e)
replaceInSetElemExpr m (Register2SetElemExpr e) =
  Register2SetElemExpr (replaceInRegisterExpr m e)

replaceInSetExpr :: Match NodeID -> SetExpr -> SetExpr
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
