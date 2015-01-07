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
-- include the function parameters, such as the IDs of the various nodes, and
-- the pattern parameters, which also contain the matchsets.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstSel.CPModel.ParamMaker
  ( MatchData (..)
  , mkParams
  )
where

import Language.InstSel.Constraints
import Language.InstSel.CPModel
  hiding
  ( bbLabel )
import qualified Language.InstSel.CPModel as C
  ( bbLabel )
import Language.InstSel.Graphs
  hiding
  ( bbLabel )
import qualified Language.InstSel.Graphs as G
  ( bbLabel )
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
  ( Function (..)
  , getExecFreqOfBBInFunction
  )
import Language.InstSel.TargetMachines
import Language.InstSel.Utils.JSON
import Data.Maybe
  ( fromJust )



--------------
-- Data types
--------------

-- | Contains the information needed to identify which instruction and pattern a
-- given match originates from. Each match is also given a 'MatchID' that must
-- be unique (although not necessarily continuous) for every match within a list
-- of 'MatchData'.
data MatchData =
    MatchData
      { mdInstrID :: InstructionID
      , mdPatternID :: PatternID
      , mdMatchID :: MatchID
      , mdMatch :: Match NodeID
      }



--------------------------------
-- JSON-related class instances
--------------------------------

instance FromJSON MatchData where
  parseJSON (Object v) =
    MatchData
      <$> v .: "instr-id"
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "match"
  parseJSON _ = mzero

instance ToJSON MatchData where
  toJSON m =
    object [ "instr-id"   .= (mdInstrID m)
           , "pattern-id" .= (mdPatternID m)
           , "match-id"   .= (mdMatchID m)
           , "match"      .= (mdMatch m)
           ]



-------------
-- Functions
-------------

-- | Takes a function, a set of matches, and a target machine and generates the
-- corresponding parameters to the constraint model.
mkParams ::
     TargetMachine
  -> Function
  -> [MatchData]
  -> CPModelParams
mkParams tm f ms =
  CPModelParams
    { functionParams = mkFunctionGraphParams f
    , matchParams = mkMatchParams tm ms
    , machineParams = mkMachineParams tm
    }

mkFunctionGraphParams :: Function -> FunctionGraphParams
mkFunctionGraphParams f =
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
  in FunctionGraphParams
       { funcOpNodes = nodeIDsByType isOperationNode
       , funcEssentialOpNodes = map getNodeID essential_op_nodes
       , funcDataNodes = nodeIDsByType isDataNode
       , funcStateNodes= nodeIDsByType isStateNode
       , funcLabelNodes= computeLabelDoms cfg
       , funcDefPlaceEdges = dp_edge_data
       , funcRootLabel = getNodeID $ fromJust $ rootInCFG cfg
       , funcBasicBlockParams =
           map
             ( \n -> BasicBlockParams
                       { C.bbLabel = (G.bbLabel $ getNodeType n)
                       , bbLabelNode = (getNodeID n)
                       , bbExecFrequency = getExecFreq n
                       }
             )
             (filter isLabelNode (getAllNodes g))
       , funcConstraints = osConstraints $ functionOS f
       }

mkMachineParams :: TargetMachine -> MachineParams
mkMachineParams tm =
  MachineParams
    { machID = tmID tm
    , machRegisters = map regID (tmRegisters tm)
    }

mkMatchParams ::
     TargetMachine
  -> [MatchData]
  -> [MatchParams]
mkMatchParams tm ms = map (processMatchData tm) ms

processMatchData ::
     TargetMachine
  -> MatchData
  -> MatchParams
processMatchData tm m =
  let i = fromJust $ findInstruction (tmInstructions tm) (mdInstrID m)
      p = fromJust $ findInstrPattern (instrPatterns i) (mdPatternID m)
  in processMatch i p (mdMatch m) (mdMatchID m)

processMatch ::
     Instruction
  -> InstrPattern
  -> Match NodeID
  -> MatchID
  -> MatchParams
processMatch i p m mid =
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
      i_props = instrProps i
      cfg = extractCFG g
      root_label_node_id =
        maybe Nothing ((findFNInMatch m) . getNodeID) (rootInCFG cfg)
      mp =
        MatchParams
          { mInstructionID = instrID i
          , mPatternID = patID p
          , mMatchID = mid
          , mOperationsCovered = findFNsInMatch m (getNodeIDs a_ns)
          , mDataNodesDefined = findFNsInMatch m (getNodeIDs d_def_ns)
          , mDataNodesUsed = findFNsInMatch m (getNodeIDs d_use_ns)
          , mDataNodesUsedByPhis = findFNsInMatch m (getNodeIDs d_use_by_phi_ns)
          , mStateNodesDefined = findFNsInMatch m (getNodeIDs s_def_ns)
          , mStateNodesUsed = findFNsInMatch m (getNodeIDs s_use_ns)
          , mRootLabelNode = root_label_node_id
          , mNonRootLabelNodes = findFNsInMatch m (getNodeIDs l_ref_ns)
          , mConstraints = mapPs2FsInConstraints m (osConstraints $ patOS p)
          , mADDUC = patADDUC p
          , mHasControlNodes = length c_ns > 0
          , mCodeSize = instrCodeSize i_props
          , mLatency = instrLatency i_props
          }
  in mp

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



-- TODO: move this somewhere else

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
