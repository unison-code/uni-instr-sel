--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ConstraintModels.ModelHandler
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Functions for constructing, lowering, and raising CP model instances.
--
--------------------------------------------------------------------------------

module Language.InstSel.ConstraintModels.ModelHandler
  ( mkHighLevelModel
  , lowerHighLevelModel
  )
where

import Language.InstSel.Constraints
import Language.InstSel.ConstraintModels.Base
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import Language.InstSel.Functions
  ( Function (..) )
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.PatternMatching
  ( PatternMatch (..) )

import Data.List
  ( elemIndex
  , sortBy
  )
import Data.Maybe
  ( fromJust )



-------------
-- Functions
-------------

-- | Constructs the corresponding high-level CP model instance from the given
-- target machine, function graph, and pattern matches.
mkHighLevelModel
  :: Function
  -> TargetMachine
  -> [PatternMatch]
  -> HighLevelModel
mkHighLevelModel function target matches =
  HighLevelModel
    { hlFunctionParams = mkHLFunctionParams function
    , hlMachineParams = mkHLMachineParams target
    , hlMatchParams = mkHLMatchParamsList target matches
    }

mkHLFunctionParams :: Function -> HighLevelFunctionParams
mkHLFunctionParams function =
  let graph = osGraph $ functionOS function
      nodeIDsByType f = getNodeIDs $ filter f (getAllNodes graph)
      essential_op_nodes =
        filter
          (\n -> isOperationNode n && (not $ isCopyNode n))
          (getAllNodes graph)
      cfg = extractCFG graph
      dp_edge_data =
        map
          ( \e ->
              ( getNodeID $ getSourceNode graph e
              , getNodeID $ getTargetNode graph e
              )
          )
          (filter isDefPlaceEdge (getAllEdges graph))
      getExecFreq n =
        fromJust $
          lookup
            (bbLabel $ getNodeType n)
            (functionBBExecFreq function)
  in HighLevelFunctionParams
       { hlFunOpNodes = nodeIDsByType isOperationNode
       , hlFunEssentialOpNodes = map getNodeID essential_op_nodes
       , hlFunDataNodes = nodeIDsByType isDataNode
       , hlFunStateNodes= nodeIDsByType isStateNode
       , hlFunLabelNodes= computeLabelDoms cfg
       , hlFunDefPlaceEdges = dp_edge_data
       , hlFunRootLabelNode = getNodeID $ fromJust $ rootInCFG cfg
       , hlFunBasicBlockParams =
           map
             ( \n -> HighLevelBasicBlockParams
                       { hlBBLabel = (bbLabel $ getNodeType n)
                       , hlBBLabelNode = (getNodeID n)
                       , hlBBExecFrequency = getExecFreq n
                       }
             )
             (filter isLabelNode (getAllNodes graph))
       , hlFunConstraints = osConstraints $ functionOS function
       }

-- | Computes the dominator sets concerning only the label nodes. It is assumed
-- there exists a single label which acts as the root, which is the label node
-- with no predecessors. It is also assumed that every other label node can be
-- reached from the root.
computeLabelDoms
  :: Graph
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

mkHLMachineParams :: TargetMachine -> HighLevelMachineParams
mkHLMachineParams target =
  HighLevelMachineParams
    { hlMachineID = tmID target
    , hlMachineRegisters = map regID (tmRegisters target)
    }

mkHLMatchParamsList :: TargetMachine -> [PatternMatch] -> [HighLevelMatchParams]
mkHLMatchParamsList target matches = map (mkHLMatchParams target) matches

mkHLMatchParams :: TargetMachine -> PatternMatch -> HighLevelMatchParams
mkHLMatchParams target match =
  let instr =
        fromJust $ findInstruction (tmInstructions target) (pmInstrID match)
      pattern =
        fromJust $ findInstrPattern (instrPatterns instr) (pmPatternID match)
  in processMatch instr pattern (pmMatch match) (pmMatchID match)

processMatch
  :: Instruction
  -> InstrPattern
  -> Match NodeID
  -> MatchID
  -> HighLevelMatchParams
processMatch instr pattern match mid =
  let graph = osGraph $ patOS pattern
      ns = getAllNodes graph
      a_ns = filter isOperationNode ns
      d_ns = filter isDataNode ns
      s_ns = filter isStateNode ns
      l_ns = filter isLabelNode ns
      c_ns = filter isControlNode ns
      d_def_ns = filter (hasAnyPredecessors graph) d_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_use_by_phi_ns = filter
                          (\n -> any isPhiNode (getSuccessors graph n))
                          d_use_ns
      s_def_ns = filter (hasAnyPredecessors graph) s_ns
      s_use_ns = filter (hasAnySuccessors graph) s_ns
      l_ref_ns = filter (hasAnyPredecessors graph) l_ns
      i_props = instrProps instr
      cfg = extractCFG graph
      root_label_node_id =
        maybe Nothing ((findFNInMatch match) . getNodeID) (rootInCFG cfg)
      asm_maps = computeAsmStrNodeMaps (patAsmStrTemplate pattern) match
  in HighLevelMatchParams
       { hlMatchInstructionID = instrID instr
       , hlMatchPatternID = patID pattern
       , hlMatchID = mid
       , hlMatchOperationsCovered = findFNsInMatch match (getNodeIDs a_ns)
       , hlMatchDataNodesDefined = findFNsInMatch match (getNodeIDs d_def_ns)
       , hlMatchDataNodesUsed = findFNsInMatch match (getNodeIDs d_use_ns)
       , hlMatchDataNodesUsedByPhis =
           findFNsInMatch match (getNodeIDs d_use_by_phi_ns)
       , hlMatchStateNodesDefined = findFNsInMatch match (getNodeIDs s_def_ns)
       , hlMatchStateNodesUsed = findFNsInMatch match (getNodeIDs s_use_ns)
       , hlMatchRootLabelNode = root_label_node_id
       , hlMatchNonRootLabelNodes = findFNsInMatch match (getNodeIDs l_ref_ns)
       , hlMatchConstraints =
           mapPs2FsInConstraints match (osConstraints $ patOS pattern)
       , hlMatchADDUC = patADDUC pattern
       , hlMatchHasControlNodes = length c_ns > 0
       , hlMatchCodeSize = instrCodeSize i_props
       , hlMatchLatency = instrLatency i_props
       , hlMatchAsmStrNodeMaplist = asm_maps
       }

-- | Computes the assembly string node ID mappings, which is done as follows: if
-- the assembly string part contains a node ID, take the node ID from the
-- corresponding node in the function graph. Otherwise use @Nothing@.
computeAsmStrNodeMaps
  :: AssemblyStringTemplate
  -> Match NodeID
  -> [Maybe NodeID]
computeAsmStrNodeMaps t m =
  map f (asmStrParts t)
  where f (ASVerbatim _) = Nothing
        f (ASRegisterOfDataNode n) = findFNInMatch m n
        f (ASImmValueOfDataNode n) = findFNInMatch m n
        f (ASBBLabelOfLabelNode n) = findFNInMatch m n
        f (ASBBLabelOfDataNode  n) = findFNInMatch m n

-- | Computes the corresponding low-level version of a high-level CP model
-- instance.
lowerHighLevelModel :: HighLevelModel -> ArrayIndexMaplists -> LowLevelModel
lowerHighLevelModel model ai_maps =
  let getAI ai_list nid = toArrayIndex $ fromJust $ nid `elemIndex` ai_list
      pairWithAI get_ai_f nids = map (\nid -> (get_ai_f nid, nid)) nids
      sortByAI get_ai_f nids =
        map
          snd
          ( sortBy
              (\(ai1, _) (ai2, _) -> compare ai1 ai2)
              (pairWithAI get_ai_f nids)
          )
      getAIFromOpNodeID nid = getAI (ai2OpNodeIDs ai_maps) nid
      getAIFromDataNodeID nid = getAI (ai2DataNodeIDs ai_maps) nid
      getAIFromStateNodeID nid = getAI (ai2StateNodeIDs ai_maps) nid
      getAIFromLabelNodeID nid = getAI (ai2LabelNodeIDs ai_maps) nid
      getAIFromMatchID mid = getAI (ai2MatchIDs ai_maps) mid
      f_params = hlFunctionParams model
      tm_params = hlMachineParams model
      m_params = sortByAI (getAIFromMatchID . hlMatchID) (hlMatchParams model)
  in LowLevelModel
       { llNumFunOpNodes = toInteger $ length $ hlFunOpNodes f_params
       , llNumFunDataNodes = toInteger $ length $ hlFunDataNodes f_params
       , llNumFunStateNodes = toInteger $ length $ hlFunStateNodes f_params
       , llNumFunLabelNodes = toInteger $ length $ hlFunLabelNodes f_params
       , llFunRootLabel = getAIFromLabelNodeID $ hlFunRootLabelNode f_params
       , llFunDomsets =
           map
             (\d -> map getAIFromLabelNodeID (domSet d))
             ( sortByAI
                 (getAIFromLabelNodeID . domNode)
                 (hlFunLabelNodes f_params)
             )
       , llFunBBExecFreqs =
           map
             hlBBExecFrequency
             ( sortByAI
                 (getAIFromLabelNodeID . hlBBLabelNode)
                 (hlFunBasicBlockParams f_params)
             )
       , llFunDataNodeLabelDefs =
           let all_data_nodes = hlFunDataNodes f_params
               maybe_defs =
                 map
                   (\n -> (n, lookup n (hlFunDefPlaceEdges f_params)))
                   all_data_nodes
           in map
                (maybe Nothing (Just . getAIFromLabelNodeID))
                (map snd (sortByAI fst maybe_defs))
       , llFunStateNodeLabelDefs =
           let all_state_nodes = hlFunStateNodes f_params
               maybe_defs =
                 map
                   (\n -> (n, lookup n (hlFunDefPlaceEdges f_params)))
                   all_state_nodes
           in map
                (maybe Nothing (Just . getAIFromLabelNodeID))
                (map snd (sortByAI fst maybe_defs))
       , llFunEssentialOpNodes = map getAIFromOpNodeID (hlFunOpNodes f_params)
       , llFunConstraints =
           map (replaceIDWithArrayIndex ai_maps) (hlFunConstraints f_params)
       , llNumRegisters = toInteger $ length $ hlMachineRegisters tm_params
       , llNumMatches = toInteger $ length m_params
       , llMatchOpNodesCovered =
           map
             (\m -> map getAIFromOpNodeID (hlMatchOperationsCovered m))
             m_params
       , llMatchDataNodesDefined =
           map
             (\m -> map getAIFromDataNodeID (hlMatchDataNodesDefined m))
             m_params
       , llMatchStateNodesDefined =
           map
             (\m -> map getAIFromStateNodeID (hlMatchStateNodesDefined m))
             m_params
       , llMatchDataNodesUsed =
           map
             (\m -> map getAIFromDataNodeID (hlMatchDataNodesUsed m))
             m_params
       , llMatchStateNodesUsed =
           map
             (\m -> map getAIFromStateNodeID (hlMatchStateNodesUsed m))
             m_params
       , llMatchRootLabelNode =
           map
             (maybe Nothing (Just . getAIFromLabelNodeID))
             (map hlMatchRootLabelNode m_params)
       , llMatchNonRootLabelNodes =
           map
             (map getAIFromLabelNodeID)
             (map hlMatchNonRootLabelNodes m_params)
       , llMatchCodeSizes = map hlMatchCodeSize m_params
       , llMatchLatencies = map hlMatchLatency m_params
       , llMatchADDUCs = map hlMatchADDUC m_params
       , llMatchConstraints =
           map
             (map (replaceIDWithArrayIndex ai_maps))
             (map hlMatchConstraints m_params)
       }

-- | Converts any ID appearing in a constraint with the corresponding array
-- index.
replaceIDWithArrayIndex :: ArrayIndexMaplists -> Constraint -> Constraint
replaceIDWithArrayIndex = undefined







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

replaceInMatchExpr
  :: Match NodeID
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

replaceInInstructionExpr
  :: Match NodeID
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
