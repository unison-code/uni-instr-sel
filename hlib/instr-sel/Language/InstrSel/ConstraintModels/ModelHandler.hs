{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.ConstraintModels.ModelHandler
  ( mkHighLevelModel
  , lowerHighLevelModel
  )
where

import Language.InstrSel.ConstraintModels.Base
import Language.InstrSel.ConstraintModels.IDs

import Language.InstrSel.Constraints
import qualified Language.InstrSel.Constraints.ConstraintBuilder as C
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Graphalyze
  ( cyclesIn' )
import Language.InstrSel.OpStructures
import Language.InstrSel.Functions
  ( Function (..)
  , fromFunctionName
  )
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , getInstructionFromPatternMatch
  )
import Language.InstrSel.Utils
  ( combinations )
import Language.InstrSel.Utils.Range

import qualified Data.Graph.Inductive as I

import Data.List
  ( elemIndex
  , nub
  , sort
  , sortBy
  )
import qualified Data.Map as M
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  , mapMaybe
  )
import Data.Tuple
  ( swap )



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
  let f_params = mkHLFunctionParams function target
      machine_params = mkHLMachineParams target
      match_params = mkHLMatchParamsList target matches
      ill_m_combs = mkIllegalMatchCombs function target matches
      interch_data = mkInterchangeableData match_params
  in HighLevelModel { hlFunctionParams = f_params
                    , hlMachineParams = machine_params
                    , hlMatchParams = match_params
                    , hlIllegalMatchCombs = ill_m_combs
                    , hlInterchangeableData = interch_data
                    }

mkHLFunctionParams :: Function -> TargetMachine -> HighLevelFunctionParams
mkHLFunctionParams function target =
  let graph = osGraph $ functionOS function
      entry_block = let n = entryBlockNode graph
                    in if isJust n
                       then fromJust n
                       else error $ "mkHLFunctionParams: function has no " ++
                                    "entry block"
      all_ns = getAllNodes graph
      nodeIDsByType f = map getNodeID $
                        filter f all_ns
      block_domsets = computeBlockDomSets graph entry_block
      op_deps = map (\(n, ns) -> (getNodeID n, map getNodeID ns)) $
                computeNonCopyOpDependencies graph
      data_deps = map (\(n, ns) -> (getNodeID n, map getNodeID ns)) $
                  computeDataDependencies graph
      getExecFreq n = fromJust $
                      lookup (nameOfBlock $ getNodeType n)
                             (functionBBExecFreq function)
      bb_params =
        map ( \n -> HighLevelBlockParams
                      { hlBlockName = getNameOfBlockNode n
                      , hlBlockNode = getNodeID n
                      , hlBlockExecFrequency = getExecFreq n
                      }
            )
            (filter isBlockNode (getAllNodes graph))
      state_def_es = map ( \e ->
                           let s = getSourceNode graph e
                               t = getTargetNode graph e
                               s_id = getNodeID s
                               t_id = getNodeID t
                           in if isStateNode s
                              then (t_id, s_id)
                              else (s_id, t_id)
                         ) $
                     filter ( \e -> isStateNode (getSourceNode graph e)
                                    || isStateNode (getTargetNode graph e)
                          ) $
                     filter isDefEdge $
                     getAllEdges graph
      valid_locs =
        -- TODO: do a proper implemention. Right now it's just a quick hack to
        -- prevent the input arguments from being located in a fixed-value
        -- register
        let okay_locs = map locID $
                        filter (isNothing . locValue) $
                        (getAllLocations target)
        in map (\n -> (n, okay_locs)) $
           functionInputs function
      same_locs = osSameLocations $ functionOS function
      int_const_data =
        let ns = filter isValueNodeWithConstValue (getAllNodes graph)
        in nub $
           mapMaybe ( \n -> let nid = getNodeID n
                                dt = getDataTypeOfValueNode n
                                r = intConstValue dt
                            in if isIntConstType dt && isRangeSingleton r
                               then Just (nid, lowerBound r)
                               else Nothing
                     )
                     ns
      value_origin_data =
        let ns = filter isValueNodeWithOrigin (getAllNodes graph)
        in map (\n -> (getNodeID n, fromJust $ originOfValue $ getNodeType n))
               ns
      call_name_data =
        let ns = filter isCallNode (getAllNodes graph)
        in map ( \n -> ( getNodeID n
                       , fromFunctionName $
                         nameOfCall $
                         getNodeType n
                       )
               )
               ns
      used_once_data = filter ( \n ->
                                let es = getDtFlowOutEdges graph n ++
                                         getStFlowOutEdges graph n
                                in length es > 0
                              ) $
                       filter isDatumNode $
                       all_ns
  in HighLevelFunctionParams
       { hlFunOperations = nodeIDsByType isOperationNode
       , hlFunCopies = nodeIDsByType isCopyNode
       , hlFunControlOps = nodeIDsByType isControlNode
       , hlFunData = nodeIDsByType isDatumNode
       , hlFunNonCopyOpDependencies = op_deps
       , hlFunDataDependencies = data_deps
       , hlFunDataUsedAtLeastOnce = map getNodeID used_once_data
       , hlFunStates = nodeIDsByType isStateNode
       , hlFunBlocks = nodeIDsByType isBlockNode
       , hlFunEntryBlock = getNodeID entry_block
       , hlFunBlockDomSets = map convertDomSetN2ID block_domsets
       , hlFunBlockParams = bb_params
       , hlFunStateDefEdges = state_def_es
       , hlFunValidValueLocs = valid_locs
       , hlFunSameValueLocs = same_locs
       , hlFunValueIntConstData = int_const_data
       , hlFunValueOriginData = value_origin_data
       , hlFunCallNameData = call_name_data
       , hlFunConstraints = osConstraints $ functionOS function
       }

mkHLMachineParams :: TargetMachine -> HighLevelMachineParams
mkHLMachineParams target =
  HighLevelMachineParams
    { hlMachineID = tmID target
    , hlMachineLocations = map locID (getAllLocations target)
    }

-- | First constructs a 'HighLevelMatchParams' for each 'PatternMatch'.
mkHLMatchParamsList
  :: TargetMachine
  -> [PatternMatch]
  -> [HighLevelMatchParams]
mkHLMatchParamsList target matches =
  fst $
  foldl ( \(ps, oid) p ->
            let (new_p, next_oid) = mkHLMatchParams target p oid
            in (ps ++ [new_p], next_oid)
        )
        ([], 0)
        matches

mkHLMatchParams
  :: TargetMachine
  -> PatternMatch
  -> OperandID
     -- ^ The next operand ID to use in processing 'Match'.
  -> (HighLevelMatchParams, OperandID)
     -- ^ The created 'HighLevelMatchParams' and the new 'OperandID' to use when
     -- processing the next 'Match'.
mkHLMatchParams target match oid =
  let instr = fromJust $ findInstruction target (pmInstrID match)
  in processMatch instr (pmMatch match) (pmMatchID match) oid

processMatch
  :: Instruction
  -> Match NodeID
  -> MatchID
  -> OperandID
     -- ^ The next operand ID to use in processing 'Match'.
  -> (HighLevelMatchParams, OperandID)
     -- ^ The created 'HighLevelMatchParams' and the new 'OperandID' to use when
     -- processing the next 'Match'.
processMatch instr match mid oid =
  let (instr', match') = enableCopyingForMultUseInputsInPattern instr match
  in processMatch' instr' match' mid oid

-- | If the given match is derived from a pattern graph that has one or more
-- input values of which the pattern makes multiple uses, this function will
-- rewrite the pattern graph and match such that one of the copied values will
-- represent the actual input value. The reason for this is to allow the input
-- value to be appropriately copied before entering the instruction.
enableCopyingForMultUseInputsInPattern
  :: Instruction
  -> Match NodeID
  -> (Instruction, Match NodeID)
enableCopyingForMultUseInputsInPattern instr match =
  let g = osGraph $ instrOS instr
      mult_use_input_vs =
        filter ( \n -> isValueNode n &&
                       not (isValueNodeWithConstValue n) &&
                       length (getDtFlowInEdges g n) == 0 &&
                       length (getDtFlowOutEdges g n) > 1
               ) $
        getAllNodes g
      rewrite old_input (old_i, old_m) =
        let old_os = instrOS old_i
            g0 = osGraph old_os
            e = head $ getDtFlowOutEdges g0 old_input
            cp_n = getTargetNode g0 e
            new_input = let es' = getDtFlowOutEdges g0 cp_n
                        in if length es' > 0
                           then getTargetNode g0 $ head $ es'
                           else error $
                                "enableCopyingForMultUseInputsInPattern: " ++
                                "pattern node " ++ show cp_n ++ " has no " ++
                                "outgoing data-flow edges"
            old_input_id = getNodeID old_input
            new_input_id = getNodeID new_input
            g1 = delNode cp_n g0
            new_valid_locs = map ( \t@(n, locs) -> if n == old_input_id
                                                 then (new_input_id, locs)
                                                 else t
                                 ) $
                             osValidLocations old_os
            new_same_locs = map ( \t@(n1, n2) -> if n1 == old_input_id
                                                 then (new_input_id, n2)
                                                 else if n2 == old_input_id
                                                      then (n1, new_input_id)
                                                      else t
                                ) $
                            osSameLocations old_os
            new_cs = map (C.replaceNodeID old_input_id new_input_id) $
                     osConstraints old_os
            new_os = old_os { osGraph = g1
                            , osValidLocations = new_valid_locs
                            , osSameLocations = new_same_locs
                            , osConstraints = new_cs
                            }
            new_m = toMatch $
                    filter (\m -> pNode m /= getNodeID cp_n) $
                    fromMatch old_m
            new_emit_str = updateNodeInEmitStrTemplate old_input_id
                                                       new_input_id
                                                       (instrEmitString old_i)
            new_i = old_i { instrOS = new_os
                          , instrInputData = (new_input_id:instrInputData old_i)
                          , instrEmitString = new_emit_str
                          }
        in (new_i, new_m)
  in foldr rewrite (instr, match) mult_use_input_vs

processMatch'
  :: Instruction
  -> Match NodeID
  -> MatchID
  -> OperandID
     -- ^ The next operand ID to use in processing 'Match'.
  -> (HighLevelMatchParams, OperandID)
     -- ^ The created 'HighLevelMatchParams' and the new 'OperandID' to use when
     -- processing the next 'Match'.
processMatch' instr match mid oid =
  let os = instrOS instr
      graph = osGraph os
      all_ns = getAllNodes graph
      o_ns = filter isOperationNode all_ns
      d_ns = filter isDatumNode all_ns
      b_ns = filter isBlockNode all_ns
      entry_b = entryBlockNode graph
      b_ns_consumed = if isJust entry_b
                      then filter ( \n -> n /= fromJust entry_b &&
                                          hasAnyPredecessors graph n &&
                                          hasAnySuccessors graph n
                                  )
                                  b_ns
                      else []
      c_ns = filter isControlNode all_ns
      d_def_ns = filter (hasAnyPredecessors graph) d_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_in_ns = filter (\n -> (getNodeID n) `elem` (instrInputData instr)) d_ns
      d_out_ns = filter (\n -> (getNodeID n) `elem` (instrOutputData instr)) $
                 d_ns
      p_ext_ns = nub $
                 instrInputData instr ++
                 instrOutputData instr ++
                 (map getNodeID $ filter isValueNodeWithConstValue d_ns)
      d_int_ns = filter (\n -> (getNodeID n) `notElem` p_ext_ns) d_ns
      d_use_by_phi_ns = filter (\n -> any isPhiNode (getSuccessors graph n))
                               d_use_ns
      b_use_by_phi_ns = map ( getTargetNode graph
                              . head
                              . filter isDefEdge
                              . getOutEdges graph
                            ) $
                        d_use_by_phi_ns
      d_def_by_phi_ns = filter (\n -> any isPhiNode (getPredecessors graph n))
                               d_def_ns
      b_def_by_phi_ns = map ( getSourceNode graph
                              . head
                              . filter isDefEdge
                              . getInEdges graph
                            ) $
                        d_def_by_phi_ns
      pn2oid_maps = zip (nub $ map getNodeID d_ns) [oid..]
      oid2fn_maps = map (\(pn, o) -> (o, findFNInMatch match pn)) $
                    pn2oid_maps
      next_oid = oid + (toOperandID $ length pn2oid_maps)
      getOpIDForPatternDataNodeID n = fromJust $ lookup n pn2oid_maps
      getOpIDForPatternDataNode = getOpIDForPatternDataNodeID . getNodeID
      getOpIDsForPatternDataNodes ns = map getOpIDForPatternDataNode ns
      getMappedFNs pns =
        let fns = findFNsInMatch match $
                  map getNodeID pns
            getSingleFN ns = if length ns == 1
                             then head ns
                             else if length ns == 0
                                  then error $ "processMatch: no mapping"
                                  else error $ "processMatch: multiple mappings"
        in map getSingleFN fns
      valid_locs = map (\(pn, ls) -> (getOpIDForPatternDataNodeID pn, ls)) $
                   osValidLocations $ instrOS instr
      same_locs = map ( \(pn1, pn2) -> ( getOpIDForPatternDataNodeID pn1
                                       , getOpIDForPatternDataNodeID pn2
                                       )
                      ) $
                  osSameLocations $ instrOS instr
      i_props = instrProps instr
  in ( HighLevelMatchParams
         { hlMatchInstructionID = instrID instr
         , hlMatchID = mid
         , hlOperandNodeMaps = oid2fn_maps
         , hlMatchOperationsCovered =  nub $ getMappedFNs o_ns
         , hlMatchOperandsDefined = nub $ getOpIDsForPatternDataNodes d_def_ns
         , hlMatchOperandsUsed = nub $ getOpIDsForPatternDataNodes d_use_ns
         , hlMatchInputOperands = nub $ getOpIDsForPatternDataNodes d_in_ns
         , hlMatchOutputOperands = nub $ getOpIDsForPatternDataNodes d_out_ns
         , hlMatchInternalOperands = nub $ getOpIDsForPatternDataNodes d_int_ns
         , hlMatchValidValueLocs = valid_locs
         , hlMatchSameValueLocs = same_locs
         , hlMatchEntryBlock =
             maybe Nothing
                   (Just . head . findFNInMatch match . getNodeID)
                   entry_b
         , hlMatchSpannedBlocks =
             if isJust entry_b
             then nub $ getMappedFNs b_ns
             else []
         , hlMatchConsumedBlocks = nub $ getMappedFNs b_ns_consumed
         , hlMatchConstraints =
             map ( (replaceThisMatchExprInC mid) .
                   (replaceNodeIDsFromP2FInC match) .
                   (replaceNodeIDsWithOperandIDs pn2oid_maps)
                 ) $
             osConstraints os
         , hlMatchIsPhiInstruction = isInstructionPhi instr
         , hlMatchIsCopyInstruction = isInstructionCopy instr
         , hlMatchIsKillInstruction = isInstructionKill instr
         , hlMatchIsNullInstruction = isInstructionNull instr
         , hlMatchHasControlFlow = length c_ns > 0
         , hlMatchCodeSize = instrCodeSize i_props
         , hlMatchLatency = instrLatency i_props
         , hlMatchOperandsUsedByPhis =
             zip (getMappedFNs b_use_by_phi_ns)
                 (getOpIDsForPatternDataNodes d_use_by_phi_ns)
         , hlMatchOperandsDefinedByPhis =
             zip (getMappedFNs b_def_by_phi_ns)
                 (getOpIDsForPatternDataNodes d_def_by_phi_ns)
         , hlMatchEmitStrNodeMaplist =
             computeEmitStrNodeMaps (instrEmitString instr) pn2oid_maps match
         }
     , next_oid
     )

-- | Computes the emit string node ID mappings, which is done as follows: if the
-- assembly string part contains a node ID, take the corresponding operand ID if
-- such a mapping exist, or the node ID from the corresponding node in the
-- function graph. Otherwise use 'Nothing'.
computeEmitStrNodeMaps
  :: EmitStringTemplate
  -> [(NodeID, OperandID)]
  -> Match NodeID
  -> [[Maybe (Either OperandID NodeID)]]
computeEmitStrNodeMaps (EmitStringTemplate ts) pn2oid_maps m =
  map (map f) ts
  where f (ESVerbatim            _) = Nothing
        f (ESLocationOfValueNode n) = Just $ getOperandOrNodeID n
        f (ESIntConstOfValueNode n) = Just $ getOperandOrNodeID n
        f (ESNameOfBlockNode     n) = Just $ getOperandOrNodeID n
        f (ESBlockOfValueNode    n) = Just $ getOperandOrNodeID n
        f (ESLocalTemporary      _) = Nothing
        f (ESFuncOfCallNode      n) = Just $ getOperandOrNodeID n
        getOperandOrNodeID pn =
          let oid = lookup pn pn2oid_maps
          in if isJust oid
             then Left $ fromJust oid
             else Right $ getSingleFN pn
        getSingleFN pn =
          let fn = findFNInMatch m pn
          in if length fn == 1
             then head fn
             else if length fn == 0
                  then error "computeEmitStrNodeMaps: no mapping"
                  else error "computeEmitStrNodeMaps: multiple mappings"

-- | Replaces occurrences of 'ThisMatchExpr' in a constraint with the given
-- match ID.
replaceThisMatchExprInC :: MatchID -> Constraint -> Constraint
replaceThisMatchExprInC mid c =
  let def_r = mkDefaultReconstructor
      mkMatchExpr _ ThisMatchExpr = AMatchIDExpr mid
      mkMatchExpr r expr = (mkMatchExprF def_r) r expr
      new_r = def_r { mkMatchExprF = mkMatchExpr }
  in apply new_r c

-- | Replaces the node IDs used in the constraint from matched pattern node IDs
-- to the corresponding function node IDs.
replaceNodeIDsFromP2FInC :: Match NodeID -> Constraint -> Constraint
replaceNodeIDsFromP2FInC m c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ (ANodeIDExpr n) =
        let fn = findFNInMatch m n
        in if length fn == 1
           then ANodeIDExpr (head fn)
           else if length fn == 0
                then error "replaceNodeIDsFromP2FInC: no mapping"
                else error "replaceNodeIDsFromP2FInC: multiple mappings"
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

-- | Converts any node IDs appearing in a constraint that has an operand ID with
-- the corresponding operand ID.
replaceNodeIDsWithOperandIDs
  :: [(NodeID, OperandID)]
  -> Constraint
  -> Constraint
replaceNodeIDsWithOperandIDs maps c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ expr@(ANodeIDExpr nid) =
        let oid = lookup nid maps
        in if isJust oid
           then NodeSelectedForOperandExpr $ AnOperandIDExpr $ fromJust oid
           else expr
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

-- | Computes the corresponding low-level version of a high-level CP model
-- instance.
lowerHighLevelModel :: HighLevelModel -> ArrayIndexMaplists -> LowLevelModel
lowerHighLevelModel model ai_maps =
  let getAIForOperationNodeID nid =
        fromJust $ findAIWithOperationNodeID ai_maps nid
      getAIForDatumNodeID nid = fromJust $ findAIWithDatumNodeID ai_maps nid
      getAIForBlockNodeID nid = fromJust $ findAIWithBlockNodeID ai_maps nid
      getAIForOperandID oid = fromJust $ findAIWithOperandID ai_maps oid
      getAIForMatchID mid = fromJust $ findAIWithMatchID ai_maps mid
      getAIForLocationID lid = fromJust $ findAIWithLocationID ai_maps lid
      pairWithAI get_ai_f nids = map (\nid -> (get_ai_f nid, nid)) nids
      sortByAI get_ai_f nids =
        map snd ( sortBy (\(ai1, _) (ai2, _) -> compare ai1 ai2)
                         (pairWithAI get_ai_f nids)
                )
      f_params = hlFunctionParams model
      tm_params = hlMachineParams model
      m_params = sortByAI (getAIForMatchID . hlMatchID)
                          (hlMatchParams model)
      operands = sortByAI (getAIForOperandID . fst)
                          (concatMap (hlOperandNodeMaps) m_params)
      blocks = sortByAI getAIForBlockNodeID $ hlFunBlocks f_params
      in_def_edges =
        concatMap ( \m ->
                    concatMap ( \b ->
                                if hlMatchIsPhiInstruction m
                                then let ops = map snd $
                                               filter ((==) b . fst) $
                                               hlMatchOperandsUsedByPhis m
                                     in map (\o -> (hlMatchID m, b, o)) ops
                                else []
                              ) $
                    blocks
                  ) $
        m_params
      out_def_edges =
        concatMap ( \m ->
                    concatMap ( \b ->
                                if hlMatchIsPhiInstruction m
                                then let ops = map snd $
                                               filter ((==) b . fst) $
                                               hlMatchOperandsDefinedByPhis m
                                     in map (\o -> (hlMatchID m, b, o)) ops
                                else []
                              ) $
                    blocks
                  ) $
        m_params
      f_valid_locs =
        concatMap (\(n, ls) -> map (\l -> (n, l)) ls) $
        hlFunValidValueLocs f_params
      f_same_locs = hlFunSameValueLocs f_params
      m_valid_locs =
        concatMap ( \m -> concatMap ( \(o, ls) ->
                                      map (\l -> (hlMatchID m, o, l)) ls
                                    ) $
                          hlMatchValidValueLocs m
                  ) $
        m_params
      m_same_locs =
        concatMap ( \m -> map (\(o1, o2) -> (hlMatchID m, o1, o2)) $
                          hlMatchSameValueLocs m
                  ) $
        m_params
  in LowLevelModel
       { llFunNumOperations = toInteger $ length $ hlFunOperations f_params
       , llFunNumData = toInteger $ length $ hlFunData f_params
       , llFunNumBlocks = toInteger $ length $ hlFunBlocks f_params
       , llFunCopies = map getAIForOperationNodeID (hlFunCopies f_params)
       , llFunControlOps =
           map getAIForOperationNodeID (hlFunControlOps f_params)
       , llFunStates = map getAIForDatumNodeID (hlFunStates f_params)
       , llFunNonCopyOpDependencies =
           map (\(_, ns) -> map getAIForOperationNodeID ns) $
           sortByAI (getAIForOperationNodeID . fst) $
           hlFunNonCopyOpDependencies f_params
       , llFunDataDependencies =
           map (\(_, ns) -> map getAIForDatumNodeID ns) $
           sortByAI (getAIForDatumNodeID . fst) $
           hlFunDataDependencies f_params
       , llFunDataUsedAtLeastOnce = map getAIForDatumNodeID $
                                    hlFunDataUsedAtLeastOnce f_params
       , llFunValidValueLocs =
           map (\(d, l) -> (getAIForDatumNodeID d, getAIForLocationID l))$
           f_valid_locs
       , llFunSameValueLocs =
           map (\(d1, d2) -> (getAIForDatumNodeID d1, getAIForDatumNodeID d2))$
           f_same_locs
       , llFunEntryBlock = getAIForBlockNodeID $ hlFunEntryBlock f_params
       , llFunBlockDomSets =
           map (\d -> map getAIForBlockNodeID (domSet d)) $
           sortByAI (getAIForBlockNodeID . domNode) $
           hlFunBlockDomSets f_params
       , llFunBBExecFreqs =
           map hlBlockExecFrequency $
           sortByAI (getAIForBlockNodeID . hlBlockNode) $
           hlFunBlockParams f_params
       , llFunStateDefEdges =
           map (\(b, s) -> (getAIForBlockNodeID b, getAIForDatumNodeID s)) $
           hlFunStateDefEdges f_params
       , llFunConstraints =
           map (replaceIDsWithArrayIndexes ai_maps) (hlFunConstraints f_params)
       , llNumLocations = toInteger $ length $ hlMachineLocations tm_params
       , llNumMatches = toInteger $ length m_params
       , llNumOperands = toInteger $ length $ map fst $ operands
       , llOperandAlternatives = map (map getAIForDatumNodeID . snd) operands
       , llMatchOperationsCovered =
           map (map getAIForOperationNodeID . hlMatchOperationsCovered) $
           m_params
       , llMatchOperandsDefined =
           map (map getAIForOperandID . hlMatchOperandsDefined) m_params
       , llMatchOperandsUsed =
           map (map getAIForOperandID . hlMatchOperandsUsed) m_params
       , llMatchExternalOperands =
           map ( \p ->
                 map getAIForOperandID $
                 nub $
                 hlMatchInputOperands p ++ hlMatchOutputOperands p
               )
               m_params
       , llMatchInternalOperands =
           map (map getAIForOperandID . hlMatchInternalOperands) m_params
       , llMatchValidValueLocs =
           map ( \(m, o, l) -> ( getAIForMatchID m
                               , getAIForOperandID o
                               , getAIForLocationID l
                               )
               ) $
           m_valid_locs
       , llMatchSameValueLocs =
           map ( \(m, o1, o2) -> ( getAIForMatchID m
                                 , getAIForOperandID o1
                                 , getAIForOperandID o2
                                 )
               ) $
           m_same_locs
       , llMatchEntryBlocks =
           map (maybe Nothing (Just . getAIForBlockNodeID)) $
           map hlMatchEntryBlock m_params
       , llMatchSpannedBlocks = map (map getAIForBlockNodeID) $
                                map hlMatchSpannedBlocks $
                                m_params
       , llMatchConsumedBlocks = map (map getAIForBlockNodeID) $
                                 map hlMatchConsumedBlocks $
                                 m_params
       , llMatchInputDefinitionEdges =
           map ( \(m, b, o) -> ( getAIForMatchID m
                               , getAIForBlockNodeID b
                               , getAIForOperandID o
                               )
               ) $
           in_def_edges
       , llMatchOutputDefinitionEdges =
           map ( \(m, b, o) -> ( getAIForMatchID m
                               , getAIForBlockNodeID b
                               , getAIForOperandID o
                               )
               ) $
           out_def_edges
       , llMatchCodeSizes = map hlMatchCodeSize m_params
       , llMatchLatencies = map hlMatchLatency m_params
       , llMatchPhiInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsPhiInstruction $
           m_params
       , llMatchCopyInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsCopyInstruction $
           m_params
       , llMatchKillInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsKillInstruction $
           m_params
       , llMatchNullInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsNullInstruction $
           m_params
       , llMatchConstraints =
           map (map (replaceIDsWithArrayIndexes ai_maps)) $
           map hlMatchConstraints $
           m_params
       , llMatchInstructionIDs = map hlMatchInstructionID m_params
       , llIllegalMatchCombs = map (map getAIForMatchID) $
                               hlIllegalMatchCombs model
       , llInterchangeableData =
           map (map getAIForDatumNodeID) (hlInterchangeableData model)
       , llTMID = hlMachineID tm_params
       }

-- | Converts any IDs appearing in a constraint with the corresponding array
-- index.
replaceIDsWithArrayIndexes :: ArrayIndexMaplists -> Constraint -> Constraint
replaceIDsWithArrayIndexes ai_maps c =
  let getAIForAnyNodeID nid = fromJust $ findAIWithAnyNodeID ai_maps nid
      getAIForMatchID mid = fromJust $ findAIWithMatchID ai_maps mid
      getAIForOperandID oid = fromJust $ findAIWithOperandID ai_maps oid
      getAIForInstructionID iid =
        fromJust $ findAIWithInstructionID ai_maps iid
      def_r = mkDefaultReconstructor
      mkNodeExpr _ (ANodeIDExpr nid) =
        ANodeArrayIndexExpr $ getAIForAnyNodeID nid
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      mkMatchExpr _ (AMatchIDExpr nid) =
        AMatchArrayIndexExpr $ getAIForMatchID nid
      mkMatchExpr r expr = (mkMatchExprF def_r) r expr
      mkOperandExpr _ (AnOperandIDExpr oid) =
        AnOperandArrayIndexExpr $ getAIForOperandID oid
      mkOperandExpr r expr = (mkOperandExprF def_r) r expr
      mkInstructionExpr _ (AnInstructionIDExpr nid) =
        AnInstructionArrayIndexExpr $ getAIForInstructionID nid
      mkInstructionExpr r expr = (mkInstructionExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr
                    , mkOperandExprF = mkOperandExpr
                    , mkMatchExprF = mkMatchExpr
                    , mkInstructionExprF = mkInstructionExpr
                    }
  in apply new_r c

findAIWithOperationNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithOperationNodeID ai_maps =
  findArrayIndexInList (ai2OperationNodeIDs ai_maps)

findAIWithDatumNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithDatumNodeID ai_maps = findArrayIndexInList (ai2DatumNodeIDs ai_maps)

findAIWithOperandID :: ArrayIndexMaplists -> OperandID -> Maybe ArrayIndex
findAIWithOperandID ai_maps = findArrayIndexInList (ai2OperandIDs ai_maps)

findAIWithBlockNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithBlockNodeID ai_maps = findArrayIndexInList (ai2BlockNodeIDs ai_maps)

findAIWithAnyNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithAnyNodeID ai_maps nid =
  let lists = [ ai2OperationNodeIDs ai_maps
              , ai2DatumNodeIDs ai_maps
              , ai2BlockNodeIDs ai_maps
              ]
      matching_lists = filter (nid `elem`) lists
  in findArrayIndexInList (head matching_lists) nid

findAIWithMatchID :: ArrayIndexMaplists -> MatchID -> Maybe ArrayIndex
findAIWithMatchID ai_maps = findArrayIndexInList (ai2MatchIDs ai_maps)

findAIWithLocationID :: ArrayIndexMaplists -> LocationID -> Maybe ArrayIndex
findAIWithLocationID ai_maps = findArrayIndexInList (ai2LocationIDs ai_maps)

findAIWithInstructionID
  :: ArrayIndexMaplists
  -> InstructionID
  -> Maybe ArrayIndex
findAIWithInstructionID ai_maps =
  findArrayIndexInList (ai2InstructionIDs ai_maps)

findArrayIndexInList :: (Eq a) => [a] -> a -> Maybe ArrayIndex
findArrayIndexInList ai_list nid =
  let index = nid `elemIndex` ai_list
  in if isJust index
     then Just $ toArrayIndex $ fromJust index
     else Nothing

computeBlockDomSets :: Graph -> Node -> [DomSet Node]
computeBlockDomSets g root =
  let cfg = extractCFG g
  in computeDomSets cfg root

-- | Computes the dependency sets for all non-copy operations. An operation @o@,
-- which is uses data defined by operations @o1@, ..., @on@, depends on @o1@,
-- ..., @on@. If any operation is a copy, then the operation defining the data
-- used by the copy is applied instead.
computeNonCopyOpDependencies :: Graph -> [(Node, [Node])]
computeNonCopyOpDependencies g0 =
  let g1 = extractSSAG g0
      all_ns = getAllNodes g1
      -- Remove all copy operations and data (but keep the dependencies)
      all_cps = filter isCopyNode all_ns
      g2 = foldr delNodeKeepEdges g1 all_cps
      all_data = filter isDatumNode all_ns
      g3 = foldr delNodeKeepEdges g2 all_data
      -- Compute op dependencies
      all_ops = filter isOperationNode all_ns
      deps = map (\n -> (n, getPredecessors g3 n)) $
             all_ops
  in deps

-- | Computes the dependency sets for all data. A datum @d@, which is defined by
-- an operation @o@ that uses data @d1@, ..., @dn@, depends on @d1@, ..., @d2@.
computeDataDependencies :: Graph -> [(Node, [Node])]
computeDataDependencies g0 =
  let g1 = extractSSAG g0
      all_ds = filter isDatumNode $
               getAllNodes g1
      deps = map ( \n -> ( n
                         , concatMap (getPredecessors g1) $
                           getPredecessors g1 n
                         )
                 ) $
             all_ds
  in deps

-- | Finds combinations of matches that are illegal, meaning they will yield a
-- cyclic data dependency if all are selected.
--
-- The cycles are found by first constructing a dependency graph for all
-- matches, where each node is a match and each edge represents a data
-- dependency between the two matches. Matches derived from generic phi patterns
-- are not included in the dependency graph (as those would always yield a cycle
-- which is not actually a cyclic data dependency), and matches which are known
-- to never be the root cause of a cycle -- that is, matches derived from null
-- instructions and copy instructions -- are also not included. To reduce the
-- time it takes to find all cycles, all SIMD matches are also removed. This is
-- because such matches often yield many cycles in large functions, and the CP
-- solver will forbid such combinations anyhow.
mkIllegalMatchCombs
  :: Function
  -> TargetMachine
  -> [PatternMatch]
  -> [[MatchID]]
mkIllegalMatchCombs function target matches =
  let g0 = I.mkGraph (zip [0..] matches) [] :: I.Gr PatternMatch ()
      getExternalDataNodes i pm fg =
        let pg = osGraph $ instrOS i
            input_ns = filter ( \n -> isValueNode n &&
                                      not (hasAnyPredecessors pg n)
                              ) $
                       getAllNodes pg
            output_nids = instrOutputData i
        in ( concatMap (findNodesWithNodeID fg) $
             concat $
             findFNsInMatch (pmMatch pm) (map getNodeID input_ns)
           , concatMap (findNodesWithNodeID fg) $
             concat $
             findFNsInMatch (pmMatch pm) output_nids
           )
      getMatchNodeFromID g m = head $
                               map fst $
                               filter ( \(_, m') ->
                                        pmMatchID m' == pmMatchID m
                                      ) $
                               I.labNodes g
      addDependencies [m1, m2] g' =
        let fg = osGraph $ functionOS function
            i1 = getInstructionFromPatternMatch target m1
            i2 = getInstructionFromPatternMatch target m2
            (m1_in_vs, m1_out_vs)  = getExternalDataNodes i1 m1 fg
            (m2_in_vs, m2_out_vs)  = getExternalDataNodes i2 m2 fg
            n1 = getMatchNodeFromID g' m1
            n2 = getMatchNodeFromID g' m2
            g'' = if any (\n -> n `elem` m2_in_vs) m1_out_vs
                  then I.insEdge (n1, n2, ()) g'
                  else g'
            g''' = if any (\n -> n `elem` m1_in_vs) m2_out_vs
                   then I.insEdge (n2, n1, ()) g''
                   else g''
        in g'''
      addDependencies _ _ =
        error $ "mkIllegalMatchCombs: illegal list length of first argument"
      g1 = foldr addDependencies g0 (combinations 2 matches)
      delNodeKeepDeps n g =
        let in_es = I.inn g n
            out_es = I.out g n
            g' = I.delNode n g
            g'' = foldr ( \(src, _, _) h ->
                          foldr ( \(_, trg, _) h'
                                  -> let e = (src, trg, ())
                                     in if not (I.hasLEdge h' e)
                                        then I.insEdge e h'
                                        else h'
                                )
                                h
                                out_es
                        )
                        g'
                        in_es
        in g''
      excludeMatches m g =
        let i = getInstructionFromPatternMatch target m
            n = getMatchNodeFromID g m
        in if isInstructionPhi i || isInstructionSimd i
           then I.delNode n g
           else if isInstructionNull i || isInstructionCopy i
                then delNodeKeepDeps n g
                else g
      g2 = foldr excludeMatches g1 matches
      forbidden_combs = map (map pmMatchID . map (fromJust . I.lab g2)) $
                        map init $ -- Remove last element as it is the same as
                                   -- the first element
                        cyclesIn' g2
  in forbidden_combs

-- | Finds the sets of data that are interchangeable. Data are interchangeable
-- if they can be globally swapped within a solution without changing the
-- program semantics.
--
-- This first gathers all sets of data that can be used, removing those that
-- contain a datum that appears as the only element in some set.
mkInterchangeableData :: [HighLevelMatchParams] -> [[NodeID]]
mkInterchangeableData params =
  let all_alt_uses =
        concatMap ( \m ->
                    let use_ops = hlMatchOperandsUsed m
                    in map (\p -> fromJust $ lookup p (hlOperandNodeMaps m)) $
                       use_ops
                  ) $
        params
      unary_uses_set = M.fromList $
                       map swap $
                       zip (repeat True) $
                       nub $
                       concat $
                       filter (\l -> length l == 1) $
                       all_alt_uses
      interch_data = nub $
                     map sort $
                     filter (not . any (\n -> M.member n unary_uses_set)) $
                     all_alt_uses
  in interch_data
