{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
  hiding
  ( computeDomSets )
import qualified Language.InstrSel.Graphs as G
  ( computeDomSets )
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
  , getInstrPatternFromPatternMatch
  )
import Language.InstrSel.Utils
  ( combinations )
import Language.InstrSel.Utils.Range

import qualified Data.Graph.Inductive as I

import Data.List
  ( elemIndex
  , nub
  , groupBy
  , sortBy
  )
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  , mapMaybe
  )



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
    { hlFunctionParams = mkHLFunctionParams function target
    , hlMachineParams = mkHLMachineParams target
    , hlMatchParams = mkHLMatchParamsList target matches
    , hlIllegalMatchCombs = mkIllegalMatchCombs function target matches
    }

mkHLFunctionParams :: Function -> TargetMachine -> HighLevelFunctionParams
mkHLFunctionParams function target =
  let graph = osGraph $ functionOS function
      entry_block = fromJust $ osEntryBlockNode $ functionOS function
      all_ns = getAllNodes graph
      nodeIDsByType f = map getNodeID $
                        filter f all_ns
      domsets = computeDomSets graph entry_block
      getExecFreq n = fromJust $
                      lookup (nameOfBlock $ getNodeType n)
                             (functionBBExecFreq function)
      bb_params =
        map ( \n -> HighLevelBlockParams
                      { hlBlockName = getNameOfBlockNode n
                      , hlBlockNode = getNodeID n
                      , hlBlockExecFrequency = getExecFreq n
                      , hlBlockIsBEBlock = (getNameOfBlockNode n)
                                           `elem`
                                           (functionBEBlocks function)
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
                        tmLocations target
        in map (\n -> (n, okay_locs)) $
           functionInputs function
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
      cp_ns = filter isCopyNode all_ns
      cp_groups = filter (\ns -> length ns > 1) $
                  groupBy ( \n1 n2 ->
                            let d1 = head $ getPredecessors graph n1
                                d2 = head $ getPredecessors graph n2
                            in d1 == d2
                          )
                          cp_ns
      interch_data = map (map (head . getSuccessors graph)) cp_groups
  in HighLevelFunctionParams
       { hlFunOperations = nodeIDsByType isOperationNode
       , hlFunCopies = nodeIDsByType isCopyNode
       , hlFunControlOps = nodeIDsByType isControlNode
       , hlFunData = nodeIDsByType isDatumNode
       , hlFunInterchangeableData = map (nub . map getNodeID) interch_data
       , hlFunStates = nodeIDsByType isStateNode
       , hlFunBlocks = nodeIDsByType isBlockNode
       , hlFunEntryBlock = entry_block
       , hlFunBlockDomSets = map convertDomSetN2ID domsets
       , hlFunBlockParams = bb_params
       , hlFunStateDefEdges = state_def_es
       , hlFunValidValueLocs = valid_locs
       , hlFunValueIntConstData = int_const_data
       , hlFunValueOriginData = value_origin_data
       , hlFunCallNameData = call_name_data
       , hlFunConstraints = osConstraints $ functionOS function
       }

mkHLMachineParams :: TargetMachine -> HighLevelMachineParams
mkHLMachineParams target =
  HighLevelMachineParams
    { hlMachineID = tmID target
    , hlMachineLocations = map locID (tmLocations target)
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
  let instr = fromJust $ findInstruction (tmInstructions target)
                                         (pmInstrID match)
      pat = fromJust $ findInstrPattern (instrPatterns instr)
                                        (pmPatternID match)
  in processMatch instr pat (pmMatch match) (pmMatchID match) oid

processMatch
  :: Instruction
  -> InstrPattern
  -> Match NodeID
  -> MatchID
  -> OperandID
     -- ^ The next operand ID to use in processing 'Match'.
  -> (HighLevelMatchParams, OperandID)
     -- ^ The created 'HighLevelMatchParams' and the new 'OperandID' to use when
     -- processing the next 'Match'.
processMatch instr pat match mid oid =
  let (pat', match') = enableCopyingForMultUseInputsInPattern pat match
  in processMatch' instr pat' match' mid oid

-- | If the given match is derived from a pattern graph that has one or more
-- input values of which the pattern makes multiple uses, this function will
-- rewrite the pattern graph and match such that one of the copied values will
-- represent the actual input value. The reason for this is to allow the input
-- value to be appropriately copied before entering the instruction.
enableCopyingForMultUseInputsInPattern
  :: InstrPattern
  -> Match NodeID
  -> (InstrPattern, Match NodeID)
enableCopyingForMultUseInputsInPattern pat match =
  let g = osGraph $ patOS pat
      mult_use_input_vs =
        filter ( \n -> isValueNode n &&
                       not (isValueNodeWithConstValue n) &&
                       length (getDtFlowInEdges g n) == 0 &&
                       length (getDtFlowOutEdges g n) > 1
               ) $
        getAllNodes g
      rewrite old_input (old_p, old_m) =
        let old_os = patOS old_p
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
            new_cs = map (replaceNodeIDsInC old_input_id new_input_id) $
                     osConstraints old_os
            new_os = old_os { osGraph = g1
                            , osValidLocations = new_valid_locs
                            , osConstraints = new_cs
                            }
            new_m = toMatch $
                    filter (\m -> pNode m /= getNodeID cp_n) $
                    fromMatch old_m
            new_emit_str = updateNodeInEmitStrTemplate old_input_id
                                                       new_input_id
                                                       (patEmitString old_p)
            new_p = old_p { patOS = new_os
                          , patExternalData =
                              (new_input_id:patExternalData old_p)
                          , patEmitString = new_emit_str
                          }
        in (new_p, new_m)
  in foldr rewrite (pat, match) mult_use_input_vs

-- | Replaces a node ID found in a given constraint with another node ID.
replaceNodeIDsInC
  :: NodeID
     -- ^ Old node ID.
  -> NodeID
     -- ^ New node ID.
  -> Constraint
  -> Constraint
replaceNodeIDsInC old_n new_n c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ e@(ANodeIDExpr n) =
        if n == old_n then ANodeIDExpr new_n else e
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

processMatch'
  :: Instruction
  -> InstrPattern
  -> Match NodeID
  -> MatchID
  -> OperandID
     -- ^ The next operand ID to use in processing 'Match'.
  -> (HighLevelMatchParams, OperandID)
     -- ^ The created 'HighLevelMatchParams' and the new 'OperandID' to use when
     -- processing the next 'Match'.
processMatch' instr pat match mid oid =
  let graph = osGraph $ patOS pat
      all_ns = getAllNodes graph
      o_ns = filter isOperationNode all_ns
      d_ns = filter isDatumNode all_ns
      b_ns = filter isBlockNode all_ns
      entry_b = osEntryBlockNode $ patOS pat
      b_ns_consumed = if isJust entry_b
                      then filter ( \n -> hasAnyPredecessors graph n
                                          && hasAnySuccessors graph n
                                  )
                                  b_ns
                      else []
      c_ns = filter isControlNode all_ns
      d_def_ns = filter (hasAnyPredecessors graph) d_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_ext_ns = filter (\n -> (getNodeID n) `elem` patExternalData pat)
                        d_ns
      d_int_ns = filter (\n -> (getNodeID n) `notElem` patExternalData pat)
                        d_ns
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
      entry_b_node_id = osEntryBlockNode $ patOS pat
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
                   osValidLocations $ patOS pat
      i_props = instrProps instr
  in ( HighLevelMatchParams
         { hlMatchInstructionID = instrID instr
         , hlMatchPatternID = patID pat
         , hlMatchID = mid
         , hlOperandNodeMaps = oid2fn_maps
         , hlMatchOperationsCovered =  nub $ getMappedFNs o_ns
         , hlMatchOperandsDefined = nub $ getOpIDsForPatternDataNodes d_def_ns
         , hlMatchOperandsUsed = nub $ getOpIDsForPatternDataNodes d_use_ns
         , hlMatchExternalOperands = nub $ getOpIDsForPatternDataNodes d_ext_ns
         , hlMatchInternalOperands = nub $ getOpIDsForPatternDataNodes d_int_ns
         , hlMatchValidValueLocs = valid_locs
         , hlMatchEntryBlock =
             maybe Nothing (Just . head . findFNInMatch match) entry_b_node_id
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
             osConstraints $ patOS pat
         , hlMatchIsPhiInstruction = isInstructionPhi $ instr
         , hlMatchIsCopyInstruction = isInstructionCopy $ instr
         , hlMatchIsInactiveInstruction = isInstructionInactive $ instr
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
             computeEmitStrNodeMaps (patEmitString pat) pn2oid_maps match
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
      m_valid_locs =
        concatMap ( \m ->
                    concatMap ( \(o, ls) ->
                                map (\l -> (hlMatchID m, o, l)) ls
                              ) $
                    hlMatchValidValueLocs m
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
       , llFunInterchangeableData =
           map (map getAIForDatumNodeID) (hlFunInterchangeableData f_params)
       , llFunValidValueLocs =
           map (\(d, l) -> (getAIForDatumNodeID d, getAIForLocationID l))$
           f_valid_locs
       , llFunEntryBlock = getAIForBlockNodeID $ hlFunEntryBlock f_params
       , llFunBlockDomSets =
           map (\d -> map getAIForBlockNodeID (domSet d)) $
           sortByAI (getAIForBlockNodeID . domNode) $
           hlFunBlockDomSets f_params
       , llFunBBExecFreqs =
           map hlBlockExecFrequency $
           sortByAI (getAIForBlockNodeID . hlBlockNode) $
           hlFunBlockParams f_params
       , llFunBranchExtBlocks =
           map (getAIForBlockNodeID . hlBlockNode) $
           filter hlBlockIsBEBlock $
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
           map (map getAIForOperandID . hlMatchExternalOperands) m_params
       , llMatchInternalOperands =
           map (map getAIForOperandID . hlMatchInternalOperands) m_params
       , llMatchValidValueLocs =
           map ( \(m, o, l) -> ( getAIForMatchID m
                               , getAIForOperandID o
                               , getAIForLocationID l
                               )
               ) $
           m_valid_locs
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
       , llMatchInactiveInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsInactiveInstruction $
           m_params
       , llMatchNullInstructions =
           map (getAIForMatchID . hlMatchID) $
           filter hlMatchIsNullInstruction $
           m_params
       , llMatchConstraints =
           map (map (replaceIDsWithArrayIndexes ai_maps)) $
           map hlMatchConstraints $
           m_params
       , llMatchPatternIDs = map hlMatchPatternID m_params
       , llMatchInstructionIDs = map hlMatchInstructionID m_params
       , llIllegalMatchCombs = map (map getAIForMatchID) $
                               hlIllegalMatchCombs model
       , llTMID = hlMachineID tm_params
       }

-- | Converts any IDs appearing in a constraint with the corresponding array
-- index.
replaceIDsWithArrayIndexes :: ArrayIndexMaplists -> Constraint -> Constraint
replaceIDsWithArrayIndexes ai_maps c =
  let getAIForAnyNodeID nid = fromJust $ findAIWithAnyNodeID ai_maps nid
      getAIForMatchID mid = fromJust $ findAIWithMatchID ai_maps mid
      getAIForOperandID oid = fromJust $ findAIWithOperandID ai_maps oid
      getAIForLocationID rid = fromJust $ findAIWithLocationID ai_maps rid
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
      mkLocationExpr _ (ALocationIDExpr nid) =
        ALocationArrayIndexExpr $ getAIForLocationID nid
      mkLocationExpr r expr = (mkLocationExprF def_r) r expr
      mkInstructionExpr _ (AnInstructionIDExpr nid) =
        AnInstructionArrayIndexExpr $ getAIForInstructionID nid
      mkInstructionExpr r expr = (mkInstructionExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr
                    , mkOperandExprF = mkOperandExpr
                    , mkMatchExprF = mkMatchExpr
                    , mkLocationExprF = mkLocationExpr
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

computeDomSets :: Graph -> NodeID -> [DomSet Node]
computeDomSets g root_id =
  let cfg = extractCFG g
      root_n = head $ findNodesWithNodeID cfg root_id
  in G.computeDomSets cfg root_n

-- | Finds combinations of matches that are illegal, meaning they will yield a
-- cyclic data dependency if all are selected. The cycles are found by first
-- constructing a dependency graph for all matches, where each node is a match
-- and each edge represents a data dependency between the two matches. Matches
-- derived from generic phi patterns are not included in the dependency graph
-- (as those would always yield a cycle which is not actually a cyclic data
-- dependency), and matches which are known to never be the root cause of a
-- cycle -- that is, matches derived from null instructions and copy
-- instructions -- are also not included.
mkIllegalMatchCombs
  :: Function
  -> TargetMachine
  -> [PatternMatch]
  -> [[MatchID]]
mkIllegalMatchCombs function target matches =
  let g0 = I.mkGraph (zip [0..] matches) [] :: I.Gr PatternMatch ()
      getExternalDataNodes ip pm fg =
        let pg = osGraph $ patOS ip
            input_ns = filter ( \n -> isValueNode n &&
                                      not (hasAnyPredecessors pg n)
                              ) $
                       getAllNodes pg
            output_nids = filter ( \nid ->
                                   nid `notElem` (map getNodeID input_ns)
                                 ) $
                          patExternalData ip
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
            p1 = getInstrPatternFromPatternMatch target m1
            p2 = getInstrPatternFromPatternMatch target m2
            (m1_in_vs, m1_out_vs)  = getExternalDataNodes p1 m1 fg
            (m2_in_vs, m2_out_vs)  = getExternalDataNodes p2 m2 fg
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
      removeUninterestingMatches m g =
        let p = getInstructionFromPatternMatch target m
            n = getMatchNodeFromID g m
        in if isInstructionPhi p
           then I.delNode n g
           else if isInstructionNull p || isInstructionCopy p
                then delNodeKeepDeps n g
                else g
      g2 = foldr removeUninterestingMatches g1 matches
      forbidden_combs = map (map pmMatchID . map (fromJust . I.lab g2)) $
                        map init $ -- Remove last element as it is the same as
                                   -- the first element
                        cyclesIn' g2
  in forbidden_combs
