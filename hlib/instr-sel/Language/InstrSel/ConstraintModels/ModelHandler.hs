--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.ConstraintModels.ModelHandler
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Functions for constructing, lowering, and raising CP model instances.
--
--------------------------------------------------------------------------------

module Language.InstrSel.ConstraintModels.ModelHandler
  ( mkHighLevelModelNoOp
  , mkHighLevelModelWOp
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
import Language.InstrSel.OpStructures
import Language.InstrSel.Functions
  ( Function (..)
  , fromFunctionName
  )
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..) )
import Language.InstrSel.Utils
  ( (===) )
import Language.InstrSel.Utils.Range
import Language.InstrSel.PrettyShow

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
mkHighLevelModelNoOp
  :: Function
  -> TargetMachine
  -> [PatternMatch]
  -> HighLevelModelNoOp
mkHighLevelModelNoOp function target matches =
  HighLevelModelNoOp
    { hlNoOpFunctionParams = mkHLFunctionParams function target
    , hlNoOpMachineParams = mkHLMachineParams target
    , hlNoOpMatchParams = mkHLMatchParamsNoOpList target matches
    }

-- | Creates a 'HighLevelModelWOp' from a a 'HighLevelModelNoOp'.
mkHighLevelModelWOp
  :: HighLevelModelNoOp
  -> [PatternMatch]
  -> HighLevelModelWOp
mkHighLevelModelWOp model matches =
  HighLevelModelWOp
    { hlWOpFunctionParams = hlNoOpFunctionParams model
    , hlWOpMachineParams = hlNoOpMachineParams model
    , hlWOpMatchParams =
        mkHLMatchParamsWOpList matches (hlNoOpMatchParams model)
    }

mkHLFunctionParams :: Function -> TargetMachine -> HighLevelFunctionParams
mkHLFunctionParams function target =
  let graph = osGraph $ functionOS function
      entry_block = fromJust $ osEntryBlockNode $ functionOS function
      nodeIDsByType f = getNodeIDs $ filter f (getAllNodes graph)
      domsets = computeDomSets graph entry_block
      getExecFreq n =
        fromJust
        $ lookup (nameOfBlock $ getNodeType n)
                 (functionBBExecFreq function)
      bb_params =
        map ( \n -> HighLevelBlockParams
                      { hlBlockName = (nameOfBlock $ getNodeType n)
                      , hlBlockNode = (getNodeID n)
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
                         )
                     $ filter ( \e -> isStateNode (getSourceNode graph e)
                                    ||
                                    isStateNode (getTargetNode graph e)
                            )
                     $ filter isDefEdge
                     $ getAllEdges graph
      valid_locs =
        -- TODO: do a proper implemention. Right now it's just a quick hack to
        -- prevent the input arguments from being located in a fixed-value
        -- register
        let okay_locs = map locID
                        $ filter (isNothing . locValue)
                        $ tmLocations target
        in map (\n -> (n, okay_locs))
           $ functionInputs function
      int_const_data =
        let ns = filter isValueNodeWithConstValue (getAllNodes graph)
        in nub
           $ mapMaybe ( \n ->
                        let nid = getNodeID n
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
        in map ( \n -> ( getNodeID n, fromFunctionName
                                      $ nameOfCall
                                      $ getNodeType n
                       )
               )
               ns
  in HighLevelFunctionParams
       { hlFunOperations = nodeIDsByType isOperationNode
       , hlFunCopies = nodeIDsByType isCopyNode
       , hlFunData = nodeIDsByType isDatumNode
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

-- | First constructs a 'HighLevelMatchParamsNoOp' for each
-- 'PatternMatch'.
mkHLMatchParamsNoOpList
  :: TargetMachine
  -> [PatternMatch]
  -> [HighLevelMatchParamsNoOp]
mkHLMatchParamsNoOpList target matches =
  map (mkHLMatchParamsNoOp target) matches

mkHLMatchParamsNoOp :: TargetMachine -> PatternMatch -> HighLevelMatchParamsNoOp
mkHLMatchParamsNoOp target match =
  let instr = fromJust $ findInstruction (tmInstructions target)
                                         (pmInstrID match)
      pattern = fromJust $ findInstrPattern (instrPatterns instr)
                                            (pmPatternID match)
  in processMatch instr pattern (pmMatch match) (pmMatchID match)

processMatch
  :: Instruction
  -> InstrPattern
  -> Match NodeID
  -> MatchID
  -> HighLevelMatchParamsNoOp
processMatch instr pattern match mid =
  let graph = osGraph $ patOS pattern
      ns = getAllNodes graph
      o_ns = filter isOperationNode ns
      d_ns = filter isDatumNode ns
      b_ns = filter isBlockNode ns
      entry_b = osEntryBlockNode $ patOS pattern
      b_ns_consumed = if isJust entry_b
                      then filter ( \n -> hasAnyPredecessors graph n
                                          &&
                                          hasAnySuccessors graph n
                                  )
                                  b_ns
                      else []
      c_ns = filter isControlNode ns
      d_def_ns = filter (hasAnyPredecessors graph) d_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_ext_ns = filter (\n -> (getNodeID n) `elem` patExternalData pattern)
                        d_ns
      d_int_ns = filter (\n -> (getNodeID n) `notElem` patExternalData pattern)
                        d_ns
      d_use_by_phi_ns = filter (\n -> any isPhiNode (getSuccessors graph n))
                               d_use_ns
      b_use_by_phi_ns = map ( getTargetNode graph
                              . head
                              . filter isDefEdge
                              . getOutEdges graph
                            )
                            d_use_by_phi_ns
      d_def_by_phi_ns = filter (\n -> any isPhiNode (getPredecessors graph n))
                               d_def_ns
      b_def_by_phi_ns = map ( getSourceNode graph
                              . head
                              . filter isDefEdge
                              . getInEdges graph
                            )
                            d_def_by_phi_ns
      entry_b_node_id = osEntryBlockNode $ patOS pattern
      i_props = instrProps instr
      emit_maps = computeEmitStrNodeMaps (patEmitString pattern) match
      valid_locs = map (\(pn, ls) -> (fromJust $ findFNInMatch match pn, ls))
                   $ osValidLocations $ patOS pattern
  in HighLevelMatchParamsNoOp
       { hlNoOpMatchInstructionID = instrID instr
       , hlNoOpMatchPatternID = patID pattern
       , hlNoOpMatchID = mid
       , hlNoOpMatchOperationsCovered = findFNsInMatch match (getNodeIDs o_ns)
       , hlNoOpMatchDataDefined = findFNsInMatch match (getNodeIDs d_def_ns)
       , hlNoOpMatchDataUsed = findFNsInMatch match (getNodeIDs d_use_ns)
       , hlNoOpMatchExternalData = findFNsInMatch match (getNodeIDs d_ext_ns)
       , hlNoOpMatchInternalData = findFNsInMatch match (getNodeIDs d_int_ns)
       , hlNoOpMatchValidValueLocs = valid_locs
       , hlNoOpMatchEntryBlock =
           maybe Nothing (findFNInMatch match) entry_b_node_id
       , hlNoOpMatchSpannedBlocks =
           if isJust entry_b
           then findFNsInMatch match (getNodeIDs b_ns)
           else []
       , hlNoOpMatchConsumedBlocks =
           findFNsInMatch match (getNodeIDs b_ns_consumed)
       , hlNoOpMatchConstraints =
           map
             ((replaceThisMatchExprInC mid) . (replaceNodeIDsFromP2FInC match))
             (osConstraints $ patOS pattern)
       , hlNoOpMatchIsPhiInstruction = isInstructionPhi $ instr
       , hlNoOpMatchIsCopyInstruction = isInstructionCopy $ instr
       , hlNoOpMatchIsNullInstruction = isInstructionNull instr
       , hlNoOpMatchHasControlFlow = length c_ns > 0
       , hlNoOpMatchCodeSize = instrCodeSize i_props
       , hlNoOpMatchLatency = instrLatency i_props
       , hlNoOpMatchDataUsedByPhis =
           zip (findFNsInMatch match $ getNodeIDs b_use_by_phi_ns)
               (findFNsInMatch match $ getNodeIDs d_use_by_phi_ns)
       , hlNoOpMatchDataDefinedByPhis =
           zip (findFNsInMatch match $ getNodeIDs b_def_by_phi_ns)
               (findFNsInMatch match $ getNodeIDs d_def_by_phi_ns)
       , hlNoOpMatchEmitStrNodeMaplist = emit_maps
       }

-- | Computes the emit string node ID mappings, which is done as follows: if the
-- assembly string part contains a node ID, take the node ID from the
-- corresponding node in the function graph. Otherwise use 'Nothing'.
computeEmitStrNodeMaps
  :: EmitStringTemplate
  -> Match NodeID
  -> [[Maybe NodeID]]
computeEmitStrNodeMaps (EmitStringTemplate ts) m =
  map (map f) ts
  where f (ESVerbatim            _) = Nothing
        f (ESLocationOfValueNode n) = findFNInMatch m n
        f (ESIntConstOfValueNode n) = findFNInMatch m n
        f (ESNameOfBlockNode     n) = findFNInMatch m n
        f (ESBlockOfValueNode    n) = findFNInMatch m n
        f (ESLocalTemporary      _) = Nothing
        f (ESFuncOfCallNode      n) = findFNInMatch m n

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
replaceNodeIDsFromP2FInC match c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ (ANodeIDExpr nid) =
        ANodeIDExpr (fromJust $ findFNInMatch match nid)
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

-- | First constructs a 'HighLevelMatchParamsWOp' for each
-- 'HighLevelMatchParamsNoOp', and then merges 'HighLevelMatchParamsWOp' that
-- are derived from the same pattern graph and cover the same operations but use
-- different value nodes as input. 'HighLevelMatchParamsWOp' that cover no
-- operations are never considered to be similar.
mkHLMatchParamsWOpList
  :: [PatternMatch]
  -> [HighLevelMatchParamsNoOp]
  -> [HighLevelMatchParamsWOp]
mkHLMatchParamsWOpList matches params =
  let getMatch mid =
        let match = filter (\m -> pmMatchID m == mid) matches
        in if length match == 1
           then head match
           else error $ "mkHighLevelModelWOp: no or multiple PatternMatches "
                        ++ "found with match ID " ++ pShow mid
      f (p1, _) (p2, _) =
        let iid1 = hlWOpMatchInstructionID p1
            iid2 = hlWOpMatchInstructionID p2
            pid1 = hlWOpMatchPatternID p1
            pid2 = hlWOpMatchPatternID p2
            mid1 = hlWOpMatchID p1
            mid2 = hlWOpMatchID p2
            covs1 = hlWOpMatchOperationsCovered p1
            covs2 = hlWOpMatchOperationsCovered p2
        in if iid1 == iid2
              && pid1 == pid2
              && length covs1 > 0
              && covs1 === covs2
           then EQ
           else compare mid1 mid2
      new_params = fst
                   $ foldl ( \(ps, next_oid) p ->
                               let (new_p, next_oid') =
                                     mkHLMatchParamsWOp p next_oid
                               in (ps ++ [new_p], next_oid')
                           )
                           ([], 0)
                           params
      new_params_w_matches =
        map (\p -> (p, getMatch $ hlWOpMatchID p)) new_params
      merged_params = map mergeSimilarHighLevelMatchParamsWOps
                      $ groupBy (\p1 p2 -> f p1 p2 == EQ)
                      $ sortBy f new_params_w_matches
  in merged_params

mkHLMatchParamsWOp
  :: HighLevelMatchParamsNoOp
  -> OperandID
     -- ^ The next operand ID to use when processing this
     -- 'HighLevelMatchParamsNoOp'.
  -> (HighLevelMatchParamsWOp, OperandID)
     -- ^ The created 'HighLevelMatchParamsWOp' and the 'OperandID' to use when
     -- processing the next group.
mkHLMatchParamsWOp p oid =
  let value_nodes = nub $ hlNoOpMatchDataDefined p ++ hlNoOpMatchDataUsed p
      op_node_maps = zip [oid..] $ map (\n -> [n]) value_nodes
      getOpIDFromNodeID = fromJust . findOpIDFromNodeID op_node_maps
  in ( HighLevelMatchParamsWOp
         { hlWOpMatchInstructionID = hlNoOpMatchInstructionID p
         , hlWOpMatchPatternID = hlNoOpMatchPatternID p
         , hlWOpMatchID = hlNoOpMatchID p
         , hlWOpOperandNodeMaps = op_node_maps
         , hlWOpMatchOperationsCovered = hlNoOpMatchOperationsCovered p
         , hlWOpMatchDataDefined =
             map getOpIDFromNodeID $ hlNoOpMatchDataDefined p
         , hlWOpMatchDataUsed =
             map getOpIDFromNodeID $ hlNoOpMatchDataUsed p
         , hlWOpMatchExternalData =
             map getOpIDFromNodeID $ hlNoOpMatchExternalData p
         , hlWOpMatchInternalData =
             map getOpIDFromNodeID $ hlNoOpMatchInternalData p
         , hlWOpMatchValidValueLocs =
             map (\(n, ls) -> (getOpIDFromNodeID n, ls))
             $ hlNoOpMatchValidValueLocs p
         , hlWOpMatchEntryBlock = hlNoOpMatchEntryBlock p
         , hlWOpMatchSpannedBlocks = hlNoOpMatchSpannedBlocks p
         , hlWOpMatchConsumedBlocks = hlNoOpMatchConsumedBlocks p
         , hlWOpMatchCodeSize = hlNoOpMatchCodeSize p
         , hlWOpMatchLatency = hlNoOpMatchLatency p
         , hlWOpMatchConstraints =
             map (replaceNodeIDsWithOperandIDs op_node_maps)
                 (hlNoOpMatchConstraints p)
         , hlWOpMatchIsPhiInstruction = hlNoOpMatchIsPhiInstruction p
         , hlWOpMatchIsCopyInstruction = hlNoOpMatchIsCopyInstruction p
         , hlWOpMatchIsNullInstruction = hlNoOpMatchIsNullInstruction p
         , hlWOpMatchHasControlFlow = hlNoOpMatchHasControlFlow p
         , hlWOpMatchDataUsedByPhis =
             map (\(b, v) -> (b, getOpIDFromNodeID v))
                 (hlNoOpMatchDataUsedByPhis p)
         , hlWOpMatchDataDefinedByPhis =
             map (\(b, v) -> (b, getOpIDFromNodeID v))
                 (hlNoOpMatchDataDefinedByPhis p)
         , hlWOpMatchEmitStrNodeMaplist =
             map ( map ( \n ->
                          if isJust n
                          then let oid' = findOpIDFromNodeID op_node_maps
                                          $ fromJust n
                               in Just
                                  $ if isJust oid'
                                    then Left $ fromJust oid'
                                    else Right $ fromJust n
                          else Nothing
                      )
                 )
                 (hlNoOpMatchEmitStrNodeMaplist p)
         }
     , oid + (toOperandID $ length op_node_maps)
     )

-- | Converts any node IDs appearing in a constraint that has an operand ID with
-- the corresponding operand ID.
replaceNodeIDsWithOperandIDs
  :: [(OperandID, [NodeID])]
  -> Constraint
  -> Constraint
replaceNodeIDsWithOperandIDs maps c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ expr@(ANodeIDExpr nid) =
        let oid = findOpIDFromNodeID maps nid
        in if isJust oid
           then NodeSelectedForOperandExpr $ AnOperandIDExpr $ fromJust oid
           else expr
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

findOpIDFromNodeID :: [(OperandID, [NodeID])] -> NodeID -> Maybe OperandID
findOpIDFromNodeID maps nid =
  let ms = filter (elem nid . snd) maps
  in if length ms > 0
     then Just $ fst $ head ms
     else Nothing

mergeSimilarHighLevelMatchParamsWOps
  :: [(HighLevelMatchParamsWOp, PatternMatch)]
  -> HighLevelMatchParamsWOp
     -- ^ The created 'HighLevelMatchParamsWOp' and the 'OperandID' to use when
     -- processing the next group.
mergeSimilarHighLevelMatchParamsWOps pps =
  let compareTuples (p1, _) (p2, _) =
        compare (hlWOpMatchID p1) (hlWOpMatchID p2)
      findNode pm1 fn1 pm2 fns =
        let m1 = pmMatch pm1
            m2 = pmMatch pm2
            pn = fromJust $ findPNInMatch m1 fn1
            fn2 = fromJust $ findFNInMatch m2 pn
        in if [fn2] `elem` fns
           then [fn2]
           else []
      mergeTuples (p, m) (p', m') =
        let maps = hlWOpOperandNodeMaps p
            maps' = hlWOpOperandNodeMaps p'
            ns' = map snd maps'
            new_maps = map ( \(oid, ns) ->
                               (oid, nub $ ns ++ findNode m (head ns) m' ns')
                           )
                           maps
        in ( p { hlWOpOperandNodeMaps = new_maps }
           , m
           )
      sorted_pps = sortBy compareTuples pps
      pp = head sorted_pps
      pps_wo_pp = filter ((EQ /=) . compareTuples pp) pps
  in fst $ foldl mergeTuples pp pps_wo_pp

-- | Computes the corresponding low-level version of a high-level CP model
-- instance.
lowerHighLevelModel :: HighLevelModelWOp -> ArrayIndexMaplists -> LowLevelModel
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
        map snd
            ( sortBy (\(ai1, _) (ai2, _) -> compare ai1 ai2)
                     (pairWithAI get_ai_f nids)
            )
      f_params = hlWOpFunctionParams model
      tm_params = hlWOpMachineParams model
      m_params = sortByAI (getAIForMatchID . hlWOpMatchID)
                          (hlWOpMatchParams model)
      operands = sortByAI (getAIForOperandID . fst)
                          (concatMap (hlWOpOperandNodeMaps) m_params)
      blocks = sortByAI getAIForBlockNodeID $ hlFunBlocks f_params
      in_def_edges =
        concatMap ( \m ->
                      concatMap ( \b ->
                                    if hlWOpMatchIsPhiInstruction m
                                    then let ops = map snd
                                                   $ filter ((==) b . fst)
                                                   $ hlWOpMatchDataUsedByPhis m
                                         in map (\o -> (hlWOpMatchID m, b, o))
                                                ops
                                    else []
                                )
                                blocks
                  )
                  m_params
      out_def_edges =
        concatMap ( \m ->
                      concatMap ( \b ->
                                    if hlWOpMatchIsPhiInstruction m
                                    then let ops = map snd
                                                   $ filter ((==) b . fst)
                                                   $ hlWOpMatchDataDefinedByPhis
                                                       m
                                         in map (\o -> (hlWOpMatchID m, b, o))
                                                ops
                                    else []
                                )
                                blocks
                  )
                  m_params
      f_valid_locs =
        concatMap (\(n, ls) -> map (\l -> (n, l)) ls)
                  (hlFunValidValueLocs f_params)
      m_valid_locs =
        concatMap ( \m ->
                      concatMap ( \(o, ls) ->
                                    map (\l -> (hlWOpMatchID m, o, l)) ls
                                )
                                (hlWOpMatchValidValueLocs m)
                  )
                  m_params
  in LowLevelModel
       { llFunNumOperations = toInteger $ length $ hlFunOperations f_params
       , llFunNumData = toInteger $ length $ hlFunData f_params
       , llFunNumBlocks = toInteger $ length $ hlFunBlocks f_params
       , llFunCopies = map getAIForOperationNodeID (hlFunCopies f_params)
       , llFunStates = map getAIForDatumNodeID (hlFunStates f_params)
       , llFunValidValueLocs =
           map (\(d, l) -> (getAIForDatumNodeID d, getAIForLocationID l))
               f_valid_locs
       , llFunEntryBlock = getAIForBlockNodeID $ hlFunEntryBlock f_params
       , llFunBlockDomSets =
           map (\d -> map getAIForBlockNodeID (domSet d))
               ( sortByAI (getAIForBlockNodeID . domNode)
                          (hlFunBlockDomSets f_params)
               )
       , llFunBBExecFreqs =
           map hlBlockExecFrequency
               ( sortByAI (getAIForBlockNodeID . hlBlockNode)
                          (hlFunBlockParams f_params)
               )
       , llFunStateDefEdges =
           map (\(b, s) -> (getAIForBlockNodeID b, getAIForDatumNodeID s))
           $ hlFunStateDefEdges f_params
       , llFunConstraints =
           map (replaceIDsWithArrayIndexes ai_maps) (hlFunConstraints f_params)
       , llNumLocations = toInteger $ length $ hlMachineLocations tm_params
       , llNumMatches = toInteger $ length m_params
       , llNumOperands = toInteger $ length $ map fst $ operands
       , llOperandAlternatives = map (map getAIForDatumNodeID . snd) operands
       , llMatchOperationsCovered =
           map (map getAIForOperationNodeID . hlWOpMatchOperationsCovered)
               m_params
       , llMatchDataDefined =
           map (map getAIForOperandID . hlWOpMatchDataDefined) m_params
       , llMatchDataUsed =
           map (map getAIForOperandID . hlWOpMatchDataUsed) m_params
       , llMatchExternalData =
           map (map getAIForOperandID . hlWOpMatchExternalData) m_params
       , llMatchInternalData =
           map (map getAIForOperandID . hlWOpMatchInternalData) m_params
       , llMatchValidValueLocs =
           map ( \(m, o, l) ->
                   ( getAIForMatchID m
                   , getAIForOperandID o
                   , getAIForLocationID l
                   )
               )
               m_valid_locs
       , llMatchEntryBlocks =
           map (maybe Nothing (Just . getAIForBlockNodeID))
               (map hlWOpMatchEntryBlock m_params)
       , llMatchSpannedBlocks = map (map getAIForBlockNodeID)
                                    (map hlWOpMatchSpannedBlocks m_params)
       , llMatchConsumedBlocks = map (map getAIForBlockNodeID)
                                    (map hlWOpMatchConsumedBlocks m_params)
       , llMatchInputDefinitionEdges =
           map ( \(m, b, o) ->
                   ( getAIForMatchID m
                   , getAIForBlockNodeID b
                   , getAIForOperandID o
                   )
               )
               in_def_edges
       , llMatchOutputDefinitionEdges =
           map ( \(m, b, o) ->
                   ( getAIForMatchID m
                   , getAIForBlockNodeID b
                   , getAIForOperandID o
                   )
               )
               out_def_edges
       , llMatchCodeSizes = map hlWOpMatchCodeSize m_params
       , llMatchLatencies = map hlWOpMatchLatency m_params
       , llMatchPhiInstructions =
           map (getAIForMatchID . hlWOpMatchID)
               (filter hlWOpMatchIsPhiInstruction m_params)
       , llMatchCopyInstructions =
           map (getAIForMatchID . hlWOpMatchID)
               (filter hlWOpMatchIsCopyInstruction m_params)
       , llMatchNullInstructions =
           map (getAIForMatchID . hlWOpMatchID)
               (filter hlWOpMatchIsNullInstruction m_params)
       , llMatchConstraints =
           map (map (replaceIDsWithArrayIndexes ai_maps))
               (map hlWOpMatchConstraints m_params)
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
