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
  ( mkHighLevelModel
  , lowerHighLevelModel
  )
where

import Language.InstrSel.ConstraintModels.Base
import Language.InstrSel.ConstraintModels.IDs

import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
  hiding
  ( computeDomSets )
import qualified Language.InstrSel.Graphs as G
  ( computeDomSets )
import Language.InstrSel.OpStructures
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..) )
import Language.InstrSel.Utils.Range

import Data.List
  ( elemIndex
  , nub
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
    }

mkHLFunctionParams :: Function -> TargetMachine -> HighLevelFunctionParams
mkHLFunctionParams function target =
  let graph = osGraph $ functionOS function
      entry_block = fromJust $ osEntryBlockNode $ functionOS function
      nodeIDsByType f = getNodeIDs $ filter f (getAllNodes graph)
      def_edges =
        map ( \e ->
              let src = getSourceNode graph e
                  srcid = getNodeID src
                  dst = getTargetNode graph e
                  dstid = getNodeID dst
              in if isBlockNode src
                 then (srcid, dstid)
                 else (dstid, srcid)
            )
            (filter isDefEdge (getAllEdges graph))
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
      mkCallingConventionConstraints =
        -- TODO: do a proper implemention. Right now it's just a quick hack to
        -- prevent the input arguments from being located in a zero-value
        -- register
        let okay_locs = map locID
                        $ filter (isNothing . locValue)
                        $ tmLocations target
        in concatMap ( \n ->
                       mkDataLocConstraints okay_locs n
                     )
                     (functionInputs function)
  in HighLevelFunctionParams
       { hlFunOperations = nodeIDsByType isNodeAnOperation
       , hlFunData = nodeIDsByType isNodeADatum
       , hlFunStates = nodeIDsByType isStateNode
       , hlFunBlocks = nodeIDsByType isBlockNode
       , hlFunEntryBlock = entry_block
       , hlFunBlockDomSets = map convertDomSetN2ID domsets
       , hlFunDefEdges = def_edges
       , hlFunBlockParams = bb_params
       , hlFunValueIntConstData = int_const_data
       , hlFunValueOriginData = value_origin_data
       , hlFunConstraints = (osConstraints $ functionOS function)
                            ++
                            mkCallingConventionConstraints
       }

mkHLMachineParams :: TargetMachine -> HighLevelMachineParams
mkHLMachineParams target =
  HighLevelMachineParams
    { hlMachineID = tmID target
    , hlMachineLocations = map locID (tmLocations target)
    }

mkHLMatchParamsList :: TargetMachine -> [PatternMatch] -> [HighLevelMatchParams]
mkHLMatchParamsList target matches = map (mkHLMatchParams target) matches

mkHLMatchParams :: TargetMachine -> PatternMatch -> HighLevelMatchParams
mkHLMatchParams target match =
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
  -> HighLevelMatchParams
processMatch instr pattern match mid =
  let graph = osGraph $ patOS pattern
      ns = getAllNodes graph
      o_ns = filter isNodeAnOperation ns
      d_ns = filter isNodeADatum ns
      b_ns = filter isBlockNode ns
      b_ns_consumed = filter ( \n -> hasAnyPredecessors graph n
                                     &&
                                     hasAnySuccessors graph n
                             )
                             b_ns
      c_ns = filter isControlNode ns
      d_def_ns = filter (hasAnyPredecessors graph) d_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_use_by_phi_ns = filter (\n -> any isPhiNode (getSuccessors graph n))
                               d_use_ns
      entry_b_node_id = osEntryBlockNode $ patOS pattern
      i_props = instrProps instr
      asm_maps = computeAsmStrNodeMaps (patAsmStrTemplate pattern) match
  in HighLevelMatchParams
       { hlMatchInstructionID = instrID instr
       , hlMatchPatternID = patID pattern
       , hlMatchID = mid
       , hlMatchOperationsCovered = findFNsInMatch match (getNodeIDs o_ns)
       , hlMatchDataDefined = findFNsInMatch match (getNodeIDs d_def_ns)
       , hlMatchDataUsed = findFNsInMatch match (getNodeIDs d_use_ns)
       , hlMatchEntryBlock = maybe Nothing (findFNInMatch match) entry_b_node_id
       , hlMatchSpannedBlocks = findFNsInMatch match (getNodeIDs b_ns)
       , hlMatchConsumedBlocks = findFNsInMatch match (getNodeIDs b_ns_consumed)
       , hlMatchConstraints =
           map
             ((replaceThisMatchExprInC mid) . (replaceNodeIDsFromP2FInC match))
             (osConstraints $ patOS pattern)
       , hlMatchADDUC = patADDUC pattern
       , hlMatchIsNonCopyInstruction = instrIsNonCopy $ instrProps instr
       , hlMatchIsNullCopyInstruction = instrIsNullCopy $ instrProps instr
       , hlMatchHasControlFlow = length c_ns > 0
       , hlMatchCodeSize = instrCodeSize i_props
       , hlMatchLatency = instrLatency i_props
       , hlMatchDataUsedByPhis =
           findFNsInMatch match (getNodeIDs d_use_by_phi_ns)
       , hlMatchAsmStrNodeMaplist = asm_maps
       }

-- | Computes the assembly string node ID mappings, which is done as follows: if
-- the assembly string part contains a node ID, take the node ID from the
-- corresponding node in the function graph. Otherwise use 'Nothing'.
computeAsmStrNodeMaps
  :: AssemblyStringTemplate
  -> Match NodeID
  -> [Maybe NodeID]
computeAsmStrNodeMaps t m =
  map f (concat $ flattenAsmStrParts t)
  where f (ASVerbatim _) = Nothing
        f (ASReferenceToValueNode n) = findFNInMatch m n
        f (ASIntConstOfValueNode  n) = findFNInMatch m n
        f (ASNameOfBlockNode      n) = findFNInMatch m n
        f (ASBlockOfValueNode     n) = findFNInMatch m n

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

-- | Computes the corresponding low-level version of a high-level CP model
-- instance.
lowerHighLevelModel :: HighLevelModel -> ArrayIndexMaplists -> LowLevelModel
lowerHighLevelModel model ai_maps =
  let getAIForOperationNodeID nid =
        fromJust $ findAIWithOperationNodeID ai_maps nid
      getAIForDatumNodeID nid = fromJust $ findAIWithDatumNodeID ai_maps nid
      getAIForBlockNodeID nid = fromJust $ findAIWithBlockNodeID ai_maps nid
      getAIForMatchID mid = fromJust $ findAIWithMatchID ai_maps mid
      pairWithAI get_ai_f nids = map (\nid -> (get_ai_f nid, nid)) nids
      sortByAI get_ai_f nids =
        map snd
            ( sortBy (\(ai1, _) (ai2, _) -> compare ai1 ai2)
                     (pairWithAI get_ai_f nids)
            )
      f_params = hlFunctionParams model
      tm_params = hlMachineParams model
      m_params = sortByAI (getAIForMatchID . hlMatchID) (hlMatchParams model)
  in LowLevelModel
       { llFunNumOperations = toInteger $ length $ hlFunOperations f_params
       , llFunNumData = toInteger $ length $ hlFunData f_params
       , llFunNumBlocks = toInteger $ length $ hlFunBlocks f_params
       , llFunStates = map getAIForDatumNodeID (hlFunStates f_params)
       , llFunEntryBlock = getAIForBlockNodeID $ hlFunEntryBlock f_params
       , llFunBlockDomSets =
           map (\d -> map getAIForBlockNodeID (domSet d))
               ( sortByAI (getAIForBlockNodeID . domNode)
                          (hlFunBlockDomSets f_params)
               )
       , llFunDefEdges =
           map ( \n ->
                 nub $
                 map (getAIForDatumNodeID . snd)
                     (filter (\(n', _) -> n == n') (hlFunDefEdges f_params))
               )
               (sortByAI getAIForBlockNodeID (hlFunBlocks f_params))
       , llFunBBExecFreqs =
           map hlBlockExecFrequency
               ( sortByAI (getAIForBlockNodeID . hlBlockNode)
                          (hlFunBlockParams f_params)
               )
       , llFunConstraints =
           map (replaceIDWithArrayIndex ai_maps) (hlFunConstraints f_params)
       , llNumLocations = toInteger $ length $ hlMachineLocations tm_params
       , llNumMatches = toInteger $ length m_params
       , llMatchOperationsCovered =
           map (\m -> map getAIForOperationNodeID (hlMatchOperationsCovered m))
               m_params
       , llMatchDataDefined =
           map (\m -> map getAIForDatumNodeID (hlMatchDataDefined m)) m_params
       , llMatchDataUsed =
           map (\m -> map getAIForDatumNodeID (hlMatchDataUsed m)) m_params
       , llMatchEntryBlocks =
           map (maybe Nothing (Just . getAIForBlockNodeID))
               (map hlMatchEntryBlock m_params)
       , llMatchSpannedBlocks = map (map getAIForBlockNodeID)
                                    (map hlMatchSpannedBlocks m_params)
       , llMatchConsumedBlocks = map (map getAIForBlockNodeID)
                                    (map hlMatchConsumedBlocks m_params)
       , llMatchCodeSizes = map hlMatchCodeSize m_params
       , llMatchLatencies = map hlMatchLatency m_params
       , llMatchADDUCs = map hlMatchADDUC m_params
       , llMatchNonCopyInstructions =
           map (getAIForMatchID . hlMatchID)
               (filter hlMatchIsNonCopyInstruction m_params)
       , llMatchNullCopyInstructions =
           map (getAIForMatchID . hlMatchID)
               (filter hlMatchIsNullCopyInstruction m_params)
       , llMatchConstraints =
           map (map (replaceIDWithArrayIndex ai_maps))
               (map hlMatchConstraints m_params)
       }

-- | Converts any ID appearing in a constraint with the corresponding array
-- index.
replaceIDWithArrayIndex :: ArrayIndexMaplists -> Constraint -> Constraint
replaceIDWithArrayIndex ai_maps c =
  let getAIForAnyNodeID nid = fromJust $ findAIWithAnyNodeID ai_maps nid
      getAIForMatchID mid = fromJust $ findAIWithMatchID ai_maps mid
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
      mkLocationExpr _ (ALocationIDExpr nid) =
        ALocationArrayIndexExpr $ getAIForLocationID nid
      mkLocationExpr r expr = (mkLocationExprF def_r) r expr
      mkInstructionExpr _ (AnInstructionIDExpr nid) =
        AnInstructionArrayIndexExpr $ getAIForInstructionID nid
      mkInstructionExpr r expr = (mkInstructionExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr
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
