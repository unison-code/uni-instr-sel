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
    { hlFunctionParams = mkHLFunctionParams function
    , hlMachineParams = mkHLMachineParams target
    , hlMatchParams = mkHLMatchParamsList target matches
    }

mkHLFunctionParams :: Function -> HighLevelFunctionParams
mkHLFunctionParams function =
  let graph = osGraph $ functionOS function
      entry_label = fromJust $ osEntryLabelNode $ functionOS function
      nodeIDsByType f = getNodeIDs $ filter f (getAllNodes graph)
      def_edges =
        map ( \e ->
              let src = getSourceNode graph e
                  srcid = getNodeID src
                  dst = getTargetNode graph e
                  dstid = getNodeID dst
              in if isLabelNode src
                 then (srcid, dstid)
                 else (dstid, srcid)
            )
            (filter isDefEdge (getAllEdges graph))
      domsets = computeDomSets graph entry_label
      getExecFreq n =
        fromJust
        $ lookup (blockName $ getNodeType n)
                 (functionBBExecFreq function)
      bb_params =
        map ( \n -> HighLevelBlockParams
                      { hlBlockName = (blockName $ getNodeType n)
                      , hlBlockLabelNode = (getNodeID n)
                      , hlBlockExecFrequency = getExecFreq n
                      }
            )
            (filter isLabelNode (getAllNodes graph))
      int_constants =
        let ns = filter isDataNodeWithConstValue (getAllNodes graph)
        in nub
           $ mapMaybe ( \n ->
                         let nid = getNodeID n
                             dt = getDataTypeOfDataNode n
                             r = intConstValue dt
                         in if isIntConstType dt && isRangeSingleton r
                            then Just (nid, lowerBound r)
                            else Nothing
                       )
                       ns
  in HighLevelFunctionParams
       { hlFunOpNodes = nodeIDsByType isOperationNode
       , hlFunEntityNodes = nodeIDsByType isEntityNode
       , hlFunStateNodes = nodeIDsByType isStateNode
       , hlFunLabelNodes = nodeIDsByType isLabelNode
       , hlFunEntryLabelNode = entry_label
       , hlFunLabelDomSets = map convertDomSetN2ID domsets
       , hlFunDefEdges = def_edges
       , hlFunBlockParams = bb_params
       , hlFunDataIntConstants = int_constants
       , hlFunConstraints = osConstraints $ functionOS function
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
      o_ns = filter isOperationNode ns
      e_ns = filter isEntityNode ns
      d_ns = filter isDataNode ns
      l_ns = filter isLabelNode ns
      c_ns = filter isControlNode ns
      e_def_ns = filter (hasAnyPredecessors graph) e_ns
      e_use_ns = filter (hasAnySuccessors graph) e_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_use_by_phi_ns = filter (\n -> any isPhiNode (getSuccessors graph n))
                               d_use_ns
      entry_l_node_id = osEntryLabelNode $ patOS pattern
      l_ref_ns = if isJust entry_l_node_id
                 then let nid = fromJust entry_l_node_id
                      in filter (\n -> getNodeID n /= nid) l_ns
                 else l_ns
      i_props = instrProps instr
      asm_maps = computeAsmStrNodeMaps (patAsmStrTemplate pattern) match
  in HighLevelMatchParams
       { hlMatchInstructionID = instrID instr
       , hlMatchPatternID = patID pattern
       , hlMatchID = mid
       , hlMatchOpNodesCovered = findFNsInMatch match (getNodeIDs o_ns)
       , hlMatchEntityNodesDefined = findFNsInMatch match (getNodeIDs e_def_ns)
       , hlMatchEntityNodesUsed = findFNsInMatch match (getNodeIDs e_use_ns)
       , hlMatchEntryLabelNode = maybe Nothing
                                       (findFNInMatch match)
                                       entry_l_node_id
       , hlMatchNonEntryLabelNodes = findFNsInMatch match (getNodeIDs l_ref_ns)
       , hlMatchConstraints =
           map
             ((replaceThisMatchExprInC mid) . (replaceNodeIDsFromP2FInC match))
             (osConstraints $ patOS pattern)
       , hlMatchADDUC = patADDUC pattern
       , hlMatchIsCopyInstruction =
           length o_ns == 1 && isCopyNode (head o_ns)
       , hlMatchHasControlNodes = length c_ns > 0
       , hlMatchCodeSize = instrCodeSize i_props
       , hlMatchLatency = instrLatency i_props
       , hlMatchDataNodesUsedByPhis =
           findFNsInMatch match (getNodeIDs d_use_by_phi_ns)
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
        f (ASLocationOfDataNode    n) = findFNInMatch m n
        f (ASImmIntValueOfDataNode n) = findFNInMatch m n
        f (ASBlockOfLabelNode      n) = findFNInMatch m n
        f (ASBlockOfDataNode       n) = findFNInMatch m n

-- | Replaces occurrences of @ThisMatchExpr@ in a constraint with the given
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
  let getAIForOpNodeID nid = fromJust $ findAIWithOpNodeID ai_maps nid
      getAIForEntityNodeID nid = fromJust $ findAIWithEntityNodeID ai_maps nid
      getAIForLabelNodeID nid = fromJust $ findAIWithLabelNodeID ai_maps nid
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
       { llNumFunOpNodes = toInteger $ length $ hlFunOpNodes f_params
       , llNumFunEntityNodes = toInteger $ length $ hlFunEntityNodes f_params
       , llNumFunLabelNodes = toInteger $ length $ hlFunLabelNodes f_params
       , llFunStateNodes = map getAIForEntityNodeID (hlFunStateNodes f_params)
       , llFunEntryLabelNode =
           getAIForLabelNodeID $ hlFunEntryLabelNode f_params
       , llFunLabelDomSets =
           map (\d -> map getAIForLabelNodeID (domSet d))
               ( sortByAI (getAIForLabelNodeID . domNode)
                          (hlFunLabelDomSets f_params)
               )
       , llFunDefEdges =
           map ( \n ->
                 nub $
                 map (getAIForEntityNodeID . snd)
                     (filter (\(n', _) -> n == n') (hlFunDefEdges f_params))
               )
               (sortByAI getAIForLabelNodeID (hlFunLabelNodes f_params))
       , llFunBBExecFreqs =
           map hlBlockExecFrequency
               ( sortByAI (getAIForLabelNodeID . hlBlockLabelNode)
                          (hlFunBlockParams f_params)
               )
       , llFunConstraints =
           map (replaceIDWithArrayIndex ai_maps) (hlFunConstraints f_params)
       , llNumLocations = toInteger $ length $ hlMachineLocations tm_params
       , llNumMatches = toInteger $ length m_params
       , llMatchOpNodesCovered =
           map (\m -> map getAIForOpNodeID (hlMatchOpNodesCovered m))
               m_params
       , llMatchEntityNodesDefined =
           map (\m -> map getAIForEntityNodeID (hlMatchEntityNodesDefined m))
               m_params
       , llMatchEntityNodesUsed =
           map (\m -> map getAIForEntityNodeID (hlMatchEntityNodesUsed m))
               m_params
       , llMatchEntryLabelNode =
           map (maybe Nothing (Just . getAIForLabelNodeID))
               (map hlMatchEntryLabelNode m_params)
       , llMatchNonEntryLabelNodes =
           map (map getAIForLabelNodeID)
               (map hlMatchNonEntryLabelNodes m_params)
       , llMatchCodeSizes = map hlMatchCodeSize m_params
       , llMatchLatencies = map hlMatchLatency m_params
       , llMatchADDUCs = map hlMatchADDUC m_params
       , llMatchCopyInstructions =
           map (getAIForMatchID . hlMatchID)
               (filter hlMatchIsCopyInstruction m_params)
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

findAIWithOpNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithOpNodeID ai_maps = findArrayIndexInList (ai2OpNodeIDs ai_maps)

findAIWithEntityNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithEntityNodeID ai_maps = findArrayIndexInList (ai2EntityNodeIDs ai_maps)

findAIWithLabelNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithLabelNodeID ai_maps = findArrayIndexInList (ai2LabelNodeIDs ai_maps)

findAIWithAnyNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIWithAnyNodeID ai_maps nid =
  let lists = [ ai2OpNodeIDs ai_maps
              , ai2EntityNodeIDs ai_maps
              , ai2LabelNodeIDs ai_maps
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
