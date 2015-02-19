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

import Language.InstSel.ConstraintModels.Base
import Language.InstSel.ConstraintModels.IDs

import Language.InstSel.Constraints
import Language.InstSel.Constraints.ConstraintReconstructor
import Language.InstSel.Graphs
  hiding
  ( computeDomsets
  , computePostDomsets
  )
import qualified Language.InstSel.Graphs as G
  ( computeDomsets
  , computePostDomsets
  )
import Language.InstSel.OpStructures
import Language.InstSel.Functions
  ( Function (..)
  , mkEmptyBBLabel
  )
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.PatternMatching
  ( PatternMatch (..) )

import Data.List
  ( elemIndex
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
      essential_op_nodes =
        filter
          (\n -> isOperationNode n && (not $ isCopyNode n))
          (getAllNodes graph)
      dom_edge_data =
        map
          ( \e ->
              ( getNodeID $ getSourceNode graph e
              , getNodeID $ getTargetNode graph e
              )
          )
          (filter isDomEdge (getAllEdges graph))
      pdom_edge_data =
        map
          ( \e ->
              ( getNodeID $ getTargetNode graph e
              , getNodeID $ getSourceNode graph e
              )
          )
          (filter isPostDomEdge (getAllEdges graph))
      domsets = computeDomsets graph entry_label
      pdomsets = computePostDomsets graph
      getExecFreq n =
        fromJust $
          lookup
            (bbLabel $ getNodeType n)
            (functionBBExecFreq function)
  in HighLevelFunctionParams
       { hlFunOpNodes = nodeIDsByType isOperationNode
       , hlFunEntityNodes = nodeIDsByType isEntityNode
       , hlFunLabelNodes = nodeIDsByType isLabelNode
       , hlFunEntryLabelNode = entry_label
       , hlFunLabelDomsets = map convertDomsetN2ID domsets
       , hlFunLabelPostDomsets = map convertPostDomsetN2ID pdomsets
       , hlFunDomEdges = dom_edge_data
       , hlFunPostDomEdges = pdom_edge_data
       , hlFunBasicBlockParams =
           map
             ( \n -> HighLevelBasicBlockParams
                       { hlBBLabel = (bbLabel $ getNodeType n)
                       , hlBBLabelNode = (getNodeID n)
                       , hlBBExecFrequency = getExecFreq n
                       }
             )
             (filter isLabelNode (getAllNodes graph))
       , hlFunEssentialOpNodes = map getNodeID essential_op_nodes
       , hlFunConstraints = osConstraints $ functionOS function
       }

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
      e_ns = filter isEntityNode ns
      d_ns = filter isDataNode ns
      l_ns = filter isLabelNode ns
      c_ns = filter isControlNode ns
      e_def_ns = filter (hasAnyPredecessors graph) e_ns
      e_use_ns = filter (hasAnySuccessors graph) e_ns
      d_use_ns = filter (hasAnySuccessors graph) d_ns
      d_use_by_phi_ns = filter
                          (\n -> any isPhiNode (getSuccessors graph n))
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
       , hlMatchOpNodesCovered = findFNsInMatch match (getNodeIDs a_ns)
       , hlMatchEntityNodesDefined = findFNsInMatch match (getNodeIDs e_def_ns)
       , hlMatchEntityNodesUsed = findFNsInMatch match (getNodeIDs e_use_ns)
       , hlMatchEntryLabelNode =
           maybe Nothing (findFNInMatch match) entry_l_node_id
       , hlMatchNonEntryLabelNodes = findFNsInMatch match (getNodeIDs l_ref_ns)
       , hlMatchConstraints =
           map
             ((replaceThisMatchExprInC mid) . (replaceNodeIDsFromP2FInC match))
             (osConstraints $ patOS pattern)
       , hlMatchADDUC = patADDUC pattern
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
        f (ASRegisterOfDataNode n) = findFNInMatch m n
        f (ASImmValueOfDataNode n) = findFNInMatch m n
        f (ASBBLabelOfLabelNode n) = findFNInMatch m n
        f (ASBBLabelOfDataNode  n) = findFNInMatch m n

-- | Replaces occurrences of @ThisMatchExpr@ in a constraint with the given
-- match ID.
replaceThisMatchExprInC :: MatchID -> Constraint -> Constraint
replaceThisMatchExprInC mid c =
  let def_r = mkDefaultReconstructor
      mkMatchExpr _ ThisMatchExpr = AMatchIDExpr mid
      mkMatchExpr r expr = (mkMatchExprF r) r expr
      new_r = def_r { mkMatchExprF = mkMatchExpr }
  in apply new_r c

-- | Replaces the node IDs used in the constraint from matched pattern node IDs
-- to the corresponding function node IDs.
replaceNodeIDsFromP2FInC :: Match NodeID -> Constraint -> Constraint
replaceNodeIDsFromP2FInC match c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ (ANodeIDExpr nid) =
        ANodeIDExpr (fromJust $ findFNInMatch match nid)
      mkNodeExpr r expr = (mkNodeExprF r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c

-- | Computes the corresponding low-level version of a high-level CP model
-- instance.
lowerHighLevelModel :: HighLevelModel -> ArrayIndexMaplists -> LowLevelModel
lowerHighLevelModel model ai_maps =
  let getAIFromOpNodeID nid = fromJust $ findAIFromOpNodeID ai_maps nid
      getAIFromEntityNodeID nid = fromJust $ findAIFromEntityNodeID ai_maps nid
      getAIFromLabelNodeID nid = fromJust $ findAIFromLabelNodeID ai_maps nid
      getAIFromMatchID mid = fromJust $ findAIFromMatchID ai_maps mid
      pairWithAI get_ai_f nids = map (\nid -> (get_ai_f nid, nid)) nids
      sortByAI get_ai_f nids =
        map
          snd
          ( sortBy
              (\(ai1, _) (ai2, _) -> compare ai1 ai2)
              (pairWithAI get_ai_f nids)
          )
      f_params = hlFunctionParams model
      tm_params = hlMachineParams model
      m_params = sortByAI (getAIFromMatchID . hlMatchID) (hlMatchParams model)
  in LowLevelModel
       { llNumFunOpNodes = toInteger $ length $ hlFunOpNodes f_params
       , llNumFunEntityNodes = toInteger $ length $ hlFunEntityNodes f_params
       , llNumFunLabelNodes = toInteger $ length $ hlFunLabelNodes f_params
       , llFunEntryLabelNode =
           getAIFromLabelNodeID $ hlFunEntryLabelNode f_params
       , llFunLabelDomsets =
           map
             (\d -> map getAIFromLabelNodeID (domSet d))
             ( sortByAI
                 (getAIFromLabelNodeID . domNode)
                 (hlFunLabelDomsets f_params)
             )
       , llFunLabelPostDomsets =
           map
             (\d -> map getAIFromLabelNodeID (domSet d))
             ( sortByAI
                 (getAIFromLabelNodeID . domNode)
                 (hlFunLabelPostDomsets f_params)
             )
       , llFunLabelDomEdges =
           map
             ( \n ->
               map
                 (getAIFromEntityNodeID . snd)
                 (filter (\(n', _) -> n == n') (hlFunDomEdges f_params))
             )
             (sortByAI id (hlFunLabelNodes f_params))
       , llFunLabelPostDomEdges =
           map
             ( \n ->
               map
                 (getAIFromEntityNodeID . snd)
                 (filter (\(n', _) -> n == n') (hlFunPostDomEdges f_params))
             )
             (sortByAI id (hlFunLabelNodes f_params))
       , llFunBBExecFreqs =
           map
             hlBBExecFrequency
             ( sortByAI
                 (getAIFromLabelNodeID . hlBBLabelNode)
                 (hlFunBasicBlockParams f_params)
             )
       , llFunEssentialOpNodes = map getAIFromOpNodeID (hlFunOpNodes f_params)
       , llFunConstraints =
           map (replaceIDWithArrayIndex ai_maps) (hlFunConstraints f_params)
       , llNumRegisters = toInteger $ length $ hlMachineRegisters tm_params
       , llNumMatches = toInteger $ length m_params
       , llMatchOpNodesCovered =
           map
             (\m -> map getAIFromOpNodeID (hlMatchOpNodesCovered m))
             m_params
       , llMatchEntityNodesDefined =
           map
             (\m -> map getAIFromEntityNodeID (hlMatchEntityNodesDefined m))
             m_params
       , llMatchEntityNodesUsed =
           map
             (\m -> map getAIFromEntityNodeID (hlMatchEntityNodesUsed m))
             m_params
       , llMatchEntryLabelNode =
           map
             (maybe Nothing (Just . getAIFromLabelNodeID))
             (map hlMatchEntryLabelNode m_params)
       , llMatchNonEntryLabelNodes =
           map
             (map getAIFromLabelNodeID)
             (map hlMatchNonEntryLabelNodes m_params)
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
replaceIDWithArrayIndex ai_maps c =
  let getAIFromAnyNodeID nid = fromJust $ findAIFromAnyNodeID ai_maps nid
      getAIFromMatchID mid = fromJust $ findAIFromMatchID ai_maps mid
      getAIFromRegisterID rid = fromJust $ findAIFromRegisterID ai_maps rid
      getAIFromInstructionID iid =
        fromJust $ findAIFromInstructionID ai_maps iid
      def_r = mkDefaultReconstructor
      mkNodeExpr _ (ANodeIDExpr nid) =
        ANodeArrayIndexExpr $ getAIFromAnyNodeID nid
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      mkMatchExpr _ (AMatchIDExpr nid) =
        AMatchArrayIndexExpr $ getAIFromMatchID nid
      mkMatchExpr r expr = (mkMatchExprF def_r) r expr
      mkRegisterExpr _ (ARegisterIDExpr nid) =
        ARegisterArrayIndexExpr $ getAIFromRegisterID nid
      mkRegisterExpr r expr = (mkRegisterExprF def_r) r expr
      mkInstructionExpr _ (AnInstructionIDExpr nid) =
        AnInstructionArrayIndexExpr $ getAIFromInstructionID nid
      mkInstructionExpr r expr = (mkInstructionExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr
                    , mkMatchExprF = mkMatchExpr
                    , mkRegisterExprF = mkRegisterExpr
                    , mkInstructionExprF = mkInstructionExpr
                    }
  in apply new_r c

findAIFromOpNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIFromOpNodeID ai_maps = findArrayIndexInList (ai2OpNodeIDs ai_maps)

findAIFromEntityNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIFromEntityNodeID ai_maps = findArrayIndexInList (ai2EntityNodeIDs ai_maps)

findAIFromLabelNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIFromLabelNodeID ai_maps = findArrayIndexInList (ai2LabelNodeIDs ai_maps)

findAIFromAnyNodeID :: ArrayIndexMaplists -> NodeID -> Maybe ArrayIndex
findAIFromAnyNodeID ai_maps nid =
  let lists = [ ai2OpNodeIDs ai_maps
              , ai2EntityNodeIDs ai_maps
              , ai2LabelNodeIDs ai_maps
              ]
      matching_lists = filter (nid `elem`) lists
  in findArrayIndexInList (head matching_lists) nid

findAIFromMatchID :: ArrayIndexMaplists -> MatchID -> Maybe ArrayIndex
findAIFromMatchID ai_maps = findArrayIndexInList (ai2MatchIDs ai_maps)

findAIFromRegisterID :: ArrayIndexMaplists -> RegisterID -> Maybe ArrayIndex
findAIFromRegisterID ai_maps = findArrayIndexInList (ai2RegisterIDs ai_maps)

findAIFromInstructionID
  :: ArrayIndexMaplists
  -> InstructionID
  -> Maybe ArrayIndex
findAIFromInstructionID ai_maps =
  findArrayIndexInList (ai2InstructionIDs ai_maps)

findArrayIndexInList :: (Eq a) => [a] -> a -> Maybe ArrayIndex
findArrayIndexInList ai_list nid =
  let index = nid `elemIndex` ai_list
  in if isJust index
     then Just $ toArrayIndex $ fromJust index
     else Nothing

computeDomsets :: Graph -> NodeID -> [PostDomset Node]
computeDomsets g root_id =
  let cfg = extractCFG g
      root_n = head $ findNodesWithNodeID cfg root_id
  in G.computeDomsets cfg root_n

-- | The postdominator sets are computed in two stages. In the first stage, a
-- sink is added and a control-flow edge is added to it from every label node
-- with no successors. The postdominator set is then computed for this graph.
-- If there are any label nodes which are not postdominated by the sink, a
-- control-flow edge is added from every such label node to the sink, and the
-- postdominator set is then recomputed on the new graph.
--
-- The reason for doing this is because infinite loops will always have
-- successors and thus not get the first control-flow edge to the sink. The
-- second stage is to detect such loops and ensure to fix the erroneous
-- postdominator sets. Note that the new postdominator sets will not be complete
-- for label nodes that appear in the infinite loops, but at least they will not
-- be incorrect.
computePostDomsets :: Graph -> [PostDomset Node]
computePostDomsets g =
  let cfg0 = extractCFG g
      lnodes_wo_succ = filter (not . hasAnySuccessors cfg0) (getAllNodes cfg0)
      (cfg1, sink) = addNewNode (LabelNode mkEmptyBBLabel) cfg0
      cfg2 = addNewCtrlFlowEdges (zip lnodes_wo_succ (repeat sink)) cfg1
      pdomsets = G.computePostDomsets cfg2 sink
      pdomsets_wo_sink = filter (\s -> sink `notElem` domSet s) pdomsets
  in if null pdomsets_wo_sink
     then removeSinkFromPostDomsets pdomsets sink
     else let lnodes_wo_sink = map domNode pdomsets_wo_sink
              cfg3 = addNewCtrlFlowEdges (zip lnodes_wo_sink (repeat sink)) cfg2
              new_pdomsets = G.computePostDomsets cfg3 sink
          in removeSinkFromPostDomsets new_pdomsets sink

removeSinkFromPostDomsets :: [PostDomset Node] -> Node -> [PostDomset Node]
removeSinkFromPostDomsets sets n =
  mapMaybe
    ( \set ->
      if domNode set == n
      then Nothing
      else Just set { domSet = [ i | i <- domSet set, i /= n ] }
    )
    sets
