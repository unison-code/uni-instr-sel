-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachines.CodeEmission
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides functions for performing code emission.
--
--------------------------------------------------------------------------------

module Language.InstSel.TargetMachines.CodeEmission
  ( AssemblyCode (..)
  , generateCode
  )
where

import Language.InstSel.CPModel
import Language.InstSel.Graphs.IDs
  ( MatchID )
import Language.InstSel.Functions
  ( BasicBlockLabel )
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.Targets

import qualified Data.Graph.Inductive as I

import Data.Maybe
  ( fromJust
  , isJust
  , mapMaybe
  )



--------------
-- Data types
--------------

-- | Represents a piece of assembly code, which is either a basic block label or
-- an assembly instruction.
data AssemblyCode
  = AsmBasicBlockLabel String
  | AsmInstruction String
  deriving (Show)

-- | A data type representing a DAG where the nodes represent selected
-- instructions, and the directed edges represent control and data dependencies
-- between the instructions. Each edge represents either a data flow, a state
-- flow, or a control flow from one instruction to another.
newtype FlowDAG
  = FlowDAG { getIntDag :: IFlowDAG }

-- | Type synonym for the internal graph.
type IFlowDAG = I.Gr MatchID ()



-------------
-- Functions
-------------

-- | Produces the corresponding assembly code from a set of CP model solution
-- data.
generateCode :: CPSolutionData -> [AssemblyCode]
generateCode sol =
  let tm = ( fromJust
           $ retrieveTargetMachine
           $ machID
           $ machineParams
           $ modelParams sol
           )
  in concat $
       map
       ( \l_node ->
           let matches = getMatchesAllocatedToBB sol l_node
               sorted_matches = sortMatchesByFlow sol matches
               instrs = mapMaybe (emitInstruction sol tm) sorted_matches
               bblabel = fromJust $ findBBLabelOfLabelNode sol l_node
           in (AsmBasicBlockLabel $ show bblabel):instrs
       )
       (orderOfBBs sol)

-- | Gets the list of matches that has been allocated to a given basic block in
-- the CP model solution. The basic block is identified using the node ID of its
-- corresponding label node.
getMatchesAllocatedToBB :: CPSolutionData -> NodeID -> [MatchID]
getMatchesAllocatedToBB sol n =
  map fst $ filter (\t -> snd t == n) $ bbAllocsForMatches sol

-- | Gets the basic block label for a given label node and CP solution data. If
-- no such label can be found, @Nothing@ is returned.
findBBLabelOfLabelNode :: CPSolutionData -> NodeID -> Maybe BasicBlockLabel
findBBLabelOfLabelNode sol n =
  let bb_params = funcBasicBlockParams $ functionParams $ modelParams sol
      found_bbs = filter (\m -> bbLabelNode m == n) bb_params
  in if length found_bbs > 0
     then Just $ bbLabel $ head found_bbs
     else Nothing

-- | Sorts a list of matches according to their flow dependencies. This is done
-- by first constructing the corresponding @FlowGraph@ for the matches, and then
-- performing a topological sort on that DAG.
sortMatchesByFlow :: CPSolutionData -> [MatchID] -> [MatchID]
sortMatchesByFlow sol ms =
  let dag = mkFlowDAG sol ms
  in I.topsort' (getIntDag dag)

-- | Takes a CP solution data set and a list of match IDs, and produces a
-- control and data dependency DAG such that every match ID is represented by a
-- node, and there is a directed edge between two nodes if the match indicated
-- by the target node uses data produced by the match indicated by the source
-- node, or if the destination node represents a pattern with control
-- nodes. Cyclic data dependencies are broken such that the pattern containing
-- the phi node which makes use of the data appears at the top of the
-- DAG. Cyclic control dependencies will appear if there is more than one match
-- with control nodes in the list (which should not happen).
mkFlowDAG :: CPSolutionData -> [MatchID] -> FlowDAG
mkFlowDAG sol ms =
  let g0 = I.mkGraph (zip [0..] ms) []
      g1 = foldr (addUseEdgesToDAG sol) g0 ms
      g2 = foldr (addControlEdgesToDAG sol) g1 ms
  in FlowDAG g2

-- | Adds an edge for each use of data or state of the given match ID. If the
-- source node is not present in the graph, no edge is added. It is assumed that
-- there always exists exactly one node in the graph representing the match ID
-- given as argument to the function. Note that this may lead to cycles, which
-- will have to be broken as a second step.
addUseEdgesToDAG :: CPSolutionData -> MatchID -> IFlowDAG -> IFlowDAG
addUseEdgesToDAG sol mid g0 =
  let ds = matchParams $ modelParams sol
      pi_n = fromJust $ getNodeOfMatch g0 mid
      m_data = getMatchParams ds mid
      ns = I.labNodes g0
      d_uses_of_pi = filter
                       (`notElem` mDataNodesUsedByPhis m_data)
                       (mDataNodesUsed m_data)
      s_uses_of_pi = mStateNodesUsed m_data
      ns_d_defs =
        map (\(n, i) -> (n, mDataNodesDefined $ getMatchParams ds i)) ns
      ns_s_defs =
        map (\(n, i) -> (n, mStateNodesDefined $ getMatchParams ds i)) ns
      g1 = foldr (addUseEdgesToDAG' pi_n ns_d_defs) g0 d_uses_of_pi
      g2 = foldr (addUseEdgesToDAG' pi_n ns_s_defs) g1 s_uses_of_pi
  in g2

addUseEdgesToDAG'
  :: I.Node
  -> [(I.Node, [NodeID])]
     -- ^ List of defs.
  -> NodeID
     -- ^ A use.
  -> IFlowDAG
  -> IFlowDAG
addUseEdgesToDAG' n def_maps use g =
  let ns = map fst $ filter (\m -> use `elem` snd m) def_maps
  in foldr (\n' g' -> I.insEdge (n', n, ()) g') g ns

-- | Gets the internal node ID (if any) of the node with a given match ID as its
-- label. It is assumed that there is always at most one such node in the graph.
getNodeOfMatch :: IFlowDAG -> MatchID -> Maybe I.Node
getNodeOfMatch g mid =
  let ns = filter (\n -> snd n == mid) $ I.labNodes g
  in if length ns > 0
     then Just (fst $ head ns)
     else Nothing

-- | If the given match ID represents a pattern that has one or more control
-- nodes, then an edge will be added to the node of that match ID from every
-- other node. This is to ensure that the instruction of that pattern appears
-- last in the basic block.
addControlEdgesToDAG :: CPSolutionData -> MatchID -> IFlowDAG -> IFlowDAG
addControlEdgesToDAG sol mid g =
  let m_data = getMatchParams (matchParams $ modelParams sol) mid
  in if mHasControlNodes m_data
     then let ns = I.labNodes g
              pi_n = fst $ head $ filter (\(_, i) -> i == mid) ns
              other_ns = map fst $ filter (\(_, i) -> i /= mid) ns
          in foldr (\n' g' -> I.insEdge (n', pi_n, ()) g') g other_ns
     else g

-- | Retrieves the @MatchParams@ entity with matching match ID. It is assumed
-- that exactly one such entity always exists in the given list.
getMatchParams :: [MatchParams] -> MatchID -> MatchParams
getMatchParams ps mid = fromJust $ findMatchParams ps mid

-- | Retrieves the 'InstrPattern' entity with matching pattern ID. It is assumed
-- that such an entity always exists in the given list.
getInstrPattern :: [Instruction] -> InstructionID -> PatternID -> InstrPattern
getInstrPattern is iid pid =
  let instr = findInstruction is iid
      pat = findInstrPattern (instrPatterns $ fromJust instr) pid
  in fromJust pat

-- | Emits the assembly instruction corresponding to a given match. If that
-- match does not produce an actual assembly instruction, @Nothing@ is returned.
emitInstruction
  :: CPSolutionData
  -> TargetMachine
  -> MatchID
  -> Maybe AssemblyCode
emitInstruction sol tm mid =
  let m_params = getMatchParams (matchParams $ modelParams sol) mid
      pat_data = getInstrPattern
                   (tmInstructions tm)
                   (mInstructionID m_params)
                   (mPatternID m_params)
      instr_parts = updateNodeIDsInAsmStrParts
                       (asmStrParts $ patAsmStrTemplate pat_data)
                       (mAsmStrNodeMaps m_params)
  in if length instr_parts > 0
     then ( Just
          $ AsmInstruction
          $ concatMap
              (emitInstructionPart sol tm)
              instr_parts
          )
     else Nothing

-- | Updates the pattern graph node IDs appearing in the assembly string parts
-- with the corresponding function graph node IDs.
updateNodeIDsInAsmStrParts
  :: [AssemblyStringPart]
     -- ^ The parts to update.
  -> [Maybe NodeID]
     -- ^ The node ID mappings for the template.
  -> [AssemblyStringPart]
updateNodeIDsInAsmStrParts asm maps =
  map f (zip asm maps)
  where f (ASVerbatim str, _) = ASVerbatim str
        f (ASRegisterOfDataNode _, Just n) = ASRegisterOfDataNode n
        f (ASImmValueOfDataNode _, Just n) = ASImmValueOfDataNode n
        f (ASBBLabelOfLabelNode _, Just n) = ASBBLabelOfLabelNode n
        f (ASBBLabelOfDataNode  _, Just n) = ASBBLabelOfDataNode  n
        f _ = error "updateNodeIDsInAsmStrParts: Invalid arguments"

-- | Emits part of an assembly instruction.
emitInstructionPart
  :: CPSolutionData
  -> TargetMachine
  -> AssemblyStringPart
  -> String
emitInstructionPart _ _ (ASVerbatim s) = s
emitInstructionPart sol _ (ASImmValueOfDataNode n) =
  let i = lookup n (immValuesOfDataNodes sol)
  in if isJust i
     then show $ fromJust i
     else -- TODO: handle this case
          "i?"
emitInstructionPart sol m (ASRegisterOfDataNode n) =
  let reg_id = lookup n $ regsOfDataNodes sol
  in if isJust reg_id
     then let reg = fromJust $ findRegister (tmRegisters m) (fromJust reg_id)
          in show $ regName reg
     else -- TODO: handle this case
          "r?"
emitInstructionPart sol _ (ASBBLabelOfLabelNode n) =
  let l = findBBLabelOfLabelNode sol n
  in if isJust l
     then show $ fromJust l
     else -- TODO: handle this case
          "l?"
emitInstructionPart sol m (ASBBLabelOfDataNode n) =
  let f_params = functionParams $ modelParams sol
      data_nodes = funcDataNodes f_params
  in if n `elem` data_nodes
     then let mid = fromJust $ findDefinerOfData sol n
              l = lookup mid (bbAllocsForMatches sol)
          in if isJust l
             then emitInstructionPart sol m (ASBBLabelOfLabelNode $ fromJust l)
             else -- TODO: handle this case
                  "l?"
     else -- TODO: handle this case
          "l?"

-- | Takes the node ID of an entity, and returns the selected match that defines
-- that entity. If no such match can be found, @Nothing@ is returned.
findDefinerOfData :: CPSolutionData -> NodeID -> Maybe MatchID
findDefinerOfData sol n =
  let m_params = matchParams $ modelParams sol
      definers = map
                   mMatchID
                   (filter (\mid -> n `elem` mDataNodesDefined mid) m_params)
      selected = filter (`elem` (selectedMatches sol)) definers
  in if length selected == 1
     then Just $ head selected
     else Nothing
