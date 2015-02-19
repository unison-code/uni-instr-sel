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

import Language.InstSel.ConstraintModels
import Language.InstSel.Graphs.IDs
  ( MatchID )
import Language.InstSel.Functions
  ( BasicBlockLabel )
import Language.InstSel.TargetMachines

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

-- | Produces the corresponding assembly code for a given target machine and
-- high-level CP model and solution.
generateCode
  :: TargetMachine
  -> HighLevelModel
  -> HighLevelSolution
  -> [AssemblyCode]
generateCode target model sol =
  concat $
    map
    ( \l_node ->
        let matches = getMatchesAllocatedToBB sol l_node
            sorted_matches = sortMatchesByFlow model matches
            instrs = mapMaybe (emitInstruction model sol target) sorted_matches
            bblabel = fromJust $ findBBLabelOfLabelNode model l_node
        in (AsmBasicBlockLabel $ show bblabel):instrs
    )
    (hlSolOrderOfBBs sol)

-- | Gets the list of matches that has been allocated to a given basic block in
-- the CP model solution. The basic block is identified using the node ID of its
-- corresponding label node.
getMatchesAllocatedToBB :: HighLevelSolution -> NodeID -> [MatchID]
getMatchesAllocatedToBB sol n =
  map fst $ filter (\t -> snd t == n) $ hlSolBBAllocsForSelMatches sol

-- | Gets the basic block label for a given label node. If no such label can be
-- found, @Nothing@ is returned.
findBBLabelOfLabelNode :: HighLevelModel -> NodeID -> Maybe BasicBlockLabel
findBBLabelOfLabelNode model n =
  let bb_params = hlFunBasicBlockParams $ hlFunctionParams model
      found_bbs = filter (\m -> hlBBLabelNode m == n) bb_params
  in if length found_bbs > 0
     then Just $ hlBBLabel $ head found_bbs
     else Nothing

-- | Sorts a list of matches according to their flow dependencies. This is done
-- by first constructing the corresponding @FlowGraph@ for the matches, and then
-- performing a topological sort on that DAG.
sortMatchesByFlow :: HighLevelModel -> [MatchID] -> [MatchID]
sortMatchesByFlow model ms =
  let dag = mkFlowDAG model ms
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
mkFlowDAG :: HighLevelModel -> [MatchID] -> FlowDAG
mkFlowDAG model ms =
  let g0 = I.mkGraph (zip [0..] ms) []
      g1 = foldr (addUseEdgesToDAG model) g0 ms
      g2 = foldr (addControlEdgesToDAG model) g1 ms
  in FlowDAG g2

-- | Adds an edge for each use of data or state of the given match ID. If the
-- source node is not present in the graph, no edge is added. It is assumed that
-- there always exists exactly one node in the graph representing the match ID
-- given as argument to the function. Note that this may lead to cycles, which
-- will have to be broken as a second step.
addUseEdgesToDAG :: HighLevelModel -> MatchID -> IFlowDAG -> IFlowDAG
addUseEdgesToDAG model mid g0 =
  let ds = hlMatchParams model
      match_node = fromJust $ getNodeOfMatch g0 mid
      match = getHLMatchParams ds mid
      ns = I.labNodes g0
      uses_of_m = filter
                    (`notElem` hlMatchDataNodesUsedByPhis match)
                    (hlMatchEntityNodesUsed match)
      defs_of_m =
        map
          (\(n, i) -> (n, hlMatchEntityNodesDefined $ getHLMatchParams ds i))
          ns
      g1 = foldr (addUseEdgesToDAG' match_node defs_of_m) g0 uses_of_m
  in g1

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
addControlEdgesToDAG :: HighLevelModel -> MatchID -> IFlowDAG -> IFlowDAG
addControlEdgesToDAG model mid g =
  let match = getHLMatchParams (hlMatchParams model) mid
  in if hlMatchHasControlNodes match
     then let ns = I.labNodes g
              pi_n = fst $ head $ filter (\(_, i) -> i == mid) ns
              other_ns = map fst $ filter (\(_, i) -> i /= mid) ns
          in foldr (\n' g' -> I.insEdge (n', pi_n, ()) g') g other_ns
     else g

-- | Retrieves the @HighLevelMatchParams@ entity with matching match ID. It is
-- assumed that exactly one such entity always exists in the given list.
getHLMatchParams :: [HighLevelMatchParams] -> MatchID -> HighLevelMatchParams
getHLMatchParams ps mid = head $ filter (\p -> hlMatchID p == mid) ps

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
  :: HighLevelModel
  -> HighLevelSolution
  -> TargetMachine
  -> MatchID
  -> Maybe AssemblyCode
emitInstruction model sol tm mid =
  let match = getHLMatchParams (hlMatchParams model) mid
      pat_data = getInstrPattern
                   (tmInstructions tm)
                   (hlMatchInstructionID match)
                   (hlMatchPatternID match)
      instr_parts = updateNodeIDsInAsmStrParts
                       (asmStrParts $ patAsmStrTemplate pat_data)
                       (hlMatchAsmStrNodeMaplist match)
  in if length instr_parts > 0
     then ( Just
          $ AsmInstruction
          $ concatMap
              (emitInstructionPart model sol tm)
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
  :: HighLevelModel
  -> HighLevelSolution
  -> TargetMachine
  -> AssemblyStringPart
  -> String
emitInstructionPart _ _ _ (ASVerbatim s) = s
emitInstructionPart _ sol _ (ASImmValueOfDataNode n) =
  let i = lookup n (hlSolImmValuesOfDataNodes sol)
  in if isJust i
     then show $ fromJust i
     else -- TODO: handle this case
          "i?"
emitInstructionPart _ sol m (ASRegisterOfDataNode n) =
  let reg_id = lookup n $ hlSolRegsOfDataNodes sol
  in if isJust reg_id
     then let reg = fromJust $ findRegister (tmRegisters m) (fromJust reg_id)
          in show $ regName reg
     else -- TODO: handle this case
          "r?"
emitInstructionPart model _ _ (ASBBLabelOfLabelNode n) =
  let l = findBBLabelOfLabelNode model n
  in if isJust l
     then show $ fromJust l
     else -- TODO: handle this case
          "l?"
emitInstructionPart model sol m (ASBBLabelOfDataNode n) =
  let function = hlFunctionParams model
      entity_nodes = hlFunEntityNodes function
  in if n `elem` entity_nodes
     then let mid = fromJust $ findDefinerOfData model sol n
              l = lookup mid (hlSolBBAllocsForSelMatches sol)
          in if isJust l
             then emitInstructionPart
                    model
                    sol
                    m
                    (ASBBLabelOfLabelNode $ fromJust l)
             else -- TODO: handle this case
                  "l?"
     else -- TODO: handle this case
          "l?"

-- | Takes the node ID of an entity, and returns the selected match that defines
-- that entity. If no such match can be found, @Nothing@ is returned.
findDefinerOfData
  :: HighLevelModel
  -> HighLevelSolution
  -> NodeID
  -> Maybe MatchID
findDefinerOfData model sol n =
  let match = hlMatchParams model
      definers = map
                   hlMatchID
                   ( filter
                       (\mid -> n `elem` hlMatchEntityNodesDefined mid)
                       match
                   )
      selected = filter (`elem` (hlSolSelMatches sol)) definers
  in if length selected == 1
     then Just $ head selected
     else Nothing
