{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.CodeEmission
  ( AssemblyCode (..)
  , generateCode
  )
where

import Language.InstrSel.ConstraintModels
import Language.InstrSel.Graphs.IDs
  ( MatchID )
import Language.InstrSel.Functions
  ( BlockName )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines

import qualified Data.Graph.Inductive as I

import Data.Maybe
  ( fromJust
  , isJust
  )



--------------
-- Data types
--------------

-- | Represents a piece of assembly code, which is either a block or an assembly
-- instruction.
data AssemblyCode
  = AsmBlock String
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

-- | A data type containing various information required in order to emit the
-- assembly code.
data EmissionState
  = EmissionState { emittedCode :: [AssemblyCode]
                    -- ^ Code that has already been emitted. For efficiency
                    -- reasons, the list will be given in reverse emission order
                    -- (that is, the last emission will appear first in the
                    -- list).
                  , varNamesInUse :: [String]
                    -- ^ Variable names that are already in use or will be used
                    -- at a later point during emission.
                  , tmpToVarNameMaps :: [(Int, String)]
                    -- ^ Mappings from a temporary identifier to a variable
                    -- name. These only apply within the scope of an emit string
                    -- template and must therefore be reset when moving from one
                    -- instruction to another.
                  , valueNodeAliases :: [(NodeID, NodeID)]
                    -- ^ Mappings from a value node that should be replaced by
                    -- another value node. The first element contains the ID of
                    -- the node to be replaced, and the second element contains
                    -- the ID of the node to replace with.
                  }
  deriving (Show)



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
generateCode target model sol@(HighLevelSolution {}) =
  reverse $
  emittedCode $
  foldl ( \st0 b ->
          let matches = getMatchesPlacedInBlock sol b
              sorted_matches = sortMatchesByFlow sol model matches
              block_name = fromJust $ findNameOfBlockNode model b
              code = AsmBlock $ pShow block_name
              st1 = st0 { emittedCode = (code:emittedCode st0) }
              st2 = foldl ( \st' m ->
                            emitInstructionsOfMatch
                              model
                              sol
                              target
                              (st' { tmpToVarNameMaps = [] })
                              m
                          )
                          st1
                          sorted_matches
          in st2
        )
        (mkInitState sol model)
        (hlSolOrderOfBlocks sol)

generateCode _ _ NoHighLevelSolution =
  error "generateCode: cannot generate code from no solution"

-- | Returns an initial emission state.
mkInitState :: HighLevelSolution -> HighLevelModel -> EmissionState
mkInitState sol model =
  let sel_matches = hlSolSelMatches sol
      null_matches = filter hlMatchIsNullInstruction $
                     map (getHLMatchParams (hlMatchParams model)) $
                     sel_matches
      op_maps = hlSolNodesOfOperands sol
      const_ns = map fst $ hlFunValueIntConstData $ hlFunctionParams model
      aliases = computeAliases op_maps const_ns null_matches
  in EmissionState { emittedCode = []
                   , varNamesInUse = getVarNamesInUse model
                   , tmpToVarNameMaps = []
                   , valueNodeAliases = aliases
                   }

-- | Computes the aliases where usage of a data node defined by a null
-- instruction would be aliased to the data node used by the null
-- instruction. This is computed transitively until all data nodes are aliased
-- to a data node defined by either a non-null instruction or an instruction
-- that does not take any input. However, if the null instruction uses a data
-- node representing a constant, then its defined data node is not aliased.
computeAliases
  :: [(OperandID, NodeID)]
  -> [NodeID]
  -> [HighLevelMatchParams]
  -> [(NodeID, NodeID)]
computeAliases op_maps const_ns null_matches =
  let getNodeID o = fromJust $ lookup o op_maps
      aliases = filter (\(_, n) -> n `notElem` const_ns) $
                map ( \m -> ( getNodeID $ head $ hlMatchOperandsDefined m
                            , getNodeID $ head $ hlMatchOperandsUsed m
                            )
                    ) $
                filter ( \m -> length (hlMatchOperandsDefined m) == 1 &&
                               length (hlMatchOperandsUsed m) == 1
                       )
                null_matches
      normalize as =
        let alias_refs = filter (\(_, b) -> b `elem` (map fst as)) as
        in if length alias_refs > 0
           then let new_as = map ( \(a, b) ->
                                   if (a, b) `elem` alias_refs
                                   then (a, fromJust $ lookup b as)
                                   else (a, b)
                                 ) $
                             as
                in normalize new_as
           else as
      normalized_aliases = normalize aliases
  in normalized_aliases

-- | Gets the list of matches that has been allocated to a given block in the CP
-- model solution. The block is identified using the node ID of its
-- corresponding block node.
getMatchesPlacedInBlock :: HighLevelSolution -> NodeID -> [MatchID]
getMatchesPlacedInBlock sol n =
  map fst $ filter (\t -> snd t == n) $ hlSolBlocksOfSelMatches sol

-- | Gets a list of variable names that are already in use in the
-- 'HighLevelModel'.
getVarNamesInUse :: HighLevelModel -> [String]
getVarNamesInUse m = map snd $ hlFunValueOriginData $ hlFunctionParams m

-- | Gets the block name for a given block node. If no such block can be found,
-- 'Nothing' is returned.
findNameOfBlockNode :: HighLevelModel -> NodeID -> Maybe BlockName
findNameOfBlockNode model n =
  let bb_params = hlFunBlockParams $ hlFunctionParams model
      found_bbs = filter (\m -> hlBlockNode m == n) bb_params
  in if length found_bbs > 0
     then Just $ hlBlockName $ head found_bbs
     else Nothing

-- | Sorts a list of matches according to their flow dependencies. This is done
-- by first constructing the corresponding 'FlowDAG' for the matches, and then
-- performing a topological sort on that DAG.
sortMatchesByFlow
  :: HighLevelSolution
  -> HighLevelModel
  -> [MatchID]
  -> [MatchID]
sortMatchesByFlow sol model ms =
  let dag = mkFlowDAG sol model ms
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
mkFlowDAG :: HighLevelSolution -> HighLevelModel -> [MatchID] -> FlowDAG
mkFlowDAG sol model ms =
  let g0 = I.mkGraph (zip [0..] ms) []
      g1 = foldr (addUseEdgesToDAG sol model) g0 ms
      g2 = foldr (addControlEdgesToDAG sol model) g1 ms
  in FlowDAG g2

-- | Adds an edge for each use of data or state of the given match ID. If the
-- source node is not present in the graph, no edge is added. It is assumed that
-- there always exists exactly one node in the graph representing the match ID
-- given as argument to the function. Note that this may lead to cycles, which
-- will have to be broken as a second step.
addUseEdgesToDAG
  :: HighLevelSolution
  -> HighLevelModel
  -> MatchID
  -> IFlowDAG
  -> IFlowDAG
addUseEdgesToDAG sol model mid g0 =
  let getNodeID o = fromJust $ lookup o $ hlSolNodesOfOperands sol
      ds = hlMatchParams model
      match_node = fromJust $ getNodeOfMatch g0 mid
      match = getHLMatchParams ds mid
      ns = I.labNodes g0
      op_uses_of_m = filter ( `notElem` ( map snd
                                          $ hlMatchOperandsUsedByPhis match
                                        )
                            ) $
                     hlMatchOperandsUsed match
      d_uses_of_m = map getNodeID op_uses_of_m
      defs_of_m =
        map ( \(n, i) -> ( n
                         , map getNodeID
                           $ hlMatchOperandsDefined $ getHLMatchParams ds i
                         )
            ) $
        ns
      g1 = foldr (addUseEdgesToDAG' match_node defs_of_m) g0 d_uses_of_m
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
-- block. It is assumed that there is always at most one such node in the graph.
getNodeOfMatch :: IFlowDAG -> MatchID -> Maybe I.Node
getNodeOfMatch g mid =
  let ns = filter (\n -> snd n == mid) $ I.labNodes g
  in if length ns > 0
     then Just $ fst $ head ns
     else Nothing

-- | If the given match ID represents a pattern that has one or more control
-- nodes, then an edge will be added to the node of that match ID from every
-- other node. This is to ensure that the instruction of that pattern appears
-- last in the block.
addControlEdgesToDAG
  :: HighLevelSolution
  -> HighLevelModel
  -> MatchID
  -> IFlowDAG
  -> IFlowDAG
addControlEdgesToDAG _ model mid g =
  let match = getHLMatchParams (hlMatchParams model) mid
  in if hlMatchHasControlFlow match
     then let ns = I.labNodes g
              pi_n = fst $ head $ filter (\(_, i) -> i == mid) ns
              other_ns = map fst $ filter (\(_, i) -> i /= mid) ns
          in foldr (\n' g' -> I.insEdge (n', pi_n, ()) g') g other_ns
     else g

-- | Retrieves the 'HighLevelMatchParams' entity with matching match ID. It
-- is assumed that exactly one such entity always exists in the given list.
getHLMatchParams
  :: [HighLevelMatchParams]
  -> MatchID
  -> HighLevelMatchParams
getHLMatchParams ps mid = head $ filter (\p -> hlMatchID p == mid) ps

-- | Retrieves the 'InstrPattern' entity with matching pattern ID. It is assumed
-- that such an entity always exists in the given list.
getInstrPattern :: TargetMachine -> InstructionID -> PatternID -> InstrPattern
getInstrPattern tm iid pid =
  let instr = findInstruction tm iid
      pat = findInstrPattern (instrPatterns $ fromJust instr) pid
  in fromJust pat

-- | Emits the instructions for a given match. Each part of the code is emitted
-- by appending strings to the instruction currently at the head of the list of
-- already-emitted code.
emitInstructionsOfMatch
  :: HighLevelModel
  -> HighLevelSolution
  -> TargetMachine
  -> EmissionState
  -> MatchID
  -> EmissionState
emitInstructionsOfMatch model sol tm st0 mid =
  let replaceAliases Nothing = Nothing
      replaceAliases (Just n) = let alias = lookup n $ valueNodeAliases st0
                                in if isJust alias then alias else (Just n)
      fetchNodeID (Just (Right nid)) = Just nid
      fetchNodeID (Just (Left oid)) =
        let nid = lookup oid (hlSolNodesOfOperands sol)
        in if isJust nid
           then nid
           else error $ "fetchNodeID: no mapping for operand ID " ++ pShow oid
      fetchNodeID Nothing = Nothing
      match = getHLMatchParams (hlMatchParams model) mid
      pat_data = getInstrPattern tm
                                 (hlMatchInstructionID match)
                                 (hlMatchPatternID match)
      emit_parts = updateNodeIDsInEmitStrParts
                     (emitStrParts $ patEmitString pat_data)
                     ( map (map (replaceAliases . fetchNodeID))
                           (hlMatchEmitStrNodeMaplist match)
                     )
  in foldl ( \st1 parts ->
             foldl (emitInstructionPart model sol tm)
                   (st1 { emittedCode = (AsmInstruction "":emittedCode st1) }) $
             parts
           )
           st0
           emit_parts

-- | Updates the pattern graph node IDs appearing in the content of an
-- 'EmitStringTemplate' with the corresponding function graph node IDs.
updateNodeIDsInEmitStrParts
  :: [[EmitStringPart]]
  -> [[Maybe NodeID]]
     -- ^ The node ID mappings for the template.
  -> [[EmitStringPart]]
updateNodeIDsInEmitStrParts emit_strs maps =
  if length emit_strs == length maps
  then let f p@(ESVerbatim {})         _        = p
           f (ESLocationOfValueNode _) (Just n) = ESLocationOfValueNode n
           f (ESIntConstOfValueNode _) (Just n) = ESIntConstOfValueNode n
           f (ESNameOfBlockNode     _) (Just n) = ESNameOfBlockNode n
           f (ESBlockOfValueNode    _) (Just n) = ESBlockOfValueNode n
           f (ESFuncOfCallNode      _) (Just n) = ESFuncOfCallNode n
           f p@(ESLocalTemporary {})         _  = p
           f _ _ = error $ "updateNodeIDsInEmitStrParts: unexpected " ++
                           "combination of arguments"
       in zipWith ( \es ms ->
                    if length es == length ms
                    then zipWith f es ms
                    else error $ "updateNodeIDsInEmitStrParts: arguments " ++
                                 "not of same length"
                  )
                  emit_strs
                  maps
  else error "updateNodeIDsInEmitStrParts: arguments not of same length"

-- | Emits part of an assembly instruction.
emitInstructionPart
  :: HighLevelModel
  -> HighLevelSolution
  -> TargetMachine
  -> EmissionState
  -> EmitStringPart
  -> EmissionState
emitInstructionPart _ _ _ st (ESVerbatim s) =
  let code = emittedCode st
      (AsmInstruction instr_str) = head code
      new_instr = AsmInstruction $ instr_str ++ s
  in st { emittedCode = (new_instr:tail code) }
emitInstructionPart model _ _ st (ESIntConstOfValueNode n) =
  let i = lookup n (hlFunValueIntConstData $ hlFunctionParams model)
  in if isJust i
     then let code = emittedCode st
              (AsmInstruction instr_str) = head code
              new_instr = AsmInstruction $ instr_str ++ (pShow $ fromJust i)
          in st { emittedCode = (new_instr:tail code) }
     else error $ "emitInstructionPart: no integer constant found for " ++
                  "function node " ++ pShow n
emitInstructionPart model sol tm st (ESLocationOfValueNode n) =
  let reg_id = lookup n $ hlSolLocationsOfData sol
  in if isJust reg_id
     then let code = emittedCode st
              (AsmInstruction instr_str) = head code
              reg = fromJust $ findLocation tm (fromJust reg_id)
              origin = lookup n ( hlFunValueOriginData
                                  $ hlFunctionParams model
                                )
          in if (isJust $ locValue reg)
             then let new_instr = AsmInstruction $ instr_str
                                  ++ (pShow $ locName reg)
                  in st { emittedCode = (new_instr:tail code) }
             else if isJust origin
                  then let new_instr = AsmInstruction $ instr_str
                                       ++ fromJust origin
                  in st { emittedCode = (new_instr:tail code) }
                  else error $ "emitInstructionPart: no origin found for " ++
                               "function node " ++ pShow n
     else error $ "emitInstructionPart: no location found for "
                  ++ "function node " ++ pShow n
emitInstructionPart model _ _ st (ESNameOfBlockNode n) =
  let l = findNameOfBlockNode model n
  in if isJust l
     then let code = emittedCode st
              (AsmInstruction instr_str) = head code
              new_instr = AsmInstruction $ instr_str ++ (pShow $ fromJust l)
          in st { emittedCode = (new_instr:tail code) }
     else error $ "emitInstructionPart: no block name found for function " ++
                  "node " ++ pShow n
emitInstructionPart model sol m st (ESBlockOfValueNode n) =
  let function = hlFunctionParams model
      data_nodes = hlFunData function
  in if n `elem` data_nodes
     then let mid = getDefinerOfData model sol n
              l = lookup mid (hlSolBlocksOfSelMatches sol)
          in if isJust l
             then emitInstructionPart model
                                     sol
                                     m
                                     st
                                     (ESNameOfBlockNode $ fromJust l)
             else error $ "emitInstructionPart: found no block assigned to " ++
                          "function node " ++ pShow n
     else error $ "emitInstructionPart: function node " ++ pShow n ++
                  " is not part of the function's data nodes"
emitInstructionPart _ _ _ st (ESLocalTemporary i) =
  let code = emittedCode st
      (AsmInstruction instr_str) = head code
      name = lookup i (tmpToVarNameMaps st)
  in if isJust name
     then let new_instr = AsmInstruction $ instr_str ++ (fromJust name)
          in st { emittedCode = (new_instr:tail code) }
     else let names_in_use = varNamesInUse st
              new_name = getUniqueVarName names_in_use
              new_instr = AsmInstruction $ instr_str ++ new_name
          in st { emittedCode = (new_instr:tail code)
                , varNamesInUse = (new_name:names_in_use)
                , tmpToVarNameMaps = ((i, new_name):tmpToVarNameMaps st)
                }
emitInstructionPart model _ _ st (ESFuncOfCallNode n) =
  let f = lookup n (hlFunCallNameData $ hlFunctionParams model)
  in if isJust f
     then let code = emittedCode st
              (AsmInstruction instr_str) = head code
              new_instr = AsmInstruction $ instr_str ++ (fromJust f)
          in st { emittedCode = (new_instr:tail code) }
     else error $ "emitInstructionPart: no function name found for " ++
                  "function call node " ++ pShow n

-- | Returns a variable name that does not appear in the given list of strings.
getUniqueVarName :: [String] -> String
getUniqueVarName used =
  head $
  dropWhile (`elem` used) $
  map (\i -> "%tmp." ++ show i) $
  ([1..] :: [Integer]) -- Cast is needed or GHC will complain

-- | Takes the node ID of a datum, and returns the selected match that defines
-- that datum.
getDefinerOfData
  :: HighLevelModel
  -> HighLevelSolution
  -> NodeID
  -> MatchID
getDefinerOfData model sol n =
  let findOp maps nid =
        let ms = filter (elem nid . snd) maps
        in if length ms > 0 then Just $ fst $ head ms else Nothing
      matches = hlMatchParams model
      definers = map hlMatchID $
                 filter ( \m -> let o = findOp (hlOperandNodeMaps m) n
                                in if isJust o
                                   then (fromJust o)
                                        `elem`
                                        hlMatchOperandsDefined m
                                   else False
                        ) $
                 matches
      selected = filter (`elem` (hlSolSelMatches sol)) definers
  in if length selected == 1
     then head selected
     else error $ "getDefinerOfData: no or multiple matches found that " ++
                  "define data with node ID " ++ pShow n
