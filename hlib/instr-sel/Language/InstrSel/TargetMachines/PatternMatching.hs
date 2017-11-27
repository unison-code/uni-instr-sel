{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

Contributing authors:
  Roberto Castaneda Lozano <rcas@sics.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , PatternMatchset (..)
  , mkPatternMatchset
  , findPatternMatchesWithMatchID
  , getInstructionFromPatternMatch
  )
where

import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Functions.IDs
  ( mkEmptyBlockName )
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Graphalyze
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
import Language.InstrSel.Utils
  ( combinations
  , groupBy
  , toPair
  )
import qualified Language.InstrSel.Utils.Set as S
import Language.InstrSel.Utils.JSON

import Data.List
  ( nub
  , sort
  )
import Data.Maybe
  ( isJust
  , fromJust
  )

import qualified Data.Map as M

import Control.DeepSeq
  ( NFData
  , rnf
  )



--------------
-- Data types
--------------

-- | Contains the matchset information; that is, the information to determine
-- which target machine the matchset concerns, along the match data.
data PatternMatchset
  = PatternMatchset
      { pmTarget :: TargetMachineID
      , pmMatches :: [PatternMatch]
      , pmTime :: Maybe Double
        -- ^ Number of seconds it took to find this pattern matchset.
      }
  deriving (Show)

-- | Contains the information needed to identify which instruction and pattern a
-- given match originates from. Each match is also given a 'MatchID' that must
-- be unique (although not necessarily continuous) for every match within a list
-- of 'PatternMatch'.
data PatternMatch
  = PatternMatch
      { pmInstrID :: InstructionID
      , pmMatchID :: MatchID
      , pmMatch :: Match NodeID
      }
  deriving (Show)

-- | Intermediate data structure for producing a 'PatternMatch'
data IntPatternMatch
  = IntPatternMatch
      { ipmInstrID :: InstructionID
      , ipmMatch :: Match Node
      , ipmHasCheckedCyclicDataDep :: Bool
        -- ^ Whether a cyclic data dependency check has already been done on
        -- this match.
      }
  deriving (Show)



--------------------------------
-- JSON-related class instances
--------------------------------

instance FromJSON PatternMatchset where
  parseJSON (Object v) =
    PatternMatchset
      <$> v .: "target-machine-id"
      <*> v .: "match-data"
      <*> v .: "time"
  parseJSON _ = mzero

instance ToJSON PatternMatchset where
  toJSON m =
    object [ "target-machine-id" .= (pmTarget m)
           , "match-data"        .= (pmMatches m)
           , "time"              .= (pmTime m)
           ]

instance FromJSON PatternMatch where
  parseJSON (Object v) =
    PatternMatch
      <$> v .: "instr-id"
      <*> v .: "match-id"
      <*> v .: "match"
  parseJSON _ = mzero

instance ToJSON PatternMatch where
  toJSON m =
    object [ "instr-id"   .= (pmInstrID m)
           , "match-id"   .= (pmMatchID m)
           , "match"      .= (pmMatch m)
           ]



----------------------------------------
-- DeepSeq-related type class instances
--
-- These are needed to be able to time
-- how long it takes to produce the
-- matchsets
----------------------------------------

instance NFData PatternMatchset where
  rnf (PatternMatchset a b c) = rnf a `seq` rnf b `seq` rnf c

instance NFData PatternMatch where
  rnf (PatternMatch a b c) = rnf a `seq` rnf b `seq` rnf c



-------------
-- Functions
-------------

-- | Finds the pattern matches in a given pattern matchset with a given match
-- ID.
findPatternMatchesWithMatchID :: PatternMatchset -> MatchID -> [PatternMatch]
findPatternMatchesWithMatchID pms mid =
  let matches = pmMatches pms
  in filter ((mid ==) . pmMatchID) matches

-- | Produces the pattern matchset for a given function and target machine.
mkPatternMatchset :: Function -> TargetMachine -> PatternMatchset
mkPatternMatchset function target =
  let fg = osGraph $ functionOS function
      int_matches = removeMatchesWithCyclicDataDeps fg $
                    concatMap (processInstr function) $
                    tmInstructions target
      matches = mkPatternMatches int_matches
  in PatternMatchset { pmTarget = tmID target
                     , pmMatches = matches
                     , pmTime = Nothing
                     }

mkPatternMatches :: [IntPatternMatch] -> [PatternMatch]
mkPatternMatches int_ms =
  let int_ms_ps = zip int_ms [0..]
      mkPatternMatch (m, mid) =
        PatternMatch { pmInstrID = ipmInstrID m
                     , pmMatchID = mid
                     , pmMatch = convertMatchN2ID $ ipmMatch m
                     }
  in map mkPatternMatch int_ms_ps

processInstr :: Function -> Instruction -> [IntPatternMatch]
processInstr fun instr =
  let os = functionOS fun
      fg = osGraph os
      dup_fg = duplicateNodes fg
      entry = let e = entryBlockNode fg
              in if isJust e
                 then fromJust e
                 else error "processInstr: function has no entry block node"
      pg = osGraph $ instrOS instr
      dup_pg = duplicateNodes pg
      matches = if not (isInstructionSimd instr)
                then map (\m -> (m, False)) $
                     map (fixMatch fg pg) $
                     removeDupMatches $
                     findMatches dup_fg dup_pg
                else let pg_cs = weakComponentsOf dup_pg
                         sub_pg = head pg_cs
                         sub_matches = removeDupMatches $
                                       findMatches dup_fg sub_pg
                     in map (\m -> (m, True)) $
                        pruneNonselectableSimdMatches fg entry $
                        map (fixMatch fg pg) $
                        mkSimdMatches dup_fg sub_pg sub_matches (tail pg_cs)
  in map ( \(m, b) -> IntPatternMatch { ipmInstrID = instrID instr
                                      , ipmMatch = m
                                      , ipmHasCheckedCyclicDataDep = b
                                      }
         ) $
     matches

-- | Constructs a list of matches for the entire SIMD pattern graph based on its
-- weakly connected components and list of matches for a single component. Each
-- match is also checked that it does not result in a cyclic data dependency
-- between any of the components.
mkSimdMatches
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ A weakly connected component of the SIMD pattern graph.
  -> [Match Node]
     -- ^ List of matches found for the component above in the function graph.
  -> [Graph]
     -- ^ List of components remaining in the SIMD pattern graph.
  -> [Match Node]
     -- ^ List of (non-cyclic data dependency) matches for the entire SIMD
     -- pattern graph.
mkSimdMatches fg sub_pg sub_matches pg_remaining_cs =
  let reassignMatch (pg_c_m, sub_m) =
        let reassign m =
              let pn = pNode m
                  fn = fNode m
                  new_pn = findPNInMatch pg_c_m pn
              in if length new_pn == 1
                 then Mapping { fNode = fn, pNode = head new_pn }
                 else if length new_pn == 0
                      then  error $ "mkSimdMatches: could not find a mapping "++
                                    "for function node with ID " ++
                                    (pShow $ getNodeID pn)
                      else  error $ "mkSimdMatches: found multiple mappings "++
                                    "for function node with ID " ++
                                    (pShow $ getNodeID pn)
        in toMatch $
           map reassign $
           fromMatch sub_m
           -- Since we will update all mappings, it will most likely make no
           -- performance difference if we create an entirely new Match instead
           -- of updating the existing Match
      sub_pg_cs_matches = map head $
                          map (findMatches sub_pg) $
                          pg_remaining_cs
      cdd_rel = computeMatchCyclicDataDepRelSet fg sub_matches
      m_combs = filter ( \ms ->
                         let ps = map toPair $ combinations 2 ms
                         in all ( \(m1, m2) -> M.notMember (m1, m2) cdd_rel &&
                                               M.notMember (m2, m1) cdd_rel
                                )
                            ps
                       ) $
                combinations (1 + (length pg_remaining_cs)) sub_matches
      simd_ms = map ( \(m:ms) ->
                      let fixed_ms = map reassignMatch $
                                     zip sub_pg_cs_matches ms
                      in mergeMatches (m:fixed_ms)
                    )
                    m_combs
  in simd_ms

-- | Removes 'PatternMatch's which have cyclic data dependencies, and are
-- therefore illegal matches.
removeMatchesWithCyclicDataDeps
  :: Graph
  -> [IntPatternMatch]
  -> [IntPatternMatch]
removeMatchesWithCyclicDataDeps fg pms =
  let ssa_fg = extractSSAG fg
  in filter ( \pm -> if ipmHasCheckedCyclicDataDep pm
                     then True
                     else not $ hasMatchCyclicDataDep ssa_fg (ipmMatch pm)
            ) $
     pms

-- | Checks if a given match will result in a cyclic data dependency in the
-- function graph. The check works as follows: first the subgraph of the
-- function covered by the match is extracted. From this subgraph, each weakly
-- connected component is extracted, and then it is checked whether a component
-- is reachable from any other component. If so, then the match has a cyclic
-- dependency. However, components that are only reachable via an input node to
-- the pattern is not considered a dependency. Due to this, such data nodes are
-- removed prior to extracting the components. Also, phi nodes not covered by
-- the match as well as any state-flow edges should not be included when
-- checking dependencies.
hasMatchCyclicDataDep
  :: Graph
     -- ^ The corresponding SSA graph of the function graph.
  -> Match Node
  -> Bool
hasMatchCyclicDataDep ssa_fg m =
  let f_ns = map fNode (fromMatch m)
      sub_fg = removeInputNodes $
               subGraph ssa_fg f_ns
      mcs = weakComponentsOf sub_fg
      noncov_phi_ns = filter (null . findPNInMatch m) $
                      filter isPhiNode $
                      getAllNodes ssa_fg
      ssa_fg_to_query = removeStateFlowEdges $
                        foldr delNode ssa_fg noncov_phi_ns
      cdd = or [ isReachableComponent ssa_fg_to_query c1 c2
               | c1 <- mcs, c2 <- mcs, getAllNodes c1 /= getAllNodes c2
               ]
  in cdd

-- | For a given function graph and list of matches, computes a relation R(m1,
-- m2) which holds if two matches m1 and m2 have a cyclic data dependency
-- between them. The returned relation is not commutative, meaning both
-- combinations need to be checked.
computeMatchCyclicDataDepRelSet
  :: Graph
  -> [Match Node]
  -> M.Map (Match Node, Match Node) Bool
     -- ^ A relation between two matches that holds if there is a cyclic data
     -- dependency between them.
computeMatchCyclicDataDepRelSet fg ms =
  let ssa_fg = extractSSAG fg
      m_sub_fgs = zip ms $
                  map ( \m -> removeInputNodes $
                              subGraph ssa_fg $
                              map fNode (fromMatch m)
                      ) $
                  ms
      hasDep (m1, m2) set =
        let sub_fg1 = fromJust $ lookup m1 m_sub_fgs
            sub_fg2 = fromJust $ lookup m2 m_sub_fgs
            has_dep = isReachableComponent ssa_fg sub_fg1 sub_fg2 ||
                      isReachableComponent ssa_fg sub_fg2 sub_fg1
        in if has_dep
           then M.insert (m1, m2) True set
           else set
      m_ps = map toPair $ combinations 2 ms
  in foldr hasDep M.empty m_ps

-- | Removes all data nodes from the given graph that act as input.
removeInputNodes :: Graph -> Graph
removeInputNodes g =
  foldr delNode g $
  filter ( \n -> isDatumNode n &&
                 not (hasAnyPredecessors g n)
         ) $
  getAllNodes g

-- | Removes all state-flow edges from the given graph.
removeStateFlowEdges :: Graph -> Graph
removeStateFlowEdges g =
  foldr delEdge g $
  filter isStateFlowEdge $
  getAllEdges g

-- | Sometimes we want pattern nodes to be mapped to the same function
-- node. However, the VF2 algorithm doesn't allow that. To get around this
-- limitation, we duplicate these nodes and then remap the pattern nodes after a
-- match is found.
duplicateNodes :: Graph -> Graph
duplicateNodes fg =
  let ns = getAllNodes fg
      bs = filter isBlockNode ns
      bs_with_inout_defs = filter ( \n -> length (getDefInEdges fg n) > 0 &&
                                          length (getDefOutEdges fg n) > 0
                                  )
                                  bs
      processBlockNode b fg0 =
        let (fg1, new_b) = addNewNode (BlockNode mkEmptyBlockName) fg0
            fg2 = updateNodeID (getNodeID b) new_b fg1
            fg3 = redirectInEdgesWhen isDefEdge new_b b fg2
        in fg3
  in foldr processBlockNode fg bs_with_inout_defs

-- | Converts a match found from a function graph and a pattern graph with
-- duplicated nodes into a mapping for the original graphs.
fixMatch :: Graph
            -- ^ Original function graph.
         -> Graph
         -- ^ Original pattern graph.
         -> Match Node
         -> Match Node
fixMatch fg pg m =
  let updatePair p =
        let new_fn = head $ findNodesWithNodeID fg $ getNodeID $ fNode p
            new_pn = head $ findNodesWithNodeID pg $ getNodeID $ pNode p
        in Mapping { fNode = new_fn, pNode = new_pn }
  in toMatch $
     map updatePair $
     fromMatch m

-- | Removes duplicate matches that cover the same nodes in the function graph
-- as some other match.
removeDupMatches :: [Match Node] -> [Match Node]
removeDupMatches ms =
  let fns = map (sort . map fNode . fromMatch) ms
      ms_fns = zip ms fns
  in map (fst . head) $
     groupBy (\(_, f1) (_, f2) -> f1 == f2) $
     ms_fns

-- | Removes SIMD matches that will never be selected becuase not all operations
-- can be moved to the same block.
pruneNonselectableSimdMatches :: Graph -> Node -> [Match Node] -> [Match Node]
pruneNonselectableSimdMatches fg entry ms =
  let n_places = computeBlockPlacements fg entry
      op_bs_sets = map (\(op, bs) -> (op, S.fromList bs)) $
                   filter (isOperationNode . fst) $
                   n_places
      isSelectable m =
        let ops = filter isOperationNode $
                  nub $
                  map fNode $
                  fromMatch m
            place_cands = map (\n -> fromJust $ lookup n op_bs_sets) ops
            place_common = S.intersections place_cands
        in S.size place_common > 0
  in filter isSelectable ms


getInstructionFromPatternMatch :: TargetMachine -> PatternMatch -> Instruction
getInstructionFromPatternMatch t m =
  let iid = pmInstrID m
      i = findInstruction t iid
  in if isJust i
     then fromJust i
     else error $ "getInstructionFromPatternMatch: target machine has no " ++
                  "instruction with ID " ++ pShow iid
