{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
  ( combinations )
import Language.InstrSel.Utils.JSON

import Data.Maybe
  ( fromJust
  , isJust
  )

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
      , pmPatternID :: PatternID
      , pmMatchID :: MatchID
      , pmMatch :: Match NodeID
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
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "match"
  parseJSON _ = mzero

instance ToJSON PatternMatch where
  toJSON m =
    object [ "instr-id"   .= (pmInstrID m)
           , "pattern-id" .= (pmPatternID m)
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
  rnf (PatternMatch a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d



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
  let matches = concatMap (processInstr function) (tmInstructions target)
      matches_with_IDs =
        map (\(m, mid) -> m { pmMatchID = mid }) $ zip matches [0..]
  in PatternMatchset { pmTarget = tmID target
                     , pmMatches = matches_with_IDs
                     , pmTime = Nothing
                     }

processInstr :: Function -> Instruction -> [PatternMatch]
processInstr f i =
  let fg = osGraph $ functionOS f
      dup_fg = duplicateNodes fg
  in concatMap (processInstrPattern fg dup_fg i) (instrPatterns i)

processInstrPattern
  :: Graph
     -- ^ The original function graph.
  -> Graph
     -- ^ The duplicated function graph.
  -> Instruction
  -> InstrPattern
  -> [PatternMatch]
processInstrPattern fg dup_fg instr pat =
  let pg = osGraph $ patOS pat
      dup_pg = duplicateNodes pg
      matches = map (fixMatch fg pg) $
                if not (isInstructionSimd instr)
                then findMatches dup_fg dup_pg
                else let pg_cs = componentsOf dup_pg
                         sub_pg = head pg_cs
                         sub_matches = findMatches dup_fg sub_pg
                     in mkSimdMatches sub_pg sub_matches (tail pg_cs)
      okay_matches = filter (not . hasCyclicDataDependency fg) matches
  in map ( \m -> PatternMatch { pmInstrID = instrID instr
                              , pmPatternID = patID pat
                              , pmMatchID = 0 -- Unique value is assigned later
                              , pmMatch = m
                              }
         ) $
     map convertMatchN2ID $
     okay_matches

-- | Constructs a list of matches for the entire SIMD pattern graph based on its
-- components and list of matches for a single component.
mkSimdMatches
  :: Graph
     -- ^ A component of the SIMD pattern graph.
  -> [Match Node]
     -- ^ List of matches found for the component above in the function graph.
  -> [Graph]
     -- ^ List of components remaining in the SIMD pattern graph.
  -> [Match Node]
     -- ^ List of matches for the entire SIMD pattern graph.
mkSimdMatches sub_pg sub_matches pg_remaining_cs =
  let reassignMatch (pg_c_m, sub_m) =
        let reassign m =
              let pn = pNode m
                  fn = fNode m
                  new_pn = findPNInMatch pg_c_m pn
              in if isJust new_pn
                 then Mapping { fNode = fn, pNode = fromJust new_pn }
                 else error $ "mkSimdMatches: could not find a mapping for " ++
                              "function node with ID " ++ (pShow $ getNodeID pn)
        in toMatch $
           map reassign $
           fromMatch sub_m
      sub_pg_cs_matches = map ( \ms -> if length ms == 1
                                       then head ms
                                       else error $ "mkSimdMatches: " ++
                                                    "unexpected number of " ++
                                                    "matches found"
                              ) $
                          map (findMatches sub_pg) $
                          pg_remaining_cs
      m_combs = combinations (1 + (length pg_remaining_cs)) sub_matches
      simd_ms = map ( \(m:ms) ->
                      let fixed_ms = map reassignMatch $
                                     zip sub_pg_cs_matches ms
                      in toMatch $ concatMap fromMatch (m:fixed_ms)
                    )
                    m_combs
  in simd_ms

-- | Checks if a given match will result in a cyclic data dependency in the
-- function graph. The check works as follows: first the subgraph of the
-- function covered by the match is extracted. From this subgraph, each
-- component is extracted, and then it is checked whether a component is
-- reachable from any other component. If so, then the match has a cyclic
-- dependency. However, components that are only reachable via an input node to
-- the pattern is not considered a dependency. Due to this, such data nodes are
-- removed prior to extracting the components.
hasCyclicDataDependency :: Graph -> Match Node -> Bool
hasCyclicDataDependency fg m =
  let f_ns = map fNode (fromMatch m)
      ssa_fg = extractSSA fg
      -- TODO: should PHI operations not covered by m be removed from ssa_fg?
      ssa_fg' = subGraph ssa_fg f_ns
      -- Data nodes which act as input to the pattern must not be included
      ssa_fg'' = foldr delNode ssa_fg' $
                 filter ( \n -> isDatumNode n &&
                                not (hasAnyPredecessors ssa_fg' n)
                        ) $
                 getAllNodes ssa_fg'
      mcs = componentsOf ssa_fg''
      cdd = or [ isReachableComponent ssa_fg c1 c2
               | c1 <- mcs, c2 <- mcs, getAllNodes c1 /= getAllNodes c2
               ]
  in cdd

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
