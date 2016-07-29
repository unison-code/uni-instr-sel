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

import Language.InstrSel.Graphs
  ( Graph
  , MatchID
  , Match (..)
  , Node
  , convertMatchN2ID
  , extractSSA
  , subGraph
  , getAllNodes
  , getFNsInMatch
  , delNode
  , isDatumNode
  , hasAnyPredecessors
  )
import Language.InstrSel.Graphs.Graphalyze
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.TargetMachines
import Language.InstrSel.Utils
  ( (===) )
import Language.InstrSel.Utils.JSON

import Control.DeepSeq
  ( NFData
  , rnf
  )

import Data.List
  ( nubBy )



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
  concatMap (processInstrPattern f i) (instrPatterns i)

processInstrPattern
  :: Function
  -> Instruction
  -> InstrPattern
  -> [PatternMatch]
processInstrPattern function instr pattern =
  let fg = osGraph $ functionOS function
      pg = osGraph $ patOS pattern
      matches = findMatches fg pg
      okay_matches = filter (not . hasCyclicDataDependency fg) matches
      non_sym_matches = if isInstructionSimd instr
                        then pruneSymmetricSimdMatches okay_matches
                        else okay_matches
  in map
       ( \m -> PatternMatch { pmInstrID = instrID instr
                            , pmPatternID = patID pattern
                            , pmMatchID = 0 -- Unique value is assigned later
                            , pmMatch = m
                            }
       )
       (map convertMatchN2ID non_sym_matches)

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
  let f_ns = getFNsInMatch m
      ssa_fg = extractSSA fg
      -- TODO: should PHI operations not covered by m be removed from ssa_fg?
      ssa_fg' = subGraph ssa_fg f_ns
      -- Data nodes which act as input to the pattern should not be included
      ssa_fg'' = foldr delNode ssa_fg'
                 $ filter ( \n -> isDatumNode n
                                  && not (hasAnyPredecessors ssa_fg' n)
                          )
                 $ getAllNodes ssa_fg'
      mcs = componentsOf ssa_fg''
      cdd = or [ isReachableComponent ssa_fg c1 c2
               | c1 <- mcs, c2 <- mcs, getAllNodes c1 /= getAllNodes c2
               ]
  in cdd

-- | Removes matches that cover the same nodes in the function graph. It is
-- assumed that the matches originate from a SIMD instruction.
pruneSymmetricSimdMatches :: [Match Node] -> [Match Node]
pruneSymmetricSimdMatches ms =
  let check m1 m2 =
        let ns1 = getFNsInMatch m1
            ns2 = getFNsInMatch m2
        in ns1 === ns2
  in nubBy check ms
