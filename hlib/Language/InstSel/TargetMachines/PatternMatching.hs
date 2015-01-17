--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachines.PatternMatching
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains functions for producing matchset information.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , PatternMatchset (..)
  , mkPatternMatchset
  )
where

import Language.InstSel.Graphs
  ( MatchID
  , Match
  )
import Language.InstSel.Graphs
  ( convertMatchN2ID )
import Language.InstSel.Graphs.PatternMatching.CP
import Language.InstSel.OpStructures
  ( OpStructure (..) )
import Language.InstSel.Functions
  ( Function (..) )
import Language.InstSel.TargetMachines
import Language.InstSel.Utils.JSON



--------------
-- Data types
--------------

-- | Contains the matchset information; that is, the information to determine
-- which target machine the matchset concerns, along the match data.
data PatternMatchset
  = PatternMatchset
      { msiTarget :: TargetMachineID
      , msiMatches :: [PatternMatch]
      }
  deriving (Show)

-- | Contains the information needed to identify which instruction and pattern a
-- given match originates from. Each match is also given a 'MatchID' that must
-- be unique (although not necessarily continuous) for every match within a list
-- of @PatternMatch@.
data PatternMatch
  = PatternMatch
      { mdInstrID :: InstructionID
      , mdPatternID :: PatternID
      , mdMatchID :: MatchID
      , mdMatch :: Match NodeID
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
  parseJSON _ = mzero

instance ToJSON PatternMatchset where
  toJSON m =
    object [ "target-machine-id" .= (msiTarget m)
           , "match-data"        .= (msiMatches m)
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
    object [ "instr-id"   .= (mdInstrID m)
           , "pattern-id" .= (mdPatternID m)
           , "match-id"   .= (mdMatchID m)
           , "match"      .= (mdMatch m)
           ]



-------------
-- Functions
-------------

-- | Produces the pattern matchset for a given function and target machine.
mkPatternMatchset :: Function -> TargetMachine -> PatternMatchset
mkPatternMatchset f tm =
  let mdata = concatMap (processInstr f) (tmInstructions tm)
      proper_mdata = map (\(m, mid) -> m { mdMatchID = mid }) $ zip mdata [0..]
  in PatternMatchset { msiTarget = tmID tm, msiMatches = proper_mdata }

processInstr :: Function -> Instruction -> [PatternMatch]
processInstr f i =
  let iid = instrID i
      patterns = instrPatterns i
  in concatMap (processInstrPattern f iid) patterns

processInstrPattern
  :: Function
  -> InstructionID
  -> InstrPattern
  -> [PatternMatch]
processInstrPattern f iid p =
  let fg = osGraph $ functionOS f
      pg = osGraph $ patOS p
      matches = map convertMatchN2ID $ findMatches fg pg
  in map
       ( \m -> PatternMatch
                 { mdInstrID = iid
                 , mdPatternID = patID p
                 , mdMatchID = 0
                 , mdMatch = m
                 }
       )
       matches
