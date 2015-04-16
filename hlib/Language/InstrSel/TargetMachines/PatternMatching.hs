--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstrSel.TargetMachines.PatternMatching
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2015
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

module Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , PatternMatchset (..)
  , mkPatternMatchset
  )
where

import Language.InstrSel.Graphs
  ( MatchID
  , Match
  )
import Language.InstrSel.Graphs
  ( convertMatchN2ID )
import Language.InstrSel.Graphs.PatternMatching.CP
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.TargetMachines
import Language.InstrSel.Utils.JSON
import Control.DeepSeq
  ( NFData, rnf )


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
      }
  deriving (Show)

instance NFData PatternMatchset where
    rnf (PatternMatchset a b c ) = rnf a `seq` rnf b `seq` rnf c

-- | Contains the information needed to identify which instruction and pattern a
-- given match originates from. Each match is also given a 'MatchID' that must
-- be unique (although not necessarily continuous) for every match within a list
-- of @PatternMatch@.
data PatternMatch
  = PatternMatch
      { pmInstrID :: InstructionID
      , pmPatternID :: PatternID
      , pmMatchID :: MatchID
      , pmMatch :: Match NodeID
      }
  deriving (Show)

instance NFData PatternMatch where
    rnf (PatternMatch a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

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



-------------
-- Functions
-------------

-- | Produces the pattern matchset for a given function and target machine.
mkPatternMatchset :: Function -> TargetMachine -> PatternMatchset
mkPatternMatchset function target =
  let matches = concatMap (processInstr function) (tmInstructions target)
      proper_matches =
        map (\(m, mid) -> m { pmMatchID = mid }) $ zip matches [0..]
  in PatternMatchset { pmTarget = tmID target, pmMatches = proper_matches, pmTime = Nothing }

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
processInstrPattern function iid pattern =
  let fg = osGraph $ functionOS function
      pg = osGraph $ patOS pattern
      matches = map convertMatchN2ID $ findMatches fg pg
  in map
       ( \m -> PatternMatch { pmInstrID = iid
                            , pmPatternID = patID pattern
                            , pmMatchID = 0
                            , pmMatch = m
                            }
       )
       matches
