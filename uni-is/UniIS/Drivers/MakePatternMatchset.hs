--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakePatternMatchset
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function and target machine as input, and produces the corresponding
-- pattern matchset by performing pattern matching of all instruction patterns
-- in the target machine on the function graph.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.MakePatternMatchset
  ( run )
where

import UniIS.Drivers
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines
  ( TargetMachine )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..), mkPatternMatchset )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import System.Clock
  ( TimeSpec, getTime, Clock(..), sec, nsec )

import Control.DeepSeq
  ( deepseq )


-------------
-- Functions
-------------

run :: MakeAction -> Function -> TargetMachine -> IO [Output]

run MakePatternMatchset function target =
  do start <- getTime Realtime
     let matches = mkPatternMatchset function target
     -- In order to use 'deepseq', all data types used within @matches@ must
     -- be an instance of the 'Control.DeepSeq.NFData' type class
     end <- matches `deepseq` getTime Realtime
     let time = seconds start end
         matches' = matches {pmTime = Just time}
     return [toOutputWithoutID $ toJson matches']

run _ _ _ = reportErrorAndExit "MakePatternMatchset: unsupported action"

seconds ::  TimeSpec -> TimeSpec -> Double
seconds start end =
    let secs  = sec end - sec start
        nsecs = nsec end - nsec start
    in fromIntegral secs + ((fromIntegral nsecs) / 1000000000)
