{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>
  Roberto Castaneda Lozano <rcas@sics.se>

-}

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
     return [toOutput $ toJson matches']

run _ _ _ = reportErrorAndExit "MakePatternMatchset: unsupported action"

seconds ::  TimeSpec -> TimeSpec -> Double
seconds start end =
    let secs  = sec end - sec start
        nsecs = nsec end - nsec start
    in fromIntegral secs + ((fromIntegral nsecs) / 1000000000)
