{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.CheckDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.CheckFunctionGraph as CheckFunctionGraph
import qualified UniIS.Drivers.CheckIntegrity as CheckIntegrity

import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (pmTarget) )



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (checkAction opts) opts

dispatch :: CheckAction -> Options -> IO [Output]
dispatch a opts
  | a == CheckNothing =
      reportErrorAndExit "No check action provided."
  | a `elem` [ CheckFunctionGraphCoverage ] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         hide_null_instrs <- getHideNullInstrsPred opts
         hide_inactive_instrs <- getHideInactiveInstrsPred opts
         CheckFunctionGraph.run a
                                hide_null_instrs
                                hide_inactive_instrs
                                function
                                matchset
                                Nothing
  | a `elem` [ CheckFunctionGraphLocationOverlap ] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         tm <- loadTargetMachine $ pmTarget matchset
         CheckFunctionGraph.run a
                                False
                                False
                                function
                                matchset
                                (Just tm)
  | a `elem` [ CheckFunctionIntegrity ] =
      do function <- loadFunctionFromJson opts
         CheckIntegrity.run a (Left function)
  | a `elem` [ CheckPatternIntegrity ] =
      do tmid <- getSelectedTargetMachineID opts
         tm <- loadTargetMachine tmid
         iid <- getSelectedInstructionID opts
         instr <- loadInstruction tm iid
         CheckIntegrity.run a (Right instr)
  | otherwise =
      reportErrorAndExit "CheckDispatcher: unsupported action"
