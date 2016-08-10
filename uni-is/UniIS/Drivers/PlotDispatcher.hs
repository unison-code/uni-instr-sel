{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.PlotDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.PlotCoverGraphs as PlotCoverGraphs
import qualified UniIS.Drivers.PlotFunctionGraphs as PlotFunctionGraphs
import qualified UniIS.Drivers.PlotPatternGraphs as PlotPatternGraphs



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (plotAction opts) opts

dispatch :: PlotAction -> Options -> IO [Output]
dispatch a opts
  | a == PlotNothing =
      reportErrorAndExit "No plot action provided."
  | a `elem` [ PlotFunctionFullGraph
             , PlotFunctionControlFlowGraph
             , PlotFunctionSSAGraph
             ] =
      do function <- loadFunctionFromJson opts
         PlotFunctionGraphs.run a function
  | a `elem` [ PlotPatternFullGraph
             , PlotPatternControlFlowGraph
             , PlotPatternSSAGraph
             ] =
      do tmid <- getSelectedTargetMachineID opts
         tm <- loadTargetMachine tmid
         iid <- getSelectedInstructionID opts
         pid <- getSelectedPatternID opts
         pattern <- loadInstrPattern tm iid pid
         PlotPatternGraphs.run a pattern
  | a `elem` [PlotCoverAllMatches, PlotCoverPerMatch] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         hide_inactive_instrs <- getHideInactiveInstrsPred opts
         PlotCoverGraphs.run a function matchset hide_inactive_instrs
  | otherwise =
      reportErrorAndExit "PlotDispatcher: unsupported action"
