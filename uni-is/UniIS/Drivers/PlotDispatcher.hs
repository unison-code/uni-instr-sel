--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.PlotDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching plot commands.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.PlotDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.PlotCoverGraphs
  as PlotCoverGraphs
import qualified UniIS.Drivers.PlotFunctionGraphs
  as PlotFunctionGraphs
import qualified UniIS.Drivers.PlotPatternGraphs
  as PlotPatternGraphs



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
         PlotCoverGraphs.run a function matchset
  | otherwise =
      reportErrorAndExit "PlotDispatcher: unsupported action"
