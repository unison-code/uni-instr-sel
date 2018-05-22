{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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

-- | Function for executing this dispatcher.
run :: Options -> IO [Output]
run opts = dispatch (plotAction opts) opts

-- | Dispatches execution to the correct driver.
dispatch :: PlotAction -> Options -> IO [Output]
dispatch a opts
  | a == PlotNothing =
      reportErrorAndExit "No plot action provided."
  | a `elem` [ PlotFunctionFullGraph
             , PlotFunctionControlFlowGraph
             , PlotFunctionSSAGraph
             ] =
      do function <- loadFunctionFromJson opts
         show_edge_nrs <- getShowEdgeNumbersPred opts
         PlotFunctionGraphs.run a show_edge_nrs function
  | a `elem` [ PlotPatternFullGraph
             , PlotPatternControlFlowGraph
             , PlotPatternSSAGraph
             ] =
      do tmid <- getSelectedTargetMachineID opts
         tm <- loadTargetMachine tmid
         iid <- getSelectedInstructionID opts
         i <- loadInstruction tm iid
         show_edge_nrs <- getShowEdgeNumbersPred opts
         PlotPatternGraphs.run a show_edge_nrs i
  | a `elem` [PlotCoverAllMatches, PlotCoverPerMatch] =
      do function <- loadFunctionFromJson opts
         model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         ai_maps <- loadArrayIndexMaplistsFromJson opts
         show_edge_nrs <- getShowEdgeNumbersPred opts
         hide_null_instrs <- getHideNullInstrsPred opts
         hide_kill_instrs <- getHideKillInstrsPred opts
         PlotCoverGraphs.run a
                             show_edge_nrs
                             hide_null_instrs
                             hide_kill_instrs
                             function
                             model
                             ai_maps
  | otherwise =
      reportErrorAndExit "PlotDispatcher: unsupported action"
