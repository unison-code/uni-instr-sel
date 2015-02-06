--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.PlotDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching plot commands.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.PlotDispatcher
  ( run )
where

import Language.InstSel.Drivers.DispatcherTools
import qualified Language.InstSel.Drivers.PlotCoverGraphs
  as PlotCoverGraphs
import qualified Language.InstSel.Drivers.PlotFunctionGraphs
  as PlotFunctionGraphs
import qualified Language.InstSel.Drivers.PlotPatternGraphs
  as PlotPatternGraphs



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (plotAction opts) opts

dispatch :: PlotAction -> Options -> IO [Output]
dispatch a opts
  | a == PlotNothing =
      reportError "No plot action provided."
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
      -- TODO: implement
      undefined
  | a `elem` [PlotCoverAllMatches, PlotCoverPerMatch] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         PlotCoverGraphs.run a function matchset
  | otherwise =
      reportError "PlotDispatcher: unsupported action"
