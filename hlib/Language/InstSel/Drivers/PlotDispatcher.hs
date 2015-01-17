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
import qualified Language.InstSel.Drivers.PlotCoverGraph as PCov
import qualified Language.InstSel.Drivers.PlotFunctionGraph as PFun
import qualified Language.InstSel.Drivers.PlotPatternGraph as PPat



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (plotAction opts) opts

dispatch :: PlotAction -> Options -> IO [Output]
dispatch a opts
  | a == PlotNothing =
      reportError "No plot action provided."
  | a `elem` [PlotFunctionGraph, PlotFunctionCFG, PlotFunctionSSA] =
      do function <- loadFunctionFromJson opts
         PFun.run a function
  | a `elem` [PlotPatternGraph, PlotPatternCFG, PlotPatternSSA] =
      -- TODO: implement
      undefined
  | a `elem` [PlotCoverAllMatches, PlotCoverPerMatch] =
      do function <- loadFunctionFromJson opts
         matchset <- loadMatchsetInfoFromJson opts
         PCov.run a function matchset
  | otherwise =
      reportError "PlotDispatcher: unsupported action"
