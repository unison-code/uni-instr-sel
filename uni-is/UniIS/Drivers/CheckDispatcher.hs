--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.CheckDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching check commands.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.CheckDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.CheckFunctionGraph as CheckFunctionGraph

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
         CheckFunctionGraph.run a function matchset Nothing
  | a `elem` [ CheckFunctionGraphLocationOverlap ] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         tm <- loadTargetMachine $ pmTarget matchset
         CheckFunctionGraph.run a function matchset (Just tm)
  | otherwise =
      reportErrorAndExit "CheckDispatcher: unsupported action"
