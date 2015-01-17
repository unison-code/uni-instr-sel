--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.TransformDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching transformation commands.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.TransformDispatcher
  ( run )
where

import Language.InstSel.Drivers.DispatcherTools
import qualified Language.InstSel.Drivers.TransformFunctionGraph
  as TransformFunctionGraph
import qualified Language.InstSel.Drivers.TransformCPSolution
  as TransformCPSolution



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (transformAction opts) opts

dispatch :: TransformAction -> Options -> IO [Output]
dispatch a opts
  | a == TransformNothing =
      reportError "No transform action provided."
  | a `elem` [CopyExtendFunctionGraph, BranchExtendFunctionGraph] =
      do content <- loadFunctionFileContent opts
         function <- loadFromJson content
         TransformFunctionGraph.run a function
  | a `elem` [RaiseLowLevelCPSolution] =
      do sol_content <- loadSolutionFileContent opts
         ai_content <- loadArrayIndexMaplistsFileContent opts
         ai_maps <- loadFromJson ai_content
         TransformCPSolution.run a sol_content ai_maps
  | otherwise =
      reportError "TransformDispatcher: unsupported action"
