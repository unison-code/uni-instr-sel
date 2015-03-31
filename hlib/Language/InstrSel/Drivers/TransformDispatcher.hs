--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.TransformDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching transformation commands.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.TransformDispatcher
  ( run )
where

import Language.InstrSel.Drivers.DispatcherTools
import qualified Language.InstrSel.Drivers.TransformFunctionGraph
  as TransformFunctionGraph
import qualified Language.InstrSel.Drivers.TransformCPModel
  as TransformCPModel
import qualified Language.InstrSel.Drivers.TransformCPSolution
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
  | a `elem` [ CopyExtendFunctionGraph
             , BranchExtendFunctionGraph
             , CombineConstantsInFunctionGraph
             ] =
      do content <- loadFunctionFileContent opts
         function <- loadFromJson content
         TransformFunctionGraph.run a function
  | a `elem` [LowerHighLevelCPModel] =
      do m_content <- loadModelFileContent opts
         ai_content <- loadArrayIndexMaplistsFileContent opts
         ai_maps <- loadFromJson ai_content
         TransformCPModel.run a m_content ai_maps
  | a `elem` [RaiseLowLevelCPSolution] =
      do sol_content <- loadSolutionFileContent opts
         ai_content <- loadArrayIndexMaplistsFileContent opts
         ai_maps <- loadFromJson ai_content
         TransformCPSolution.run a sol_content ai_maps
  | otherwise =
      reportError "TransformDispatcher: unsupported action"
