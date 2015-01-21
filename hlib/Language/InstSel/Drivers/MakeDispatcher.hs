--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching make commands.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.MakeDispatcher
  ( run )
where

import Language.InstSel.Drivers.DispatcherTools
import qualified Language.InstSel.Drivers.MakeArrayIndexMaplists
  as MakeArrayIndexMaplists
import qualified Language.InstSel.Drivers.MakeAssemblyCode
  as MakeAssemblyCode
import qualified Language.InstSel.Drivers.MakeCPModel
  as MakeCPModel
import qualified Language.InstSel.Drivers.MakeFunctionFromLLVM
  as MakeFunctionFromLLVM
import qualified Language.InstSel.Drivers.MakePatternMatchset
  as MakePatternMatchset



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (makeAction opts) opts

dispatch :: MakeAction -> Options -> IO [Output]
dispatch a opts
  | a == MakeNothing =
      reportError "No make action provided."
  | a `elem` [MakeFunctionGraphFromLLVM] =
      do content <- loadFunctionFileContent opts
         MakeFunctionFromLLVM.run a content
  | a `elem` [MakePatternMatchset] =
      do function <- loadFunctionFromJson opts
         tid <- getSelectedTargetMachineID opts
         target <- loadTargetMachine tid
         MakePatternMatchset.run a function target
  | a `elem` [MakeArrayIndexMaplists] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         MakeArrayIndexMaplists.run a function matchset
  | a `elem` [MakeHighLevelCPModel] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         MakeCPModel.run a function matchset
  | a `elem` [MakeAssemblyCode] =
      do model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         sol_content <- loadSolutionFileContent opts
         sol <- loadFromJson model_content
         MakeAssemblyCode.run a model sol
  | otherwise =
      reportError "MakeDispatcher: unsupported action"