--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakeDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching make commands.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.MakeDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.MakeArrayIndexMaplists as MakeArrayIndexMaplists
import qualified UniIS.Drivers.MakeAssemblyCode as MakeAssemblyCode
import qualified UniIS.Drivers.MakeCPModelNoOp as MakeCPModelNoOp
import qualified UniIS.Drivers.MakeCPModelWOp as MakeCPModelWOp
import qualified UniIS.Drivers.MakeFunctionFromLLVM as MakeFunctionFromLLVM
import qualified UniIS.Drivers.MakePatternMatchset as MakePatternMatchset
import qualified UniIS.Drivers.MakeLowLevelModelDump as MakeLowLevelModelDump



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (makeAction opts) opts

dispatch :: MakeAction -> Options -> IO [Output]
dispatch a opts
  | a == MakeNothing =
      reportErrorAndExit "No make action provided."
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
         model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         MakeArrayIndexMaplists.run a function model
  | a `elem` [MakeLowLevelModelDump] =
      do function <- loadFunctionFromJson opts
         model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         ai_maps <- loadArrayIndexMaplistsFromJson opts
         MakeLowLevelModelDump.run a function model ai_maps
  | a `elem` [MakeHighLevelCPModelNoOp] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         MakeCPModelNoOp.run a function matchset
  | a `elem` [MakeHighLevelCPModelWOp] =
      do model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         matchset <- loadPatternMatchsetFromJson opts
         MakeCPModelWOp.run a model matchset
  | a `elem` [MakeAssemblyCode] =
      do model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         sol_content <- loadSolutionFileContent opts
         sol <- loadFromJson sol_content
         MakeAssemblyCode.run a model sol
  | otherwise =
      reportErrorAndExit "MakeDispatcher: unsupported action"
