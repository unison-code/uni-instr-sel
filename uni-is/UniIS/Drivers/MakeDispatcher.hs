{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeDispatcher
  ( run )
where

import UniIS.Drivers.DispatcherTools
import qualified UniIS.Drivers.MakeArrayIndexMaplists as MakeArrayIndexMaplists
import qualified UniIS.Drivers.MakeAssemblyCode as MakeAssemblyCode
import qualified UniIS.Drivers.MakeCPModel as MakeCPModel
import qualified UniIS.Drivers.MakePatternMatchset as MakePatternMatchset
import qualified UniIS.Drivers.MakeLowLevelModelDump as MakeLowLevelModelDump
import qualified UniIS.Drivers.MakeLowLevelSolutionDump
  as MakeLowLevelSolutionDump



-------------
-- Functions
-------------

-- | Function for executing this dispatcher.
run :: Options -> IO [Output]
run opts = dispatch (makeAction opts) opts

-- | Dispatches execution to the correct driver.
dispatch :: MakeAction -> Options -> IO [Output]
dispatch a opts
  | a == MakeNothing =
      reportErrorAndExit "No make action provided."
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
  | a `elem` [MakeLowLevelSolutionDump] =
      do function <- loadFunctionFromJson opts
         model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         ai_maps <- loadArrayIndexMaplistsFromJson opts
         sol_content <- loadSolutionFileContent opts
         sol <- loadFromJson sol_content
         MakeLowLevelSolutionDump.run a function model ai_maps sol
  | a `elem` [MakeHighLevelCPModel] =
      do function <- loadFunctionFromJson opts
         matchset <- loadPatternMatchsetFromJson opts
         MakeCPModel.run a function matchset
  | a `elem` [MakeAssemblyCode] =
      do model_content <- loadModelFileContent opts
         model <- loadFromJson model_content
         sol_content <- loadSolutionFileContent opts
         sol <- loadFromJson sol_content
         MakeAssemblyCode.run a model sol
  | otherwise =
      reportErrorAndExit "MakeDispatcher: unsupported action"
