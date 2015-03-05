--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.DispatcherTools
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains useful imports and functions when implementing a dispatcher.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.DispatcherTools
  ( module Language.InstrSel.Drivers.Base
  , module Language.InstrSel.Utils
  , module Language.InstrSel.Utils.IO
  , getSelectedTargetMachineID
  , loadFileContent
  , loadArrayIndexMaplistsFileContent
  , loadFunctionFileContent
  , loadModelFileContent
  , loadPatternMatchsetFileContent
  , loadSolutionFileContent
  , loadTargetMachine
  , loadFromJson
  , loadFunctionFromJson
  , loadPatternMatchsetFromJson
  )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines
  ( TargetMachine
  , TargetMachineID
  , toTargetMachineID
  )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset )
import Language.InstrSel.TargetMachines.Targets
  ( retrieveTargetMachine )
import Language.InstrSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstrSel.Utils.IO
import Language.InstrSel.Utils.JSON

import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Directory
  ( doesFileExist )



-------------
-- Functions
-------------

readFileContent :: FilePath -> IO String
readFileContent file =
  do exists_file <- doesFileExist file
     when (not exists_file) $
       reportError $ "File " ++ show file ++ " does not exist."
     readFile file

-- | Loads the content of a file.
loadFileContent
  :: String
     -- ^ Error message when the file path is @Nothing@.
  -> Maybe FilePath
     -- ^ The file to load.
  -> IO String
     -- ^ The file content.
loadFileContent err file =
  do when (isNothing file) $
       reportError err
     readFileContent $ fromJust file

loadArrayIndexMaplistsFileContent :: Options -> IO String
loadArrayIndexMaplistsFileContent =
  loadFileContent "No array index maplists file provided."
  . arrayIndexMaplistsFile

loadFunctionFileContent :: Options -> IO String
loadFunctionFileContent =
  loadFileContent "No function file provided." . functionFile

loadModelFileContent :: Options -> IO String
loadModelFileContent =
  loadFileContent "No model file provided." . modelFile

loadPatternMatchsetFileContent :: Options -> IO String
loadPatternMatchsetFileContent =
  loadFileContent "No pattern matchset file provided." . patternMatchsetFile

loadSolutionFileContent :: Options -> IO String
loadSolutionFileContent =
  loadFileContent "No solution file provided." . solutionFile

-- | Returns the target machine ID specified on the command line. Reports error
-- if no target is specified.
getSelectedTargetMachineID :: Options -> IO TargetMachineID
getSelectedTargetMachineID opts =
  do let tid = targetName opts
     when (isNothing tid) $
       reportError "No target machine provided."
     return $ toTargetMachineID $ fromJust tid

-- | Returns the target machine with given ID. Reports error if no such target
-- machine exists.
loadTargetMachine :: TargetMachineID -> IO TargetMachine
loadTargetMachine tid =
  do let target = retrieveTargetMachine tid
     when (isNothing target) $
       reportError $ "Unrecognized target machine: " ++ (show tid)
     return $ fromJust target

-- | Parses a given JSON string and loads its content. Reports error if this
-- fails.
loadFromJson :: (FromJSON a) => String -> IO a
loadFromJson str =
  do let res = fromJson str
     when (isLeft res) $
       reportError $ fromLeft res
     return $ fromRight res

loadFunctionFromJson :: Options -> IO Function
loadFunctionFromJson opts =
  do content <- loadFunctionFileContent opts
     loadFromJson content

loadPatternMatchsetFromJson :: Options -> IO PatternMatchset
loadPatternMatchsetFromJson opts =
  do content <- loadPatternMatchsetFileContent opts
     loadFromJson content
