--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.DispatcherTools
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains useful imports and functions when implementing a dispatcher.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.DispatcherTools
  ( module Language.InstSel.Drivers.Base
  , module Language.InstSel.Utils
  , module Language.InstSel.Utils.IO
  , loadFileContent
  , loadFunctionFileContent
  , loadMatchsetFileContent
  , loadTargetMachine
  , loadFromJson
  , loadFunctionFromJson
  , loadMatchsetInfoFromJson
  )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions
  ( Function )
import Language.InstSel.TargetMachines
  ( TargetMachine
  , toTargetMachineID
  )
import Language.InstSel.TargetMachines.PatternMatching
  ( MatchsetInfo )
import Language.InstSel.TargetMachines.Targets
  ( retrieveTargetMachine )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.IO
import Language.InstSel.Utils.JSON

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

loadFunctionFileContent :: Options -> IO String
loadFunctionFileContent =
  loadFileContent "No function file provided." . functionFile

loadMatchsetFileContent :: Options -> IO String
loadMatchsetFileContent =
  loadFileContent "No matchset file provided." . matchsetFile

-- | Returns the target machine specified on the command line. If no target is
-- specified, or if no such target exists, failure is reported.
loadTargetMachine :: Options -> IO TargetMachine
loadTargetMachine opts =
  do let tname = targetName opts
     when (isNothing tname) $
       reportError "No target provided."
     let target = retrieveTargetMachine $ toTargetMachineID $ fromJust tname
     when (isNothing target) $
       reportError $ "Unrecognized target: " ++ (show $ fromJust tname)
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

loadMatchsetInfoFromJson :: Options -> IO MatchsetInfo
loadMatchsetInfoFromJson opts =
  do content <- loadMatchsetFileContent opts
     loadFromJson content
