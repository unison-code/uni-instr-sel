{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.DispatcherTools
  ( module UniIS.Drivers.Base
  , module Language.InstrSel.Utils
  , module Language.InstrSel.Utils.IO
  , getSelectedTargetMachineID
  , getSelectedInstructionID
  , getSelectedPatternID
  , loadFileContent
  , loadArrayIndexMaplistsFileContent
  , loadFunctionFileContent
  , loadModelFileContent
  , loadPatternMatchsetFileContent
  , loadSolutionFileContent
  , loadTargetMachine
  , loadInstrPattern
  , loadFromJson
  , loadFunctionFromJson
  , loadPatternMatchsetFromJson
  , loadArrayIndexMaplistsFromJson
  )
where

import UniIS.Drivers.Base
import UniIS.Targets

import Language.InstrSel.ConstraintModels
  ( ArrayIndexMaplists )
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines
  ( TargetMachine (..)
  , TargetMachineID
  , toTargetMachineID
  , Instruction (..)
  , InstructionID
  , toInstructionID
  , PatternID
  , toPatternID
  , InstrPattern
  , findInstruction
  , findInstrPattern
  )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset )
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



-------------
-- Functions
-------------

-- | Loads the content of a file.
loadFileContent
  :: String
     -- ^ Error message when the file path is 'Nothing'.
  -> Maybe FilePath
     -- ^ The file to load.
  -> IO String
     -- ^ The file content.
loadFileContent err file =
  do when (isNothing file) $
       reportErrorAndExit err
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
  do let tmid = targetName opts
     when (isNothing tmid) $
       reportErrorAndExit "No target machine provided."
     return $ toTargetMachineID $ fromJust tmid

-- | Returns the instruction ID specified on the command line. Reports error
-- if no instruction is specified.
getSelectedInstructionID :: Options -> IO InstructionID
getSelectedInstructionID opts =
  do let iid = instructionID opts
     when (isNothing iid) $
       reportErrorAndExit "No instruction ID provided."
     return $ toInstructionID $ fromJust iid

-- | Returns the pattern ID specified on the command line. Reports error
-- if no pattern is specified.
getSelectedPatternID :: Options -> IO PatternID
getSelectedPatternID opts =
  do let pid = patternID opts
     when (isNothing pid) $
       reportErrorAndExit "No pattern ID provided."
     return $ toPatternID $ fromJust pid

-- | Returns the target machine with given ID. Reports error if no such target
-- machine exists.
loadTargetMachine :: TargetMachineID -> IO TargetMachine
loadTargetMachine tid =
  do let target = retrieveTargetMachine tid
     when (isNothing target) $
       reportErrorAndExit $ "Unrecognized target machine: " ++ (pShow tid)
     return $ fromJust target

-- | Returns the instruction pattern with given ID. Reports error if no such
-- instruction or pattern exists.
loadInstrPattern
  :: TargetMachine
  -> InstructionID
  -> PatternID
  -> IO InstrPattern
loadInstrPattern tm iid pid =
  do let instr = findInstruction (tmInstructions tm) iid
     when (isNothing instr) $
       reportErrorAndExit $ "No instruction with ID '" ++ (pShow iid)
                            ++ "' in target machine '" ++ (pShow $ tmID tm)
                            ++ "'"
     let pattern = findInstrPattern (instrPatterns $ fromJust instr) pid
     when (isNothing pattern) $
       reportErrorAndExit $ "No pattern with ID '" ++ (pShow pid)
                            ++ "' in instruction '" ++ (pShow iid) ++ "'"
                            ++ "' in target machine '" ++ (pShow $ tmID tm)
                            ++ "'"
     return $ fromJust pattern

-- | Parses a given JSON string and loads its content. Reports error if this
-- fails.
loadFromJson :: (FromJSON a) => String -> IO a
loadFromJson str =
  do let res = fromJson str
     when (isLeft res) $
       reportErrorAndExit $ fromLeft res
     return $ fromRight res

loadFunctionFromJson :: Options -> IO Function
loadFunctionFromJson opts =
  do content <- loadFunctionFileContent opts
     loadFromJson content

loadPatternMatchsetFromJson :: Options -> IO PatternMatchset
loadPatternMatchsetFromJson opts =
  do content <- loadPatternMatchsetFileContent opts
     loadFromJson content

loadArrayIndexMaplistsFromJson :: Options -> IO ArrayIndexMaplists
loadArrayIndexMaplistsFromJson opts =
  do content <- loadArrayIndexMaplistsFileContent opts
     loadFromJson content
