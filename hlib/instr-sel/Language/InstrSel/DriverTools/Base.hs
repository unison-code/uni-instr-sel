{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.DriverTools.Base
  ( Log (..)
  , LogMessage (..)
  , Output (..)
  , concatLogs
  , emptyLog
  , toLog
  , emitToStdout
  , emitToFile
  , toOutput
  , toOutputWithID
  , toOutputWithExitCode
  , toOutputWithIDAndExitCode
  , mkOutputFromLog
  )
where

import Language.InstrSel.Utils.IO
  ( ExitCode (..)
  , errorExitCode
  , successExitCode
  )

import Data.List
  ( intercalate )
import Data.Maybe
  ( fromJust
  , isJust
  )
import System.FilePath
  ( splitExtension
  , takeDirectory
  , takeFileName
  )



--------------
-- Data types
--------------

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: Maybe String
        -- ^ A unique string that identifies this output.
      , oExitCode :: ExitCode
        -- ^ When the program exists after emitting the output, exit the this
        -- exit code. If multiple 'Output's are given, the exit code of the last
        -- 'Output' is used.
      , oData :: String
        -- ^ The produced output.
      }

-- | Contains an log, which is a list of 'LogMessage's.
newtype Log
  = Log { msgList :: [LogMessage] }

-- | A representation for producing log messages, which in turn will later be
-- turned into 'Output'.
data LogMessage
  = ErrorMessage String
  | WarningMessage String



-------------
-- Functions
-------------

-- | Creates an output that has no ID and no exit code. This is useful when
-- there is exactly one output produced.
toOutput
  :: String
     -- ^ The output string.
  -> Output
toOutput s = toOutput' Nothing successExitCode s

-- | Creates an output with an ID but no exit code.
toOutputWithID
  :: String
     -- ^ The ID.
  -> String
     -- ^ The output string.
  -> Output
toOutputWithID oid s = toOutput' (Just oid) successExitCode s

-- | Creates an output with an exit code but no ID.
toOutputWithExitCode
  :: ExitCode
     -- ^ The exit code.
  -> String
     -- ^ The output string.
  -> Output
toOutputWithExitCode code s = toOutput' Nothing code s

-- | Creates an output with an ID and exit code.
toOutputWithIDAndExitCode
  :: String
     -- ^ The ID.
  -> ExitCode
     -- ^ The exit code.
  -> String
     -- ^ The output string.
  -> Output
toOutputWithIDAndExitCode oid code s = toOutput' (Just oid) code s

toOutput' :: Maybe String -> ExitCode -> String -> Output
toOutput' oid code s = Output { oID = oid
                              , oExitCode = code
                              , oData = s
                              }

-- | Emits output to @STDOUT@.
emitToStdout :: Output -> IO ()
emitToStdout = putStrLn . oData

-- | Emits output to a file of a given name and the output ID suffixed to the
-- file name before the file extension of the path. If the path has no file
-- extension, then the ID is simply appended to the path.
emitToFile :: FilePath -> Output -> IO ()
emitToFile fp o =
  let dname = takeDirectory fp
      (fname, ext) = splitExtension $ takeFileName fp
      oid = oID o
      filename = dname ++
                 (if length dname > 0 then "/" else "") ++
                 fname ++
                 ( if isJust oid
                   then (if length fname > 0 then "." else "") ++ fromJust oid
                   else ""
                 ) ++
                 ext
  in writeFile filename (oData o)

-- | Creates an empty log.
emptyLog :: Log
emptyLog = Log []

-- | Converts a 'LogMessage' into a 'Log.
toLog :: LogMessage -> Log
toLog msg = Log [msg]

-- | Concatenates a list of 'Log's into a single 'Log'.
concatLogs :: [Log] -> Log
concatLogs logs = Log $ concat $ map msgList logs

isErrorMessage :: LogMessage -> Bool
isErrorMessage (ErrorMessage {}) = True
isErrorMessage _ = False

-- | Converts a 'LogMessage' to a 'String'
msg2Str :: LogMessage -> String
msg2Str (ErrorMessage str) = str
msg2Str (WarningMessage str) = str

-- | Makes an 'Output' from a given 'Log'. An empty log indicates no errors.
mkOutputFromLog :: Log -> [Output]
mkOutputFromLog (Log []) = []
mkOutputFromLog (Log msgs) =
  let code = if any isErrorMessage msgs
             then errorExitCode
             else successExitCode
      msgs_str = intercalate "\n\n" $
                 map msg2Str msgs
  in [toOutputWithExitCode code msgs_str]
