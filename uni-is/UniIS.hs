{-
Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-
The main driver for invoking various Unison commands related to instruction
selection.
-}

{-# LANGUAGE DeriveDataTypeable #-}

import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.Targets
  ( getTargetMachine )
import Language.InstSel.Utils
  ( splitOn
  , toLower
  )
import Language.InstSel.Utils.IO

import Language.InstSel.Drivers
import qualified Language.InstSel.Drivers.LlvmIrProcessor as LlvmIrProcessor
import qualified Language.InstSel.Drivers.Modeler as Modeler
import qualified Language.InstSel.Drivers.PatternMatcher as PatternMatcher
import qualified Language.InstSel.Drivers.Plotter as Plotter
import qualified Language.InstSel.Drivers.Transformer as Transformer

import System.Console.CmdArgs
import System.Console.CmdArgs.Text

import Control.Monad
  ( when )
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  )
import System.Directory
  ( doesFileExist )
import System.FilePath.Posix
  ( splitExtension )



--------------------------------------------
-- Options-related data types and functions
--------------------------------------------

data Options
  = Options
      { command :: String
      , inFile :: Maybe String
      , outFile :: Maybe String
      , targetName :: Maybe String
      , matchFile :: Maybe String
      , plotAction :: Plotter.PlotAction
      , transformAction :: Transformer.TransformAction
      }
  deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { command = def
        &= argPos 0
        &= typ "COMMAND"
    , inFile = def
        &= help "File containing the function."
        &= name "f"
        &= name "function-file"
        &= explicit
        &= typFile
    , outFile = Nothing
        &= help ( "File that will contain the output. If the output involves "
                  ++ "multiple files, each file will be suffixed with a "
                  ++ "unique ID."
                )
        &= name "o"
        &= name "output"
        &= explicit
        &= typFile
    , targetName = Nothing
        &= help "Name of the target machine."
        &= name "t"
        &= name "target-name"
        &= explicit
        &= typ "TARGET"
    , matchFile = Nothing
        &= help "File containing the matches."
        &= typFile
        &= explicit
        &= name "m"
        &= name "match-file"
    , plotAction =
        enum [ Plotter.DoNothing
                 &= auto
                 &= ignore
             , Plotter.PlotFunctionGraph
                 &= help "Print function graph in DOT format."
                 &= name "plot-fg"
                 &= explicit
             , Plotter.PlotFunctionGraphCoverage
                 &= help ( "Print function graph in DOT format, and mark the "
                           ++ "nodes that has a potential cover."
                         )
                 &= name "plot-fg-cov"
                 &= explicit
             , Plotter.PlotFunctionGraphCoveragePerMatch
                 &= help ( "Same as --plot-fg-cov, but shows "
                           ++ "the coverage for each individual match."
                         )
                 &= name "plot-fg-cov-per-match"
                 &= explicit
             ]
        &= groupname "Plot flags"
    , transformAction =
        enum [ Transformer.DoNothing
                 &= auto
                 &= ignore
             , Transformer.CopyExtendFunction
                 &= help "Extends the given function graph with copies."
                 &= name "copy-extend-fg"
                 &= explicit
             ]
        &= groupname "Transformation flags"
    }
    &= helpArg [ help "Displays this message."
               , name "h"
               , name "help"
               , explicit
               , groupname "Other flags"
               ]
    &= versionArg [ ignore ]
    &= program "uni-is"
    &= summary ( "Unison (instruction selection) tool\n"
                 ++
                 "Gabriel Hjort Blindell <ghb@kth.se>"
               )
    &= details
         ( splitOn
             "\n"
             ( showText
                 defaultWrap
                 [ Line "Available commands:"
                 , Cols [ "  lower-llvm-ir"
                        , " Rewrites an LLVM IR file into an expected "
                          ++ "canonical form."
                        ]
                 , Cols [ "  process-llvm-ir"
                        , " Converts an LLVM IR file into the graph-based IR "
                          ++ "format."
                        ]
                 , Cols [ "  transform"
                        , " Performs a transformation on the input."
                        ]
                 , Cols [ "  pattern-match"
                        , " Performs pattern matching on the given function "
                          ++ "graph."
                        ]
                 , Cols [ "  make-cp-model"
                        , " Produces a CP model instance."
                        ]
                 , Cols [ "  plot"
                        , " Produces various plots for the input."
                        ]
                 , Cols [ "  check-function"
                        , " Performs sanity checks on the input."
                        ]
                 , Line "The commands may be written in lower or upper case."
                 ]
             )
         )



-------------
-- Functions
-------------

readFileContent :: FilePath -> IO String
readFileContent file =
  do exists_file <- doesFileExist file
     when (not exists_file) $
       reportError $ "File " ++ show file ++ " does not exist."
     readFile file

getRequiredFunctionFile :: Options -> IO FilePath
getRequiredFunctionFile opts =
  do let file = inFile opts
     when (isNothing file) $
       reportError "No function file provided."
     return $ fromJust file

getRequiredMatchFile :: Options -> IO FilePath
getRequiredMatchFile opts =
  do let file = matchFile opts
     when (isNothing file) $
       reportError "No match file provided."
     return $ fromJust file

getOptionalMatchFile :: Options -> IO (Maybe FilePath)
getOptionalMatchFile opts = return $ matchFile opts

-- | Returns the target machine specified on the command line. If no target is
-- specified, or if no such target exists, failure is reported.
getRequiredTarget :: Options -> IO TargetMachine
getRequiredTarget opts =
  do let tname = targetName opts
     when (isNothing tname) $
       reportError "No target provided."
     let target = getTargetMachine $ toTargetMachineID $ fromJust tname
     when (isNothing target) $
       reportError $ "Unrecognized target: " ++ (show $ fromJust tname)
     return $ fromJust target

-- | Returns the target machine specified on the command line. If no target is
-- specified, 'Nothing' is returned. If a target is specified, but no such
-- target exists, failure is reported.
getOptionalTarget :: Options -> IO (Maybe TargetMachine)
getOptionalTarget opts =
  do let tname = targetName opts
     if isJust tname
     then do let target = getTargetMachine $ toTargetMachineID $ fromJust tname
             when (isNothing target) $
               reportError $ "Unrecognized target: " ++ (show $ fromJust tname)
             return target
     else return Nothing

-- | If an output file is given as part of the options, then the returned
-- function will emit all data to the output file with the output ID suffixed to
-- the output file name (this may mean that several output files are
-- produced). Otherwise the data will be emitted to 'STDOUT'.
getEmitFunction :: Options -> IO (Output -> IO ())
getEmitFunction opts =
  do let file = outFile opts
     if isNothing file
     then return emitToStdout
     else return $ emitToFile (fromJust file)

-- | A function that emits output to 'STDOUT'.
emitToStdout :: Output -> IO ()
emitToStdout = putStrLn . oData

-- | A function that emits output to a file of a given name and the output ID
-- suffixed.
emitToFile :: FilePath -> Output -> IO ()
emitToFile fp o =
  let (fname, ext) = splitExtension fp
      filename = fname ++ oID o ++ ext
  in writeFile filename (oData o)



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     output <-
       case (toLower $ command opts) of
         "process-llvm-ir" ->
           do file <- getRequiredFunctionFile opts
              content <- readFileContent file
              LlvmIrProcessor.run content
         "pattern-match" ->
           do file <- getRequiredFunctionFile opts
              content <- readFileContent file
              target <- getRequiredTarget opts
              PatternMatcher.run content target
         "make-cp-model" ->
           do f_file <- getRequiredFunctionFile opts
              m_file <- getRequiredMatchFile opts
              f_content <- readFileContent f_file
              m_content <- readFileContent m_file
              Modeler.run f_content m_content
         "plot" ->
           do f_file <- getRequiredFunctionFile opts
              m_file <- getOptionalMatchFile opts
              f_content <- readFileContent f_file
              m_content <- if isJust m_file
                           then
                             do content <- readFileContent $ fromJust m_file
                                return $ Just content
                           else return Nothing
              Plotter.run f_content m_content (plotAction opts)
         "transform" ->
           do file <- getRequiredFunctionFile opts
              content <- readFileContent file
              Transformer.run content (transformAction opts)
         "lower-llvm-ir" ->
           undefined
           -- TODO: implement
         "check-function" ->
           undefined
           -- TODO: implement
         cmd ->
           error $ "Unrecognized command: " ++ show cmd
     emit <- getEmitFunction opts
     mapM_ emit output
