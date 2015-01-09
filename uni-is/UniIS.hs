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
  ( toLower )

import Language.InstSel.Drivers
import qualified Language.InstSel.Drivers.LlvmIrProcessor as LlvmIrProcessor
import qualified Language.InstSel.Drivers.Modeler as Modeler
import qualified Language.InstSel.Drivers.PatternMatcher as PatternMatcher
import qualified Language.InstSel.Drivers.Plotter as Plotter

import System.Console.CmdArgs

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
      , plotFunctionGraph :: Bool
      , plotFunctionGraphCoverage :: Bool
      , plotFunctionGraphCoveragePerMatch :: Bool
      , matchFile :: Maybe String
      , inFile :: Maybe String
      , outFile :: Maybe String
      , targetName :: Maybe String
      }
  deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { command = def
        &= argPos 0
        &= typ "COMMAND"
    , inFile = def
        &= help "File containing the input."
        &= typFile
        &= explicit
        &= name "i"
        &= name "input"
        &= groupname "Common options"
    , outFile = Nothing
        &= help ( "File that will contain the output. If the output involves "
                  ++ "multiple files, each file will be suffixed with a "
                  ++ "unique ID."
                )
        &= typFile
        &= explicit
        &= name "o"
        &= name "output"
    , targetName = Nothing
        &= help "Name of the target machine."
        &= typ "TARGET"
        &= explicit
        &= name "t"
        &= name "target"
    , plotFunctionGraph = False
        &= help "Print function graph in DOT format."
        &= explicit
        &= name "plot-function-graph"
        &= groupname "PLOT ONLY options"
    , plotFunctionGraphCoverage = False
        &= help ( "Print function graph in DOT format, and mark the nodes that "
                  ++ "has a potential cover"
                )
        &= explicit
        &= name "plot-function-graph-coverage"
        &= groupname "PLOT ONLY options"
    , plotFunctionGraphCoveragePerMatch = False
        &= help ( "Same as --plot-function-graph-coverage, but shows the "
                  ++ "coverage for each individual match."
                )
        &= explicit
        &= name "plot-function-graph-coverage-per-match"
        &= groupname "PLOT ONLY options"
    , matchFile = Nothing
        &= help "File containing the matches."
        &= typFile
        &= explicit
        &= name "match-file"
        &= groupname "MODEL ONLY options"
    }
    &=
    program "uni-is"
    &=
    summary ( "Unison (instruction selection) tool\n"
              ++
              "Gabriel Hjort Blindell   ghb@kth.se"
            )
    &=
    details [ "Available commands:"
            , "   process-llvm-ir   Converts an LLVM IR file into the "
              ++ "graph-based IR format"
            , "   lower-llvm-ir     Rewriting an LLVM IR file into an expected "
              ++ "canonical form"
            , "   pattern-match     Performs pattern matching on the function "
              ++ "graph"
            , "   make-cp-model     Produces a CP model instance"
            , "   plot              Produces various plots of a given function"
            , "   check-function    Performs sanity checks on a given function"
            , "The commands may be written in lower or upper case."
            ]



-------------
-- Functions
-------------

readFileContent :: FilePath -> IO String
readFileContent file =
  do exists_file <- doesFileExist file
     when (not exists_file) $
       error $ "File " ++ show file ++ " does not exist."
     readFile file

-- | Returns the target machine specified on the command line. If no target is
-- specified, or if no such target exists, failure is reported.
getRequiredTarget :: Options -> IO TargetMachine
getRequiredTarget opts =
  do let tname = targetName opts
     when (isNothing tname) $
       error "No target provided."
     let target = getTargetMachine $ toTargetMachineID $ fromJust tname
     when (isNothing target) $
       error $ "Unrecognized target: " ++ (show $ fromJust tname)
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
               error $ "Unrecognized target: " ++ (show $ fromJust tname)
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

-- | Gets the requested plot action, based on the command line options. If no
-- action is requested, an error is reported. If more than one action is
-- requested, an arbitrary action is selected.
getRequiredPlotAction :: Options -> IO Plotter.PlotAction
getRequiredPlotAction opts =
  let actions = [ ( Plotter.PlotFunctionGraph
                  , plotFunctionGraph opts
                  )
                , ( Plotter.PlotFunctionGraphCoverage
                  , plotFunctionGraphCoverage opts
                  )
                , ( Plotter.PlotFunctionGraphCoveragePerMatch
                  , plotFunctionGraphCoveragePerMatch opts
                  )
                ]
      selected = filter snd actions
  in if length selected > 0
     then return $ fst $ head selected
     else error "No plot action provided."



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     output <-
       case (toLower $ command opts) of
         "process-llvm-ir" ->
           do when (isNothing $ inFile opts) $
                error "No LLVM IR file is provided as input."
              content <- readFileContent $ fromJust $ inFile opts
              LlvmIrProcessor.run content
         "pattern-match" ->
           do when (isNothing $ inFile opts) $
                error "No function JSON file is provided as input."
              content <- readFileContent $ fromJust $ inFile opts
              target <- getRequiredTarget opts
              PatternMatcher.run content target
         "make-cp-model" ->
           do when (isNothing $ inFile opts) $
                error "No function JSON file is provided as input."
              when (isNothing $ matchFile opts) $
                error "No matches JSON file is provided as input."
              fcontent <- readFileContent $ fromJust $ inFile opts
              mcontent <- readFileContent $ fromJust $ matchFile opts
              Modeler.run fcontent mcontent
         "plot" ->
           do when (isNothing $ inFile opts) $
                error "No function JSON file is provided as input."
              fcontent <- readFileContent $ fromJust $ inFile opts
              mcontent <- if isJust $ matchFile opts
                          then
                            do c <- readFileContent $ fromJust $ matchFile opts
                               return $ Just c
                          else return Nothing
              action <- getRequiredPlotAction opts
              Plotter.run fcontent mcontent action
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
