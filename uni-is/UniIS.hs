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

import qualified Language.InstSel.Drivers.Modeler as Modeler
import qualified Language.InstSel.Drivers.Plotter as Plotter
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.TargetMachines.IDs
import Language.InstSel.TargetMachines.Targets
  ( getTargetMachine )
import Language.InstSel.Utils
  ( toLower )
import Control.Monad
  ( when )
import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Console.CmdArgs
import System.Directory
  ( doesFileExist )



---------------------------------
-- Help functions and data types
---------------------------------

data Options
    = Options
        { command :: String
        , plotFunctionGraph :: Bool
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
        &= help "File that contains the input."
        &= typFile
        &= explicit
        &= name "i"
        &= name "input"
        &= groupname "Common options"
    , outFile = Nothing
        &= help "File for writing the result."
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
        &= help "Print function graph (DOT format)."
        &= explicit
        &= name "plot-function-graph"
        &= groupname "PLOT ONLY options"
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
            , "   LOWER - for rewriting the input into canonical form"
            , "   MODEL - for producing a CP model instance"
            , "   PLOT  - for producing various plots of the input"
            , "   CHECK - for sanity checks of the input"
            , "The commands may be written in lower or upper case."
            ]

readFileContent :: FilePath -> IO String
readFileContent file =
  do exists_file <- doesFileExist file
     when (not exists_file) $
       error $ "File " ++ show file ++ " does not exist."
     readFile file

getTarget :: Options -> IO TargetMachine
getTarget opts =
  do let tname = targetName opts
     when (isNothing tname) $
       error "No target provided."
     let target = getTargetMachine $ toTargetMachineID $ fromJust tname
     when (isNothing target) $
       error $ "Unrecognized target: " ++ (show $ fromJust tname)
     return $ fromJust target

-- | If an output file is given as part of the options, then the returned
-- function will write all data to the output file. Otherwise the data will be
-- written to 'STDOUT'.
getEmitFunction :: Options -> IO (String -> IO ())
getEmitFunction opts =
  do let file = outFile opts
     if isNothing file
     then return putStrLn
     else return $ writeFile (fromJust file)



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     case (toLower $ command opts) of
       "model" ->
         do when (isNothing $ inFile opts) $
              error "No LLVM IR file is provided as input."
            content <- readFileContent $ fromJust $ inFile opts
            target <- getTarget opts
            emitter <- getEmitFunction opts
            Modeler.run content target emitter
       "plot" ->
         do when (isNothing $ inFile opts) $
              error "No input file."
            content <- readFileContent $ fromJust $ inFile opts
            target <- getTarget opts
            emitter <- getEmitFunction opts
            Plotter.run
              content
              target
              [ plotFunctionGraph opts ]
              emitter
       "lower" ->
         undefined
         -- TODO: implement
       "check" ->
         undefined
         -- TODO: implement
       cmd ->
         error $ "Unrecognized command: " ++ show cmd
