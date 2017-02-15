{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}
{-
Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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

import UniIS.Drivers
import qualified UniIS.Drivers.CheckDispatcher as Check
import qualified UniIS.Drivers.MakeDispatcher as Make
import qualified UniIS.Drivers.PlotDispatcher as Plot
import qualified UniIS.Drivers.TransformDispatcher as Transform

import Language.InstrSel.Utils.IO
  ( successExitCode )
import Language.InstrSel.Utils.String
  ( splitOn
  , toLower
  )

import System.Console.CmdArgs
import System.Console.CmdArgs.Text

import Data.Maybe
  ( fromJust
  , isNothing
  )

import System.Exit
  ( exitWith )



-------------
-- Functions
-------------

parseArgs :: Options
parseArgs =
  Options
    { command = def
        &= argPos 0
        &= typ "COMMAND"
    , functionFile = def
        &= name "f"
        &= name "function-file"
        &= explicit
        &= typFile
        &= help "File containing a function."
    , patternMatchsetFile = def
        &= typFile
        &= explicit
        &= name "p"
        &= name "pattern-matchset-file"
        &= help "File containing a pattern matchset."
    , arrayIndexMaplistsFile = def
        &= name "a"
        &= name "array-index-maplists-file"
        &= explicit
        &= typFile
        &= help "File containing array index maplists."
    , modelFile = def
        &= typFile
        &= explicit
        &= name "m"
        &= name "model-file"
        &= help "File containing a CP model instance."
    , solutionFile = def
        &= name "s"
        &= name "solution-file"
        &= explicit
        &= typFile
        &= help "File containing a CP model solution."
    , outFile = def
        &= name "o"
        &= name "output"
        &= explicit
        &= help ( "File that will contain the output. If the output " ++
                  "involves multiple files, each file will be suffixed with " ++
                  "a unique ID."
                )
        &= typFile
    , targetName = def
        &= name "t"
        &= name "target-name"
        &= explicit
        &= typ "TARGET"
        &= help "Name of a target machine."
    , instructionID = def
        &= name "instruction"
        &= explicit
        &= typ "ID"
        &= help "ID of an instruction."
    , patternID = def
        &= name "pattern"
        &= explicit
        &= typ "ID"
        &= help "ID of a pattern."
    , makeAction =
        enum [ MakeNothing
                 &= auto
                 &= ignore
             , MakePatternMatchset
                 &= name "compute-pattern-matchset"
                 &= explicit
                 &= help ( "Computes the pattern matchset by performing " ++
                           "pattern matching on the given function using " ++
                           "the instruction patterns available in the given " ++
                           "target machine."
                         )
             , MakeArrayIndexMaplists
                 &= name "compute-array-index-maplists"
                 &= explicit
                 &= help ( "Computes the array index maplists from " ++
                           "the given function graph and pattern matchset. "
                         )
             , MakeLowLevelModelDump
                 &= name "dump-low-level-model-info"
                 &= explicit
                 &= help ( "Dumps useful information about the low-level CP " ++
                           "model instance."
                         )
             , MakeLowLevelSolutionDump
                 &= name "dump-low-level-solution-info"
                 &= explicit
                 &= help ( "Dumps useful information about the low-level CP " ++
                           "solution instance."
                         )
             , MakeHighLevelCPModel
                 &= name "construct-hl-cp-model"
                 &= explicit
                 &= help ( "Constructs a high-level CP model instance from " ++
                           "the given function graph and pattern matchset ."
                         )
             , MakeAssemblyCode
                 &= name "generate-asm"
                 &= explicit
                 &= help ( "Generates the corresponding assembly code from " ++
                           "the given high-level CP model and solution."
                         )
             ]
        &= groupname "'make' command flags"
    , transformAction =
        enum [ TransformNothing
                 &= auto
                 &= ignore
             , RemoveDeadCodeInFunctionGraph
                 &= name "remove-dead-code-in-fun"
                 &= explicit
                 &= help "Removes dead code in function graph."
             , RemoveRedundantConversionsInFunctionGraph
                 &= name "remove-conv-redundancies-in-fun"
                 &= explicit
                 &= help "Removes redundant type conversions in function graph."
             , EnforcePhiNodeInvariantsInFunctionGraph
                 &= name "enforce-phi-invariants-in-fun"
                 &= explicit
                 &= help ( "Transforms phi nodes such that graph invariants " ++
                           "are maintained."
                         )
             , RemovePhiNodeRedundanciesInFunctionGraph
                 &= name "remove-phi-redundancies-in-fun"
                 &= explicit
                 &= help "Removes redundancies concerning phi nodes."
             , LowerPointersInFunctionGraph
                 &= name "lower-pointers-in-fun"
                 &= explicit
                 &= help ( "Rewrites pointer values into integer values in " ++
                           "given function graph."
                         )
             , CopyExtendFunctionGraph
                 &= name "copy-extend-fun"
                 &= explicit
                 &= help "Extends the given function with copies."
             , BranchExtendFunctionGraph
                 &= name "branch-extend-fun"
                 &= explicit
                 &= help ( "Extends the given function with additional " ++
                           "branches alone every conditional control flow " ++
                           "edge."
                         )
             , CombineConstantsInFunctionGraph
                 &= name "combine-consts-in-fun"
                 &= explicit
                 &= help ( "Combines data nodes in the given function that " ++
                           "represent the same constant."
                         )
             , AlternativeExtendPatternMatchset
                 &= name "alternative-extend-pat"
                 &= explicit
                 &= help ( "Inserts alternative mappings for value nodes " ++
                           "that could take its value from multiple " ++
                           "copy-related value nodes."
                         )
             , RaiseLowLevelCPSolution
                 &= name "raise-ll-cp-solution"
                 &= explicit
                 &= help ( "Raises a low-level CP model solution to a " ++
                           "high-level CP model solution."
                         )
             , LowerHighLevelCPModel
                 &= name "lower-hl-cp-model"
                 &= explicit
                 &= help ( "Lowers a high-level CP model instance to a " ++
                           "low-level CP model instance."
                         )
             ]
        &= groupname "'transform' command flags"
    , plotAction =
        enum [ PlotNothing
                 &= auto
                 &= ignore
             , PlotFunctionFullGraph
                 &= name "plot-fun-full-graph"
                 &= explicit
                 &= help "Plots the full graph (in DOT format) of a function."
             , PlotFunctionControlFlowGraph
                 &= name "plot-fun-cf-graph"
                 &= explicit
                 &= help ( "Plots the control flow graph (in DOT format) " ++
                           "of a function."
                         )
             , PlotFunctionSSAGraph
                 &= name "plot-fun-ssa-graph"
                 &= explicit
                 &= help "Plots the SSA graph (in DOT format) of a function."
             , PlotPatternFullGraph
                 &= name "plot-pat-full-graph"
                 &= explicit
                 &= help "Plots the full graph (in DOT format) of a pattern."
             , PlotPatternControlFlowGraph
                 &= name "plot-pat-cf-graph"
                 &= explicit
                 &= help ( "Plots the control flow graph (in DOT format) " ++
                           "of a pattern."
                         )
             , PlotPatternSSAGraph
                 &= name "plot-pat-ssa-graph"
                 &= explicit
                 &= help "Plots the SSA graph (in DOT format) of a pattern."
             , PlotCoverAllMatches
                 &= name "plot-cover-all-matches"
                 &= explicit
                 &= help ( "Same as --plot-fun-graph, but also marks the " ++
                           "nodes that is potentially covered by some match."
                         )
             , PlotCoverPerMatch
                 &= name "plot-cover-per-match"
                 &= explicit
                 &= help ( "Same as --plot-cover-all-matches, but produces " ++
                           "a separate plot for each individual match."
                         )
             ]
        &= groupname "'plot' command flags"
    , showEdgeNumbers = def
        &= name "show-edge-numbers"
        &= explicit
        &= help "Whether to show edge numbers in the plot."
    , hideNullInstructions = def
        &= name "hide-null-instrs"
        &= explicit
        &= help ( "Whether to exclude null instructions in the cover plots." ++
                  " Only usable together with --plot-cover-all-matches and " ++
                  "--check-fun-coverage options."
                )
    , hideInactiveInstructions = def
        &= name "hide-inactive-instrs"
        &= explicit
        &= help ( "Whether to exclude inactive instructions in the cover " ++
                  "plots. Only usable together with " ++
                  "--plot-cover-all-matches and --check-fun-coverage options."
                )
    , altLimit = def
        &= name "alt-limit"
        &= explicit
        &= typ "ID"
        &= help "Maximum number of alternatives (per case)."
    , checkAction =
        enum [ CheckNothing
                 &= auto
                 &= ignore
             , CheckFunctionIntegrity
                 &= name "check-fun-integrity"
                 &= explicit
                 &= help ( "Checks the integrity of a function." )
             , CheckPatternIntegrity
                 &= name "check-pat-integrity"
                 &= explicit
                 &= help ( "Checks the integrity of a pattern." )
             , CheckFunctionGraphCoverage
                 &= name "check-fun-coverage"
                 &= explicit
                 &= help ( "Checks whether the function graph is coverable." )
             , CheckFunctionGraphLocationOverlap
                 &= name "check-fun-location-overlap"
                 &= explicit
                 &= help ( "Checks whether all data nodes have instructions " ++
                           "with overlapping location requirements."
                         )
             ]
        &= groupname "'check' command flags"
    }
    &= helpArg [ help "Displays this message."
               , name "h"
               , name "help"
               , explicit
               , groupname "Other flags"
               ]
    &= versionArg [ ignore ]
    &= program "uni-is"
    &= summary ( "Unison (instruction selection) tool\n" ++
                 "Gabriel Hjort Blindell <ghb@kth.se>"
               )
    &= details
         ( splitOn
             "\n"
             ( showText
                 defaultWrap
                 [ Line "Available commands:"
                 , Cols [ "  make"
                        , "  Produce new data from the input."
                        ]
                 , Cols [ "  transform"
                        , "  Perform a transformation on the input."
                        ]
                 , Cols [ "  plot"
                        , "  Produce various plots for the input."
                        ]
                 , Cols [ "  check"
                        , "  Perform various checks on the input."
                        ]
                 , Line "The commands may be written in lower or upper case."
                 ]
             )
         )

-- | If an output file is given as part of the options, then the returned
-- function will emit all data to the output file with the output ID suffixed
-- to the output file name (this may mean that several output files are
-- produced). Otherwise the data will be emit to @STDOUT@.
mkEmitFunction :: Options -> IO (Output -> IO ())
mkEmitFunction opts =
  do let file = outFile opts
     if isNothing file
     then return emitToStdout
     else return $ emitToFile (fromJust file)



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     output <-
       case (toLower $ command opts) of
         "make"      -> Make.run opts
         "transform" -> Transform.run opts
         "plot"      -> Plot.run opts
         "check"     -> Check.run opts
         cmd ->
           error $ "Unrecognized command: " ++ show cmd
     emit <- mkEmitFunction opts
     mapM_ emit output
     let e_code = if length output > 0
                  then oExitCode $ last output
                  else successExitCode
     exitWith e_code
