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

import UniISLLVM.Drivers
import qualified UniISLLVM.Drivers.MakeDispatcher as Make

import Language.InstrSel.Utils
  ( splitOn
  , toLower
  )
import Language.InstrSel.Utils.IO
  ( successExitCode )

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
    , outFile = def
        &= name "o"
        &= name "output"
        &= explicit
        &= help ( "File that will contain the output. If the output " ++
                  "involves multiple files, each file will be suffixed with " ++
                  "a unique ID."
                )
        &= typFile
    , makeAction =
        enum [ MakeNothing
                 &= auto
                 &= ignore
             , MakeFunctionGraphFromLLVM
                 &= name "construct-fun-from-llvm"
                 &= explicit
                 &= help "Constructs a function from an LLVM IR file."
             ]
        &= groupname "'make' command flags"
    }
    &= helpArg [ help "Displays this message."
               , name "h"
               , name "help"
               , explicit
               , groupname "Other flags"
               ]
    &= versionArg [ ignore ]
    &= program "uni-is-llvm"
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
         cmd ->
           error $ "Unrecognized command: " ++ show cmd
     emit <- mkEmitFunction opts
     mapM_ emit output
     let e_code = if length output > 0
                  then oExitCode $ last output
                  else successExitCode
     exitWith e_code
