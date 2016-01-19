{-
Copyright (c) 2014-2015, Gabriel Hjort Blindell <ghb@kth.se>
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

import UniTarGen.Drivers
import qualified UniTarGen.Drivers.GenerateTargetMachine as GenerateTM
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO

import Data.Maybe
  ( fromJust
  , isNothing
  )

import System.Console.CmdArgs
import System.Directory
  ( createDirectoryIfMissing )



-------------
-- Functions
-------------

parseArgs :: Options
parseArgs =
  Options
    { machDescFile = def
        &= name "m"
        &= name "machine-description"
        &= explicit
        &= typFile
        &= help "File containing a machine description."
    , outDir = def
        &= name "o"
        &= name "output"
        &= explicit
        &= help ( "Directory that will contain the output." )
        &= typDir
    }
    &= helpArg [ help "Displays this message."
               , name "h"
               , name "help"
               , explicit
               , groupname "Other flags"
               ]
    &= versionArg [ ignore ]
    &= program "uni-targen"
    &= summary ( "Unison (target machine generator) tool\n"
                 ++
                 "Gabriel Hjort Blindell <ghb@kth.se>"
               )

-- | Returns output directory specified on the command line. Reports error if no
-- directory is specified.
getOutDir :: Options -> IO FilePath
getOutDir opts =
  do let d = outDir opts
     when (isNothing d) $
       reportErrorAndExit "No output directory provided."
     return $ (fromJust d) ++ "/"

-- | If an output file is given as part of the options, then the returned
-- function will emit all data to the output file with the output ID suffixed
-- to the output file name (this may mean that several output files are
-- produced). Otherwise the data will be emit to @STDOUT@.
mkEmitFunction :: Options -> IO (Output -> IO ())
mkEmitFunction opts =
  do dir <- getOutDir opts
     return $ (\o -> do createDirectoryIfMissing True (dir ++ "/")
                        emitToFile dir o)



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     output <- GenerateTM.run opts
     emit <- mkEmitFunction opts
     mapM_ emit output
