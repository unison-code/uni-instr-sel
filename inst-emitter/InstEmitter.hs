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
Takes the solution JSON file and the post-processing parameters JSON file and
outputs (on stdout) the corresponding assembly instructions for that solution.
-}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

import Language.InstructionSelection.SolutionData
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (decode)
import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Console.CmdArgs
import System.Exit (exitFailure)



---------------------------------
-- Help functions and data types
---------------------------------

data Options
    = Options {
          sFile :: Maybe String
        , ppFile :: Maybe String
      }
    deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options {
    sFile = Nothing
        &= typFile
        &= help "The JSON file containing the solution."
  , ppFile = Nothing
        &= typFile
        &= help "The JSON file containing the post-processing parameters."
  }

getJSON :: FilePath -> IO BS.ByteString
getJSON = BS.readFile



----------------
-- Main program
----------------

main :: IO ()
main =
  do Options {..} <- cmdArgs parseArgs
     when (isNothing sFile) $
       do putStrLn "No solution file provided."
          exitFailure
     when (isNothing ppFile) $
       do putStrLn "No post-processing parameter file provided."
          exitFailure
     s_json <- getJSON $ fromJust sFile
     let s_data = decode (getJSON $ fromJust sFile) :: Maybe SolutionData
     let pp_data = decode (getJSON $ fromJust ppFile) :: Maybe PostParamData
     putStrLn $ show s_data
     putStrLn $ show pp_data
