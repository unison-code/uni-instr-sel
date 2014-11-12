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

import Language.InstSel.CPModel
import Language.InstSel.CPModel.Json
import Language.InstSel.CPModel.PostProcessor
import Language.InstSel.Graphs.IDs
  ( MatchID
  , NodeID
  )
import Language.InstSel.TargetMachine.IDs
  ( BBLabelID )
import Language.InstSel.TargetMachine.Targets
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Control.Monad
  ( when )
import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Console.CmdArgs
import System.Exit
  ( exitFailure )



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

getPIsAllocatedToBB :: CPSolutionData
                       -> NodeID              -- ^ The node ID of the
                                              -- corresponding label node.
                       -> [MatchID]
getPIsAllocatedToBB cp_data n =
  map fst $ filter (\t -> snd t == n) $ bbAllocsForMatches cp_data

labNodes2BBLabels :: CPSolutionData -> [NodeID] -> [BBLabelID]
labNodes2BBLabels cp_data ns =
  let bb_maps = funcBBLabels $ functionData $ modelParams cp_data
  in map (\n -> labBB $ head $ filter (\m -> labNode m == n) bb_maps) ns

genCode :: CPSolutionData -> [String]
genCode cp_data =
  let labs = orderOfBBs cp_data
      pi_lists = map (getPIsAllocatedToBB cp_data) labs
      dags = map (mkControlDataFlowDAG cp_data) pi_lists
      tm = fromJust
           $ getTargetMachine
           $ machID
           $ machineData
           $ modelParams cp_data
      is = map (emitInstructions cp_data tm) dags
      bb_code = zipWith
                (\bb ss -> (show bb ++ ":"):ss)
                (labNodes2BBLabels cp_data labs)
                is
  in concat bb_code



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
     s_json <- readFile $ fromJust sFile
     pp_json <- readFile $ fromJust ppFile
     let s_res = fromJson s_json
         pp_res = fromJson pp_json
     when (isLeft s_res) $
       do putStrLn $ fromLeft s_res
          exitFailure
     when (isLeft pp_res) $
       do putStrLn $ fromLeft pp_res
          exitFailure
     let cp_raw_data = fromRight s_res :: RawCPSolutionData
         pp_raw_data = fromRight pp_res :: RawPostParams
         cp_data = fromRawCPSolutionData pp_raw_data cp_raw_data
         code = genCode cp_data
     mapM_ putStrLn code
