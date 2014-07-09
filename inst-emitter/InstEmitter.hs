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

import Language.InstructionSelection.CPModel
import Language.InstructionSelection.CPModel.Json
import Language.InstructionSelection.CPModel.PostProcessor
import Language.InstructionSelection.Patterns.IDs
  ( PatternInstanceID
  , toPatternInstanceID
  )
import Language.InstructionSelection.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Control.Monad (when)
import Data.List (sortBy)
import Data.Maybe
  ( catMaybes
  , fromJust
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

getPIsAllocatedToBB :: CPSolutionData
                       -> PostParams
                       -> Integer -- ^ The basic block identifier
                       -> [PatternInstanceID]
getPIsAllocatedToBB cp pp bbi =
  let ps = map (Just . toPatternInstanceID) $ arrInd2PattInstIDs pp
      ps' = zipWith (\p bbi' -> if bbi' == bbi then p else Nothing)
                    ps (bbAllocsForPIs cp)
  in catMaybes ps'

-- | Gets a list of basic blocks in the order according to the solution.

getOrderedBBList :: CPSolutionData -> [Integer]
getOrderedBBList cp =
  let last_bb = length (orderOfBBs cp) - 1
      bbs = zip (orderOfBBs cp) $ map toInteger [0..last_bb]
      sorted_bbs = sortBy (\b1 b2 -> compare (fst b1) (fst b2)) bbs
  in map snd sorted_bbs



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
     let s_data = fromRight s_res :: CPSolutionData
         pp_data = fromRight pp_res :: PostParams
     -- TODO: implement the rest of the program
     let bbs = getOrderedBBList s_data
         pi_lists = map (getPIsAllocatedToBB s_data pp_data) bbs
         dags = map (mkDataDepDAG $ patInstData $ modelParams pp_data) pi_lists
     mapM_ (putStrLn . show) dags
     --putStrLn $ show s_data
     --putStrLn $ show pp_data
