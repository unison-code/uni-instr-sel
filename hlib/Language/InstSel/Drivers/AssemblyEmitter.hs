--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.AssemblyEmitter
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a CP model solution and post parameters, and emits the corresponding
-- assembly code.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.AssemblyEmitter
  ( run )
where

import Language.InstSel.Drivers.Base

import Language.InstSel.CPModel
import Language.InstSel.CPModel.PostProcessor
import Language.InstSel.Graphs.IDs
  ( MatchID
  , NodeID
  )
import Language.InstSel.Functions
  ( BasicBlockLabel )
import Language.InstSel.TargetMachines.Targets

import Data.List
  ( intercalate )
import Data.Maybe
  ( fromJust )



-------------
-- Functions
-------------

getPIsAllocatedToBB
  :: CPSolutionData
  -> NodeID
      -- ^ The node ID of the corresponding label node.
  -> [MatchID]
getPIsAllocatedToBB cp_data n =
  map fst $ filter (\t -> snd t == n) $ bbAllocsForMatches cp_data

labNodes2BBLabels :: CPSolutionData -> [NodeID] -> [BasicBlockLabel]
labNodes2BBLabels cp_data ns =
  let bb_maps = funcBasicBlockParams $ functionParams $ modelParams cp_data
  in map (\n -> bbLabel $ head $ filter (\m -> bbLabelNode m == n) bb_maps) ns

genCode :: CPSolutionData -> [String]
genCode cp_data =
  let labs = orderOfBBs cp_data
      pi_lists = map (getPIsAllocatedToBB cp_data) labs
      dags = map (mkControlDataFlowDAG cp_data) pi_lists
      tm = fromJust
           $ getTargetMachine
           $ machID
           $ machineParams
           $ modelParams cp_data
      is = map (emitInstructions cp_data tm) dags
      bb_code = zipWith
                (\bb ss -> (show bb ++ ":"):ss)
                (labNodes2BBLabels cp_data labs)
                is
  in concat bb_code

run
  :: String
     -- ^ The solution in JSON format.
  -> String
     -- ^ The post parameters in JSON format.
  -> IO [Output]
     -- ^ The produced output.
run s_str p_str =
  do s_data <- parseJson s_str
     p_data <- parseJson p_str
     let cp_data = fromRawCPSolutionData p_data s_data
         code = genCode cp_data
     return [toOutputWithoutID $ intercalate "\n" code]
