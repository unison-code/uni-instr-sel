{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.ConstraintModels.ArrayIndexMaplistsMaker
  ( mkArrayIndexMaplists )
where

import Language.InstrSel.ConstraintModels.Base
import Language.InstrSel.Graphs
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.TargetMachines

import Data.List
  ( sort
  , sortBy
  )



-------------
-- Functions
-------------

mkArrayIndexMaplists
  :: Function
  -> TargetMachine
  -> HighLevelModel
  -> ArrayIndexMaplists
mkArrayIndexMaplists function tm model =
  let g = osGraph $ functionOS function
      nodes = getAllNodes g
      o_nodes = sort $ filter isOperationNode nodes
      e_nodes = sort $ filter isDatumNode nodes
      l_nodes = sort $ filter isBlockNode nodes
      -- We sort the match parameters in increasing order of latency because
      -- chuffed (the constraint solver we use) will most likely attempt to
      -- select the matches in variable order, and doing this sorting will
      -- thereby, at least in principle, assist the solving. In addition, if the
      -- latencies are equal and one of them is a kill match, then the kill
      -- match comes first.
      compareMatches m1 m2
        | hlMatchIsKillInstruction m1 = LT
        | hlMatchIsKillInstruction m2 = GT
        | otherwise = compare (hlMatchLatency m1) (hlMatchLatency m2)
      match_params = sortBy compareMatches $
                     hlMatchParams model
      match_ids = map hlMatchID match_params
      op_ids = sort $ concatMap ((map fst) . hlOperandNodeMaps) match_params
      locations = getAllLocations tm
      instructions = getAllInstructions tm
  in ArrayIndexMaplists { ai2OperationNodeIDs = map getNodeID o_nodes
                        , ai2DatumNodeIDs = map getNodeID e_nodes
                        , ai2OperandIDs = op_ids
                        , ai2BlockNodeIDs = map getNodeID l_nodes
                        , ai2MatchIDs = match_ids
                        , ai2LocationIDs = map locID locations
                        , ai2InstructionIDs = map instrID instructions
                        }
