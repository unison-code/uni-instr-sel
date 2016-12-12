{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>
  Roberto Castaneda Lozano <rcas@sics.se>

-}

module Language.InstrSel.ConstraintModels.SolutionHandler
  ( raiseLowLevelSolution )
where

import Language.InstrSel.ConstraintModels.Base

import Data.Maybe
  ( catMaybes )



-------------
-- Functions
-------------

-- | Raises a low-level CP model solution to a high-level CP model solution.
raiseLowLevelSolution
  :: LowLevelSolution
  -> HighLevelModel
  -> ArrayIndexMaplists
  -> HighLevelSolution
raiseLowLevelSolution
  sol@(LowLevelSolution {})
  _
  ai_maps
  =
  let ai_maps_for_matches = ai2MatchIDs ai_maps
      ai_maps_for_blocks = ai2BlockNodeIDs ai_maps
      ai_maps_for_data = ai2DatumNodeIDs ai_maps
      ai_maps_for_operands = ai2OperandIDs ai_maps
      ai_maps_for_locations = ai2LocationIDs ai_maps
      getNodeIDFromBlockAI ai = ai_maps_for_blocks !! (fromIntegral ai)
      getNodeIDFromDatumAI ai = ai_maps_for_data !! (fromIntegral ai)
      getLocationIDFromAI ai = ai_maps_for_locations !! (fromIntegral ai)
      order_of_blocks = map getNodeIDFromBlockAI (llSolOrderOfBlocks sol)
      sel_matches =
        catMaybes $
        zipWith (\is_sel mid -> if is_sel then Just mid else Nothing)
                (llSolIsMatchSelected sol)
                ai_maps_for_matches
      alts_of_operands =
        catMaybes $ zipWith3 ( \has_alt o ai ->
                               if has_alt
                               then Just (o, getNodeIDFromDatumAI ai)
                               else Nothing
                             )
                             (llSolHasOperandAlt sol)
                             ai_maps_for_operands
                             (llSolAltsOfOperands sol)
      blocks_of_sel_matches =
        catMaybes $ zipWith3 ( \is_sel mid ai ->
                               if is_sel
                               then Just (mid, getNodeIDFromBlockAI ai)
                               else Nothing
                             )
                             (llSolIsMatchSelected sol)
                             ai_maps_for_matches
                             (llSolBlocksOfMatches sol)
      locs_of_data =
        catMaybes $ zipWith3 ( \has_reg nid ai ->
                               if has_reg
                               then Just (nid, getLocationIDFromAI ai)
                               else Nothing
                             )
                             (llSolHasDatumLocation sol)
                             ai_maps_for_data
                             (llSolLocationsOfData sol)
  in HighLevelSolution { hlSolOrderOfBlocks = order_of_blocks
                       , hlSolSelMatches = sel_matches
                       , hlSolNodesOfOperands = alts_of_operands
                       , hlSolBlocksOfSelMatches = blocks_of_sel_matches
                       , hlSolLocationsOfData = locs_of_data
                       , hlSolCost = llSolCost sol
                       , hlIsOptimal = llIsOptimal sol
                       , hlSolTime = llSolTime sol
                       , hlPrepTime = llPrepTime sol
                       }

raiseLowLevelSolution NoLowLevelSolution _ _ = NoHighLevelSolution
