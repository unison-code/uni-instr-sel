--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.ConstraintModels.SolutionHandler
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Functions for lowering and raising CP model solutions.
--
--------------------------------------------------------------------------------

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
  -> ArrayIndexMaplists
  -> HighLevelSolution
raiseLowLevelSolution sol@(LowLevelSolution {}) ai_maps =
  let ai_maps_for_matches = ai2MatchIDs ai_maps
      ai_maps_for_blocks = ai2BlockNodeIDs ai_maps
      ai_maps_for_entities = ai2EntityNodeIDs ai_maps
      ai_maps_for_locations = ai2LocationIDs ai_maps
      getNodeIDFromBlockAI ai = ai_maps_for_blocks !! (fromIntegral ai)
      getLocationIDFromAI ai = ai_maps_for_locations !! (fromIntegral ai)
      order_of_blocks = map getNodeIDFromBlockAI (llSolOrderOfBlocks sol)
      sel_matches =
        catMaybes
        $ zipWith (\is_sel mid -> if is_sel then Just mid else Nothing)
                  (llSolIsMatchSelected sol)
                  ai_maps_for_matches
      blocks_of_sel_matches =
        catMaybes $ zipWith3
                    ( \is_sel mid ai ->
                        if is_sel
                        then Just (mid, getNodeIDFromBlockAI ai)
                        else Nothing
                    )
                    (llSolIsMatchSelected sol)
                    ai_maps_for_matches
                    (llSolBlocksOfMatches sol)
      locs_of_data =
        catMaybes $ zipWith3
                    ( \has_reg nid ai ->
                        if has_reg
                        then Just (nid, getLocationIDFromAI ai)
                        else Nothing
                    )
                    (llSolHasDatumLocation sol)
                    ai_maps_for_entities
                    (llSolLocationsOfData sol)
  in HighLevelSolution
       { hlSolOrderOfBlocks = order_of_blocks
       , hlSolSelMatches = sel_matches
       , hlSolBlocksOfSelMatches = blocks_of_sel_matches
       , hlSolLocationsOfData = locs_of_data
       , hlSolCost = llSolCost sol
       , hlIsOptimal = llIsOptimal sol
       }
raiseLowLevelSolution NoLowLevelSolution _ = NoHighLevelSolution
