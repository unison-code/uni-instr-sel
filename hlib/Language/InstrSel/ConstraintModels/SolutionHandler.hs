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
  let ai_match_id_maps = ai2MatchIDs ai_maps
      ai_block_node_id_maps = ai2BlockNodeIDs ai_maps
      ai_entity_node_id_maps = ai2EntityNodeIDs ai_maps
      ai_location_id_maps = ai2LocationIDs ai_maps
      getNodeIDFromBlockAI ai = ai_block_node_id_maps !! (fromIntegral ai)
      getLocationIDFromAI ai = ai_location_id_maps !! (fromIntegral ai)
      order_of_bbs = map getNodeIDFromBlockAI (llSolOrderOfBBs sol)
      sel_matches =
        catMaybes
        $ zipWith (\is_sel mid -> if is_sel then Just mid else Nothing)
                  (llSolIsMatchSelected sol)
                  ai_match_id_maps
      bbs_of_sel_matches =
        catMaybes $ zipWith3
                    ( \is_sel mid ai ->
                        if is_sel
                        then Just (mid, getNodeIDFromBlockAI ai)
                        else Nothing
                    )
                    (llSolIsMatchSelected sol)
                    ai_match_id_maps
                    (llSolBBsOfMatches sol)
      locs_of_data_nodes =
        catMaybes $ zipWith3
                    ( \has_reg nid ai ->
                        if has_reg
                        then Just (nid, getLocationIDFromAI ai)
                        else Nothing
                    )
                    (llSolHasValueNodeLocation sol)
                    ai_entity_node_id_maps
                    (llSolLocsOfValueNodes sol)
  in HighLevelSolution
       { hlSolOrderOfBBs = order_of_bbs
       , hlSolSelMatches = sel_matches
       , hlSolBBsOfSelMatches = bbs_of_sel_matches
       , hlSolLocsOfValueNodes = locs_of_data_nodes
       , hlSolCost = llSolCost sol
       , hlIsOptimal = llIsOptimal sol
       }
raiseLowLevelSolution NoLowLevelSolution _ = NoHighLevelSolution
