--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ConstraintModels.SolutionHandler
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Functions for lowering and raising CP model solutions.
--
--------------------------------------------------------------------------------

module Language.InstSel.ConstraintModels.SolutionHandler
  ( raiseLowLevelSolution )
where

import Language.InstSel.ConstraintModels.Base

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
raiseLowLevelSolution sol ai_maps =
  let ai_match_id_maps = ai2MatchIDs ai_maps
      ai_label_node_id_maps = ai2LabelNodeIDs ai_maps
      ai_entity_node_id_maps = ai2EntityNodeIDs ai_maps
      ai_register_id_maps = ai2RegisterIDs ai_maps
      getNodeIDFromLabelAI ai = ai_label_node_id_maps !! (fromIntegral ai)
      getRegisterIDFromAI ai = ai_register_id_maps !! (fromIntegral ai)
      order_of_bbs = map getNodeIDFromLabelAI (llSolOrderOfBBs sol)
      sel_matches =
        catMaybes $
          zipWith
            (\is_sel mid -> if is_sel then Just mid else Nothing)
            (llSolIsMatchSelected sol)
            ai_match_id_maps
      bb_allocs_for_sel_matches =
        catMaybes $
          zipWith3
          ( \is_sel mid ai ->
              if is_sel
              then Just (mid, getNodeIDFromLabelAI ai)
              else Nothing
          )
          (llSolIsMatchSelected sol)
          ai_match_id_maps
          (llSolBBAllocsForMatches sol)
      regs_of_data_nodes =
        catMaybes $
          zipWith3
          ( \has_reg nid ai ->
              if has_reg
              then Just (nid, getRegisterIDFromAI ai)
              else Nothing
          )
          (llSolHasDataNodeRegister sol)
          ai_entity_node_id_maps
          (llSolRegsSelectedForDataNodes sol)
      imm_values_of_data_nodes =
        catMaybes $
          zipWith3
          ( \has_value nid value ->
              if has_value then Just (nid, value) else Nothing
          )
          (llSolHasDataNodeImmValue sol)
          ai_entity_node_id_maps
          (llSolImmValuesOfDataNodes sol)
  in HighLevelSolution
       { hlSolOrderOfBBs = order_of_bbs
       , hlSolSelMatches = sel_matches
       , hlSolBBAllocsForSelMatches = bb_allocs_for_sel_matches
       , hlSolRegsOfDataNodes = regs_of_data_nodes
       , hlSolImmValuesOfDataNodes = imm_values_of_data_nodes
       , hlSolCost = llSolCost sol
       }
