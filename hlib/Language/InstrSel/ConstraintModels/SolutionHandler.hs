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
  -> HighLevelModel
  -> ArrayIndexMaplists
  -> HighLevelSolution
raiseLowLevelSolution sol@(LowLevelSolution {}) model ai_maps =
  let ai_maps_for_matches = ai2MatchIDs ai_maps
      ai_maps_for_blocks = ai2BlockNodeIDs ai_maps
      ai_maps_for_data = ai2DatumNodeIDs ai_maps
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
                    ai_maps_for_data
                    (llSolLocationsOfData sol)
      hl_sol = HighLevelSolution
               { hlSolOrderOfBlocks = order_of_blocks
               , hlSolSelMatches = sel_matches
               , hlSolBlocksOfSelMatches = blocks_of_sel_matches
               , hlSolLocationsOfData = locs_of_data
               , hlSolCost = llSolCost sol
               , hlIsOptimal = llIsOptimal sol
               , hlSolTime = llSolTime sol
               }
      hl_sol' = deleteExplicitFallthroughs model hl_sol
  in hl_sol'

raiseLowLevelSolution NoLowLevelSolution _ _ = NoHighLevelSolution

-- | Implements unconditional branches as fallthroughs whenever possible (due to
-- a model limitation, this can happen if empty blocks are placed in between the
-- unconditional branch and the target block)
deleteExplicitFallthroughs
    :: HighLevelModel
    -> HighLevelSolution
    -> HighLevelSolution

{-

TODO: handle these cases (as examples):

gsm.add.gsm_mult:

entry:
  %const.32767 = ADDiu %ZERO, 32767
  %const.-32768 = ADDiu %ZERO, -32768
  seq r?, %b, %const.-32768
  %const.-32768 = ADDiu %ZERO, -32768
  seq r?, %a, %const.-32768
  %or.cond = AND %cmp, %cmp3
  BEQ %or.cond, bb1, %ZERO
bb0:
if.else:
  %mul = MUL %conv, %conv6
  %shr10 = SRL %mul, 15
  B return
bb1:
return:
  %retval.0 = PHI %conv7, if.else, %const.32767, entry
  %temp1 = SLL %retval.0, 16
  %temp2 = SRA %temp1, 16
  RetRA %temp2


gsm.add.gsm_sub:

entry:
  %sub = SUB %conv, %conv1
  %cmp = SLTi %sub, -32768
  %const.-32768 = ADDiu %ZERO, -32768
  BEQ %cmp, bb1, %ZERO
bb0:
cond.false:
  %const.32767 = ADDiu %ZERO, 32767
  BGT %sub, %const.32767, bb3
bb2:
cond.false.selectcont:
  %phitmp = PHI %const.32767, cond.false.selecttrue, t0, cond.false
  B cond.end7
bb1:
cond.end7:
  %cond8 = PHI %const.-32768, entry, %phitmp, cond.false.selectcont
  %temp1 = SLL %cond8, 16
  %temp2 = SRA %temp1, 16
  RetRA %temp2
bb3:
cond.false.selecttrue:
  %const.32767 = ADDiu %ZERO, 32767
  B cond.false.selectcont
-}

deleteExplicitFallthroughs _model sol@(HighLevelSolution {}) = sol
deleteExplicitFallthroughs _ NoHighLevelSolution = NoHighLevelSolution
