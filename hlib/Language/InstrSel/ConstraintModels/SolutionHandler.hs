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

import Language.InstrSel.Graphs
import Data.Maybe
import Language.InstrSel.TargetMachines

import Language.InstrSel.OpStructures

-------------
-- Functions
-------------

-- | Raises a low-level CP model solution to a high-level CP model solution.
raiseLowLevelSolution
  :: LowLevelSolution
  -> HighLevelModel
  -> TargetMachine
  -> ArrayIndexMaplists
  -> HighLevelSolution
raiseLowLevelSolution sol@(LowLevelSolution {}) model tm ai_maps =
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
      hl_sol' = deleteExplicitFallthroughs model tm hl_sol
  in hl_sol'

raiseLowLevelSolution NoLowLevelSolution _ _ _ = NoHighLevelSolution

-- | Implements unconditional branches as fallthroughs whenever possible (due to
-- a model limitation, this can happen if empty blocks are placed in between the
-- unconditional branch and the target block)
deleteExplicitFallthroughs
    :: HighLevelModel
    -> TargetMachine
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

deleteExplicitFallthroughs model tm sol@(HighLevelSolution {}) =
    let matches  = hlSolSelMatches sol
        brs      = filter (isUnconditionalBranch model tm) matches
        brs'     = filter (isExplicitFallthrough sol model) brs
    in case brs' of
         []   -> sol
         brs'' -> deleteExplicitFallthroughs model tm (removeMatches brs'' sol)
deleteExplicitFallthroughs _ _ NoHighLevelSolution = NoHighLevelSolution

isUnconditionalBranch :: HighLevelModel -> TargetMachine -> MatchID -> Bool
isUnconditionalBranch model tm match =
    let mp = getHLMatchParams (hlMatchParams model) match
        os = patOS $ getInstrPattern (tmInstructions tm)
                                     (hlMatchInstructionID mp)
                                     (hlMatchPatternID mp)
        ns = getAllNodes $ osGraph os
     -- TODO: there might be a more elegant/robust way of characterizing
    -- unconditional branches, this is mostly a hack for the CP2015 paper
    in length ns == 3 &&
       length (filter isControlNode ns) == 1 &&
       length (filter isBlockNode ns) == 2 &&
       length (osConstraints os) == 1

isExplicitFallthrough :: HighLevelSolution -> HighLevelModel -> MatchID -> Bool
isExplicitFallthrough sol model match =
    let mp = getHLMatchParams (hlMatchParams model) match
        -- TODO: we assume here that the first spanned block is the source block
        -- and the second spanned block is the destination block, is this
        -- assumption safe?
        [s, d] = hlMatchSpannedBlocks mp
        bs = hlSolOrderOfBlocks sol
        between = takeWhile (\b -> b /= d) $ tail $ dropWhile (\b -> b /= s) bs
    in all (isEmptyBlock sol) between

isEmptyBlock :: HighLevelSolution -> NodeID -> Bool
isEmptyBlock sol b = length (getMatchesPlacedInBlock sol b) == 1

removeMatches :: [MatchID] -> HighLevelSolution -> HighLevelSolution
removeMatches matches sol = foldl removeMatch sol matches

removeMatch :: HighLevelSolution -> MatchID -> HighLevelSolution
removeMatch sol match =
    let sol' = sol {
                   hlSolSelMatches =
                     filter (\m -> m /= match) (hlSolSelMatches sol)
                 , hlSolBlocksOfSelMatches =
                     filter (\(m, _) -> m /= match) (hlSolBlocksOfSelMatches sol)
               , hlSolCost =
                   -- TODO!
                     hlSolCost sol
                   }
    in sol'

-- TODO: the code below is copy-pasted, refactor after the CP2015 deadline!

-- | Gets the list of matches that has been allocated to a given block in the CP
-- model solution. The block is identified using the node ID of its
-- corresponding block node.
getMatchesPlacedInBlock :: HighLevelSolution -> NodeID -> [MatchID]
getMatchesPlacedInBlock sol n =
  map fst $ filter (\t -> snd t == n) $ hlSolBlocksOfSelMatches sol


-- | Retrieves the @HighLevelMatchParams@ entity with matching match ID. It is
-- assumed that exactly one such entity always exists in the given list.
getHLMatchParams :: [HighLevelMatchParams] -> MatchID -> HighLevelMatchParams
getHLMatchParams ps mid = head $ filter (\p -> hlMatchID p == mid) ps

-- | Retrieves the 'InstrPattern' entity with matching pattern ID. It is assumed
-- that such an entity always exists in the given list.
getInstrPattern :: [Instruction] -> InstructionID -> PatternID -> InstrPattern
getInstrPattern is iid pid =
  let instr = findInstruction is iid
      pat = findInstrPattern (instrPatterns $ fromJust instr) pid
  in fromJust pat
