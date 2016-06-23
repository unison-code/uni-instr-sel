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

import Language.InstrSel.Graphs
import Language.InstrSel.Functions
import Language.InstrSel.Utils
import Data.Maybe
import Data.List
import Language.InstrSel.TargetMachines

import Language.InstrSel.OpStructures

-------------
-- Functions
-------------

-- | Raises a low-level CP model solution to a high-level CP model solution.
raiseLowLevelSolution
  :: LowLevelSolution
  -> HighLevelModelWOp
  -> TargetMachine
  -> ArrayIndexMaplists
  -> HighLevelSolution
raiseLowLevelSolution
  sol@(LowLevelSolution {})
  model
  tm
  ai_maps
  =
  let ai_maps_for_matches = ai2MatchIDs ai_maps
      ai_maps_for_blocks = ai2BlockNodeIDs ai_maps
      ai_maps_for_data = ai2DatumNodeIDs ai_maps
      ai_maps_for_operands = ai2OperandIDs ai_maps
      ai_maps_for_locations = ai2LocationIDs ai_maps
      getNodeIDFromBlockAI ai = ai_maps_for_blocks !! (fromIntegral ai)
      getNodeIDFromAI ai = ai_maps_for_data !! (fromIntegral ai)
      getLocationIDFromAI ai = ai_maps_for_locations !! (fromIntegral ai)
      order_of_blocks = map getNodeIDFromBlockAI (llSolOrderOfBlocks sol)
      sel_matches =
        catMaybes
        $ zipWith (\is_sel mid -> if is_sel then Just mid else Nothing)
                  (llSolIsMatchSelected sol)
                  ai_maps_for_matches
      nodes_of_operands =
        zipWith (\ai oid -> (oid, getNodeIDFromAI ai))
                (llSolNodesOfOperands sol)
                ai_maps_for_operands
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
               , hlSolNodesOfOperands = nodes_of_operands
               , hlSolBlocksOfSelMatches = blocks_of_sel_matches
               , hlSolLocationsOfData = locs_of_data
               , hlSolCost = llSolCost sol
               , hlIsOptimal = llIsOptimal sol
               , hlSolTime = llSolTime sol
               , hlCoreSolTime = llCoreSolTime sol
               }
      hl_sol' = deleteExplicitFallthroughs model tm hl_sol
  in hl_sol'

raiseLowLevelSolution NoLowLevelSolution _ _ _ = NoHighLevelSolution

-- | Implements unconditional branches as fallthroughs whenever possible (due to
-- a model limitation, this can happen if empty blocks are placed in between the
-- unconditional branch and the target block)
deleteExplicitFallthroughs
    :: HighLevelModelWOp
    -> TargetMachine
    -> HighLevelSolution
    -> HighLevelSolution
deleteExplicitFallthroughs
  model
  tm
  sol@(HighLevelSolution {})
  =
  let matches  = hlSolSelMatches sol
      brs      = filter (isUnconditionalBranch model tm) matches
      brs'     = filter (isExplicitFallthrough sol model) brs
  in case brs' of
       []   -> sol
       brs'' ->
           let sol' = foldl (removeMatch model) sol brs''
           in deleteExplicitFallthroughs model tm sol'

deleteExplicitFallthroughs _ _ NoHighLevelSolution = NoHighLevelSolution

isUnconditionalBranch :: HighLevelModelWOp -> TargetMachine -> MatchID -> Bool
isUnconditionalBranch model tm match =
  let mp = getHLMatchParams (hlWOpMatchParams model) match
      os = patOS $ getInstrPattern (tmInstructions tm)
                                   (hlWOpMatchInstructionID mp)
                                   (hlWOpMatchPatternID mp)
      ns = getAllNodes $ osGraph os
   -- TODO: there might be a more elegant/robust way of characterizing
  -- unconditional branches, this is mostly a hack for the CP2015 paper
  in length ns == 3 &&
     length (filter isControlNode ns) == 1 &&
     length (filter isBlockNode ns) == 2 &&
     length (osConstraints os) == 1

isExplicitFallthrough
  :: HighLevelSolution
  -> HighLevelModelWOp
  -> MatchID
  -> Bool
isExplicitFallthrough sol model match =
  let mp = getHLMatchParams (hlWOpMatchParams model) match
      -- TODO: we assume here that the first spanned block is the source block
      -- and the second spanned block is the destination block, is this
      -- assumption safe?
      [s, d] = hlWOpMatchSpannedBlocks mp
      bs = hlSolOrderOfBlocks sol
      between = takeWhile (\b -> b /= d) $ tail $ dropWhile (\b -> b /= s) bs
  in precedes s d bs && all (isEmptyBlock sol) between

isEmptyBlock :: HighLevelSolution -> NodeID -> Bool
isEmptyBlock sol b = length (getMatchesPlacedInBlock sol b) == 1

precedes :: Eq a => a -> a -> [a] -> Bool
precedes p s l =
  let l' = dropWhile (\e -> e /= p) l
  in s `elem` l'

removeMatch
  :: HighLevelModelWOp
  -> HighLevelSolution
  -> MatchID
  -> HighLevelSolution
removeMatch model sol match =
  let mps  = getHLMatchParams (hlWOpMatchParams model) match
      b    = fromJust $ hlWOpMatchEntryBlock mps
      -- TODO: replace when optimizing for code size
      l    = hlWOpMatchLatency mps
      bps  = hlFunBlockParams $ hlWOpFunctionParams model
      f    = hlBlockExecFrequency $ fromJust $
             find (\bp -> hlBlockNode bp == b) bps
      mc   = l * (fromNatural $ fromExecFreq f)
      sol' = sol { hlSolSelMatches =
                       filter (\m -> m /= match) (hlSolSelMatches sol)
                 , hlSolBlocksOfSelMatches =
                     filter (\(m, _) -> m /= match)
                                (hlSolBlocksOfSelMatches sol)
                 , hlSolCost = hlSolCost sol - mc
                 }
  in sol'

-- TODO: the code below is copy-pasted, refactor after the CP2015 deadline!

-- | Gets the list of matches that has been allocated to a given block in the CP
-- model solution. The block is identified using the node ID of its
-- corresponding block node.
getMatchesPlacedInBlock :: HighLevelSolution -> NodeID -> [MatchID]
getMatchesPlacedInBlock sol n =
  map fst $ filter (\t -> snd t == n) $ hlSolBlocksOfSelMatches sol


-- | Retrieves the 'HighLevelMatchParamsWOp' entity with matching match ID. It
-- is assumed that exactly one such entity always exists in the given list.
getHLMatchParams
  :: [HighLevelMatchParamsWOp]
  -> MatchID
  -> HighLevelMatchParamsWOp
getHLMatchParams ps mid = head $ filter (\p -> hlWOpMatchID p == mid) ps

-- | Retrieves the 'InstrPattern' entity with matching pattern ID. It is assumed
-- that such an entity always exists in the given list.
getInstrPattern :: [Instruction] -> InstructionID -> PatternID -> InstrPattern
getInstrPattern is iid pid =
  let instr = findInstruction is iid
      pat = findInstrPattern (instrPatterns $ fromJust instr) pid
  in fromJust pat
