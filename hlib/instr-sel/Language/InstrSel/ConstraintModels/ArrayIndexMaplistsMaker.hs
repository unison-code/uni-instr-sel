--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.ConstraintModels.ArrayIndexMaplistsMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Constructs the array index mapping information which is necessary to lower a
-- high-level CP model instances and to raise low-level CP model solutions.
--
--------------------------------------------------------------------------------

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
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..) )



-------------
-- Functions
-------------

mkArrayIndexMaplists
  :: Function
  -> TargetMachine
  -> [PatternMatch]
  -> ArrayIndexMaplists
mkArrayIndexMaplists function tm matches =
  let g = osGraph $ functionOS function
      nodes = getAllNodes g
      o_nodes = filter isOperationNode nodes
      e_nodes = filter isDatumNode nodes
      l_nodes = filter isBlockNode nodes
      match_ids = map pmMatchID (matches)
      locations = tmLocations tm
      instructions = tmInstructions tm
  in ArrayIndexMaplists { ai2OperationNodeIDs = map getNodeID o_nodes
                        , ai2DatumNodeIDs = map getNodeID e_nodes
                        , ai2BlockNodeIDs = map getNodeID l_nodes
                        , ai2MatchIDs = match_ids
                        , ai2LocationIDs = map locID locations
                        , ai2InstructionIDs = map instrID instructions
                        }
