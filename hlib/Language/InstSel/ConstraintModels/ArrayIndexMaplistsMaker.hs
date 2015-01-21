--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ConstraintModels.ArrayIndexMaplistsMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
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

module Language.InstSel.ConstraintModels.ArrayIndexMaplistsMaker
  ( mkArrayIndexMaplists )
where

import Language.InstSel.ConstraintModels.Base
import Language.InstSel.Graphs
import Language.InstSel.Functions
  ( Function (..) )
import Language.InstSel.OpStructures
  ( OpStructure (..) )
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.PatternMatching
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
      d_nodes = filter isDataNode nodes
      s_nodes = filter isStateNode nodes
      l_nodes = filter isLabelNode nodes
      match_ids = map pmMatchID (matches)
      registers = tmRegisters tm
      instructions = tmInstructions tm
  in ArrayIndexMaplists
       { ai2OpNodeIDs = map getNodeID o_nodes
       , ai2DataNodeIDs = map getNodeID d_nodes
       , ai2StateNodeIDs = map getNodeID s_nodes
       , ai2LabelNodeIDs = map getNodeID l_nodes
       , ai2MatchIDs = match_ids
       , ai2RegisterIDs = map regID registers
       , ai2InstructionIDs = map instrID instructions
       }
