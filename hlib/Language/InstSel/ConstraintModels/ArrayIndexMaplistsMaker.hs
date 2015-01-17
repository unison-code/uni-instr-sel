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
  ( PatternMatch (..)
  , PatternMatchset (..)
  )
import Language.InstSel.TargetMachines.Targets
  ( retrieveTargetMachine )

import Data.Maybe
  ( fromJust )


-------------
-- Functions
-------------

mkArrayIndexMaplists :: Function -> PatternMatchset -> ArrayIndexMaplists
mkArrayIndexMaplists function matchset =
  let match_ids = map pmMatchID (pmMatches matchset)
      g = osGraph $ functionOS function
      nodes = getAllNodes g
      l_nodes = filter isLabelNode nodes
      d_nodes = filter isDataNode nodes
      tm = fromJust $ retrieveTargetMachine (pmTarget matchset)
      registers = tmRegisters tm
  in ArrayIndexMaplists
       { ai2MatchIDs = match_ids
       , ai2LabelNodeIDs = map getNodeID l_nodes
       , ai2DataNodeIDs = map getNodeID d_nodes
       , ai2RegisterIDs = map regID registers
       }
