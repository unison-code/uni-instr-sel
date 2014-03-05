--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.ParamMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ParamMaker (
  mkParams
) where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Utils (Natural,
                                            removeDuplicates,
                                            toNatural)



-------------
-- Functions
-------------

mkParams :: OpStructure                 -- ^ The program function.
            -> [(OpStructure, Natural)] -- ^ The patterns.
            -> CPModelParams
mkParams func pats =
  -- TODO: implement
  CPModelParams (ProgramGraphData (countNumUniqueNodeIds func) [] [] [] [] [])
                []
                MachineData

countNumUniqueNodeIds :: OpStructure -> Natural
countNumUniqueNodeIds = toNatural . length . removeDuplicates . allNodes . graph

getAllLabelNodeIds :: OpStructure -> [NodeId]
getAllLabelNodeIds = map (nodeId) . filter (isLabelNode) . allNodes . graph

getAllDataNodeIds :: OpStructure -> [NodeId]
getAllDataNodeIds = map (nodeId) . filter (isDataNode) . allNodes . graph

getAllStateNodeIds :: OpStructure -> [NodeId]
getAllStateNodeIds = map (nodeId) . filter (isStateNode) . allNodes . graph
