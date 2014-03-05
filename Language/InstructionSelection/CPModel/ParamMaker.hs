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
  CPModelParams (ProgramGraphData
                 (countNumUniqueNodeIds func)
                 (getAllLabelNodeIds func)
                 (computeLabelIDomMappings func)
                 (getAllDataNodeIds func)
                 (getAllStateNodeIds func)
                 (osConstraints func))
                (map mkPatternGraphData pats)
                -- TODO: fix building of machine data
                MachineData

countNumUniqueNodeIds :: OpStructure -> Natural
countNumUniqueNodeIds =
  toNatural . length . removeDuplicates . allNodes . osGraph

getAllLabelNodeIds :: OpStructure -> [NodeId]
getAllLabelNodeIds = map (nodeId) . filter (isLabelNode) . allNodes . osGraph

getAllDataNodeIds :: OpStructure -> [NodeId]
getAllDataNodeIds = map (nodeId) . filter (isDataNode) . allNodes . osGraph

getAllStateNodeIds :: OpStructure -> [NodeId]
getAllStateNodeIds = map (nodeId) . filter (isStateNode) . allNodes . osGraph

computeLabelIDomMappings :: OpStructure -> [( NodeId -- ^ The dominator node.
                                            , NodeId -- ^ The dominated node.
                                            )]
computeLabelIDomMappings os =
  -- TODO: implement
  []

mkPatternGraphData :: (OpStructure, Natural) -> PatternGraphData
mkPatternGraphData (os, id) =
  -- TODO: implement
  PatternGraphData  0 [] [] [] [] [] [] [] []
