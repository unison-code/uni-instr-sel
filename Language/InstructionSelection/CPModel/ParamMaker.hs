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
            -> [(OpStructure, [Match], Natural)] -- ^ The patterns.
            -> CPModelParams
mkParams func pats =
  CPModelParams (ProgramGraphData
                 (mkNodePartition func)
                 (computeLabelIDomMappings func)
                 (osConstraints func))
                (map mkPatternGraphData pats)
                -- TODO: fix building of machine data
                MachineData

mkNodePartition :: OpStructure -> NodePartition
mkNodePartition os =
  let g = osGraph os
  in NodePartition
     (getUniqueNodeIdsByType g isComputationNode)
     (getUniqueNodeIdsByType g isControlNode)
     (getUniqueNodeIdsByType g isDataNode)
     (getUniqueNodeIdsByType g isLabelNode)
     (getUniqueNodeIdsByType g isPhiNode)
     (getUniqueNodeIdsByType g isStateNode)
     (getUniqueNodeIdsByType g isTransferNode)

getUniqueNodeIdsByType :: Graph -> (Node -> Bool) -> [NodeId]
getUniqueNodeIdsByType g f =
  removeDuplicates $ map (nodeId) $ filter (f) $ allNodes g

-- | TODO: write description

computeLabelIDomMappings :: OpStructure -> [( NodeId -- ^ The dominator node.
                                            , NodeId -- ^ The dominated node.
                                            )]
computeLabelIDomMappings os =
  -- TODO: implement
  []

mkPatternGraphData :: (OpStructure, [Match], Natural) -> PatternGraphData
mkPatternGraphData (os, matches, id) =
  let g = osGraph os
  in PatternGraphData
     (mkNodePartition os)
     (mkUseDefs g isDataNode)
     (mkUseDefs g isLabelNode)
     (mkUseDefs g isStateNode)
     (osConstraints os)
     matches

mkUseDefs :: Graph -> (Node -> Bool) -> UseDefNodes
mkUseDefs g f =
  -- TODO: implement
  UseDefNodes [] []