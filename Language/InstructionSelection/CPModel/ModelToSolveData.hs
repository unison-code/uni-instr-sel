--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.ModelToSolveData
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data structures representing the data for the CP model to solve.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.ModelToSolveData where



import Language.InstructionSelection.Graphs (NodeId, Match, NodeMapping)



data ModelToSolveData
    = ModelToSolveData ProgramGraphData [PatternGraphData] MachineData
    deriving (Show)



-- | Describes the necessary program graph data.

data ProgramGraphData
    = ProgramGraphData {

          -- | Number of unique nodes.

          progNumUniqueNodes :: NodeId

          -- | The IDs of the label nodes.

        , progLabelNodes :: [NodeId]

          -- | The IDs of the data nodes.

        , progDataNodes :: [NodeId]

          -- | The IDs of the state nodes.

        , progStateNodes :: [NodeId]

      }
    deriving (Show)



-- | Describes the necessary pattern graph data.

data PatternGraphData
    = PatterGraphData {

          -- | Number of unique nodes.

          patNumUniqueNodes :: NodeId

          -- | The IDs of the label nodes.

        , patLabelNodes :: [NodeId]

          -- | The IDs of the use data nodes.

        , patUseDataNodes :: [NodeId]

          -- | The IDs of the def data nodes.

        , patDefDataNodes :: [NodeId]

          -- | The IDs of the use state nodes.

        , patUseStateNodes :: [NodeId]

          -- | The IDs of the def state nodes.

        , patDefStateNodes :: [NodeId]

          -- | Matches found for this pattern.

        , matches :: [Match]

      }
    deriving (Show)



-- | Describes the necessary target machine data.

data MachineData
    = MachineData {

          -- TODO: implement

      }
    deriving (Show)
