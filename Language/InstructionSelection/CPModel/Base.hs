--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data structures representing the data for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.Base where

import Language.InstructionSelection.Graphs (NodeId, Match, NodeMapping)
import Language.InstructionSelection.OpStructures (Constraint)
import Language.InstructionSelection.Utils (Natural)



data CPModelParams
    = CPModelParams ProgramGraphData [PatternGraphData] MachineData
    deriving (Show)



-- | Describes the necessary program graph data.

data ProgramGraphData
    = ProgramGraphData {

          -- | The nodes in the program graph.

          progNodes :: NodePartition

          -- | The dominator set for the basic blocks, which are represented by
          -- the label nodes.

        , progLabelDoms :: [( NodeId   -- ^ The dominated node.
                            , [NodeId] -- ^ The dominator set.
                            )]

          -- | The mappings of basic block names to label nodes.

        , progBasicBlocks :: [( String -- ^ Name of basic block.
                              , NodeId -- ^ ID of the corresponding label node.
                              )]

          -- | The program constraints, if any.

        , progConstraints :: [Constraint]

      }
    deriving (Show)



-- | Describes the necessary pattern graph data.

data PatternGraphData
    = PatternGraphData {

          -- | The nodes in the pattern graph.

          patNodes :: NodePartition

          -- | The 'use' and 'def' nodes of type Data.

        , patDataUseDefs :: UseDefNodes

          -- | The 'use' and 'def' nodes of type Label.

        , patLabelUseDefs :: UseDefNodes

          -- | The 'use' and 'def' nodes of type State.

        , patStateUseDefs :: UseDefNodes

          -- | The pattern constraints, if any.

        , patConstraints :: [Constraint]

          -- | Matches found for this pattern.

        , matches :: [Match]

      }
    deriving (Show)



-- | Contains all node IDs within a graph, partitioned after node type.

data NodePartition
    = NodePartition {

          -- | Unique IDs of the computation nodes.

          computationNodes :: [NodeId]

          -- | Unique IDs of the control nodes.

        , controlNodes :: [NodeId]

          -- | Unique IDs of the data nodes.

        , dataNodes :: [NodeId]

          -- | Unique IDs of the label nodes.

        , labelNodes :: [NodeId]

          -- | Unique IDs of the phi nodes.

        , phiNodes :: [NodeId]

          -- | Unique IDs of the state nodes.

        , stateNodes :: [NodeId]

          -- | Unique IDs of the transfer nodes.

        , transferNodes :: [NodeId]

      }
    deriving (Show)

-- | Contains the 'use' and 'def' nodes of a particular node type.

data UseDefNodes
    = UseDefNodes {

          -- | Unique IDs of the 'use' nodes.

          useNodes :: [NodeId]

          -- | Unique IDs of the 'def' nodes.

        , defNodes :: [NodeId]

      }
    deriving (Show)

-- | Describes the necessary target machine data.

data MachineData
    = MachineData {

          -- TODO: implement

      }
    deriving (Show)
