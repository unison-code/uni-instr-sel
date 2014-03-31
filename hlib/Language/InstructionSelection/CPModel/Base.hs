--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
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

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Graphs ( NodeId (..)
                                            , Matchset (..)
                                            )
import Language.InstructionSelection.Patterns.Ids (InstanceId)
import Language.InstructionSelection.PrettyPrint
import Language.InstructionSelection.TargetMachine (RegisterId)



--------------
-- Data types
--------------

-- | Wrapper for all model parameters.

data CPModelParams
    = CPModelParams {
          funcData :: FunctionGraphData
        , patInstData :: [PatternInstanceData]
        , machData :: MachineData
      }
    deriving (Show)

-- | Describes the necessary function graph data.

data FunctionGraphData
    = FunctionGraphData {

          -- | The action nodes in the function graph.

          funcActionNodes :: [NodeId]

          -- | The data nodes in the function graph.

        , funcDataNodes :: [NodeId]

          -- | The state nodes in the function graph.

        , funcStateNodes :: [NodeId]

          -- | The label nodes in the function graph, along with their dominator
          -- sets.

        , funcLabelDoms :: [( NodeId   -- ^ The dominated label node.
                            , [NodeId] -- ^ The dominator set.
                            )]

          -- | The function constraints, if any.

        , funcConstraints :: [Constraint]

      }
    deriving (Show)

-- | Describes the necessary pattern instance data.

data PatternInstanceData
    = PatternInstanceData {

          -- | The matchset ID of this pattern instance.

          patInstanceId :: InstanceId

          -- | The action nodes in the function graph which are covered by this
          -- pattern instance.

        , patActionNodesCovered :: [NodeId]

          -- | The data nodes in the function graph which are defined by this
          -- pattern instance.

        , patDataNodesDefined :: [NodeId]

          -- | The data nodes in the function graph which are used by this
          -- pattern instance.

        , patDataNodesUsed :: [NodeId]

          -- | The state nodes in the function graph which are defined by this
          -- pattern instance.

        , patStateNodesDefined :: [NodeId]

          -- | The state nodes in the function graph which are used by this
          -- pattern instance.

        , patStateNodesUsed :: [NodeId]

          -- | The pattern-specific constraints, if any. All node IDs used in
          -- the patterns refer to nodes in the function graph (not the pattern
          -- graph).

        , patConstraints :: [Constraint]

          -- | Whether the use-def constraints should be removed from this
          -- pattern instance. This only applies to the generic phi patterns.

        , patNoUseDefConstraints :: Bool

          -- | The size of the instruction associated with this pattern
          -- instance.

        , patCodeSize :: Integer

          -- | The latency of the instruction associated with this pattern
          -- instance.

        , patLatency :: Integer

      }
    deriving (Show)

-- | Contains the necessary target machine data.

data MachineData
    = MachineData {

          -- | The registers in the target machine.

          machRegisters :: [RegisterId]

      }
    deriving (Show)
