--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.CPModel.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data structures representing the data for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.Base where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Graphs
  ( BBLabel (..)
  , Domset (..)
  , NodeID (..)
  )
import Language.InstructionSelection.Patterns.IDs (PatternInstanceID)
import Language.InstructionSelection.TargetMachine (RegisterID)
import Language.InstructionSelection.Utils (Natural)



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

          funcActionNodes :: [NodeID]

          -- | The data nodes in the function graph.

        , funcDataNodes :: [NodeID]

          -- | The state nodes in the function graph.

        , funcStateNodes :: [NodeID]

          -- | The label nodes in the function graph, along with their dominator
          -- sets.

        , funcLabelDoms :: [Domset NodeID]

          -- | The root label, or entry point into the function.

        , funcRootLabel :: NodeID

          -- | The basic block labels of the label nodes.

        , funcBBLabels :: [BBLabelData]

          -- | The function constraints, if any.

        , funcConstraints :: [Constraint]

      }
    deriving (Show)

-- | Associates a basic block label with a label node.

data BBLabelData
    = BBLabelData {

          -- | The node ID of the label node.

          labNode :: NodeID

          -- | The basic block label of the label node.

        , labBB :: BBLabel

      }
    deriving (Show)

-- | Describes the necessary pattern instance data.

data PatternInstanceData
    = PatternInstanceData {

          -- | The matchset ID of this pattern instance.

          patInstanceID :: PatternInstanceID

          -- | The action nodes in the function graph which are covered by this
          -- pattern instance.

        , patActionNodesCovered :: [NodeID]

          -- | The data nodes in the function graph which are defined by this
          -- pattern instance.

        , patDataNodesDefined :: [NodeID]

          -- | The data nodes in the function graph which are used by this
          -- pattern instance. Unlike 'patDataNodesUsedByPhis', this list
          -- contains all data nodes used by any action node appearing in this
          -- pattern instance.

        , patDataNodesUsed :: [NodeID]

          -- | The data nodes in the function graph which are used by phi nodes
          -- appearing this pattern instance. This information is required
          -- during instruction emission in order to break cyclic data
          -- dependencies.

        , patDataNodesUsedByPhis :: [NodeID]

          -- | The state nodes in the function graph which are defined by this
          -- pattern instance.

        , patStateNodesDefined :: [NodeID]

          -- | The state nodes in the function graph which are used by this
          -- pattern instance.

        , patStateNodesUsed :: [NodeID]

          -- | The label nodes in the function graph which are referred to by
          -- this pattern instance.

        , patLabelNodesReferred :: [NodeID]

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

          machRegisters :: [RegisterID]

      }
    deriving (Show)

-- | Contains the data for a solution to the CP model.

data CPSolutionData
    = CPSolutionData {

          -- | The basic block (given as array indices) to which a particular
          -- pattern instance was allocated. An array index for a pattern
          -- instance corresponds to an index into the list.

          bbAllocsForPIs :: [Integer]

          -- | Indicates whether a particular pattern instance was selected. An
          -- array index for a pattern instance corresponds to an index into the
          -- list.

        , selectionOfPIs :: [Bool]

          -- | The order of basic blocks. An array index for a label node in the
          -- function graph corresponds to an index into the list.

        , orderOfBBs :: [Integer]

      }
    deriving (Show)

-- | Contains the post-processing parameters.

data PostParams
    = PostParams {

          -- | The CP model parameters.

          modelParams :: CPModelParams

          -- | The array indices-to-pattern instance id mappings.

        , arrInd2PattInstIDs :: [Natural]

          -- | The array indices-to-label node ID mappings.

        , arrInd2LabNodeIDs :: [Natural]

      }
    deriving (Show)
