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

module Language.InstructionSelection.CPModel.Base
  ( BBLabelData (..)
  , CPModelParams (..)
  , CPSolutionData (..)
  , FunctionGraphData (..)
  , MachineData (..)
  , PatternInstanceData (..)
  , RawCPSolutionData (..)
  , RawPostParams (..)
  , fromRawCPSolutionData
  )
where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Graphs
  ( BBLabel (..)
  , Domset (..)
  , NodeID (..)
  )
import Language.InstructionSelection.Patterns.IDs
  ( InstructionID
  , PatternID
  , PatternInstanceID
  )
import Language.InstructionSelection.TargetMachine.IDs
import Language.InstructionSelection.Utils
  (Natural)
import Data.List
  (sortBy)
import Data.Maybe
  (catMaybes)



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

          -- | The instruction ID of this pattern instance.

          patInstructionID :: InstructionID

          -- | The pattern ID of this pattern instance.

        , patPatternID :: PatternID

          -- | The matchset ID of this pattern instance.

        , patInstanceID :: PatternInstanceID

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

          -- | Whether the use-def-dom constraints apply to this pattern
          -- instance. This will typically always be set to 'True' for all
          -- patterns instances except those of the generic phi patterns.

        , patAUDDC :: Bool

          -- | The size of the instruction associated with this pattern
          -- instance.

        , patCodeSize :: Integer

          -- | The latency of the instruction associated with this pattern
          -- instance.

        , patLatency :: Integer

          -- | Maps an 'AssemblyID', which is denoted as the index into the
          -- list, that appear in the 'AssemblyString' of the instruction, to a
          -- particular data node in the function graph according to the
          -- pattern's operation structure and matchset. See also
          -- 'InstPattern.patAssIDMaps'.

        , patAssIDMaps :: [NodeID]

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

data RawCPSolutionData
    = RawCPSolutionData {

          -- | The basic block (given as array indices) to which a particular
          -- pattern instance was allocated. An array index for a pattern
          -- instance corresponds to an index into the list.

          rawBBAllocsForPIs :: [Natural]

          -- | Indicates whether a particular pattern instance was selected. An
          -- array index for a pattern instance corresponds to an index into the
          -- list.

        , rawIsPISelected :: [Bool]

          -- | The order of basic blocks. An array index for a label node in the
          -- function graph corresponds to an index into the list.

        , rawOrderOfBBs :: [Natural]

          -- | Indicates whether a register has been selected for a particular
          -- data node. An array index for a data node corresponds to an index
          -- into the list.

        , rawHasDataNodeRegister :: [Bool]

          -- | Specifies the register selected for a particular data node. An
          -- array index for a data node corresponds to an index into the list.
          -- The register value is only valid if the corresponding value in
          -- 'hasDataNodeRegister' is set to 'True'.

        , rawRegsSelectedForDataNodes :: [RegisterID]

          -- | Indicates whether an immediate value has been assigned to a
          -- particular data node. An array index for a data node corresponds to
          -- an index into the list.

        , rawHasDataNodeImmValue :: [Bool]

          -- | Specifies the immediate value assigned to a particular data
          -- node. An array index for a data node corresponds to an index into
          -- the list. The immediate value is only valid if the corresponding
          -- value in 'hasDataNodeImmValue' is set to 'True'.

        , rawImmValuesOfDataNodes :: [Integer]

      }
    deriving (Show)

-- | Contains the post-processing parameters.

data RawPostParams
    = RawPostParams {

          -- | The CP model parameters.

          rawModelParams :: CPModelParams

          -- | The array indices-to-pattern instance id mappings.

        , rawArrInd2PattInstIDs :: [PatternInstanceID]

          -- | The array indices-to-label node ID mappings.

        , rawArrInd2LabNodeIDs :: [NodeID]

          -- | The array indices-to-data node ID mappings.

        , rawArrInd2DataNodeIDs :: [NodeID]

      }
    deriving (Show)

-- | Contains the data for a solution to the CP model, converted from the raw
-- solution and post-processing parameters data.

data CPSolutionData
    = CPSolutionData {

          -- | The CP model parameters.

          modelParams :: CPModelParams

          -- | The basic block (represented by the node ID of the corresponding
          -- label node) to which a particular pattern instance was allocated.
          -- A missing entry means that the corresponding pattern instance ID
          -- was not selected and thus not allocated to a valid basic block.

        , bbAllocsForPIs :: [(PatternInstanceID, NodeID)]

          -- | The selected pattern instances.

        , selectedPIs :: [PatternInstanceID]

          -- | The order of basic blocks (represented by the node ID of the
          -- corresponding label node).

        , orderOfBBs :: [NodeID]

          -- | The registers assigned for certain data nodes. A missing entry
          -- means that no register was assigned to the corresponding data node.

        , regsOfDataNodes :: [(NodeID, RegisterID)]

          -- | The immediate values assigned for certain data nodes. A missing
          -- entry means that no immediate value was assigned to the
          -- corresponding data node.

        , immValuesOfDataNodes :: [(NodeID, Integer)]

      }
    deriving (Show)



-------------
-- Functions
-------------

-- | Converts raw CP solution and post-processing parameters data into a more
-- convenient form.

fromRawCPSolutionData :: RawPostParams -> RawCPSolutionData -> CPSolutionData
fromRawCPSolutionData pp_data cp_data =
  CPSolutionData
  (rawModelParams pp_data)
  (computeBBAllocsForPIs pp_data cp_data)
  (computeSelectionOfPIs pp_data cp_data)
  (computeOrderOfBBs pp_data cp_data)
  (computeRegsOfDataNodes pp_data cp_data)
  (computeImmValuesOfDataNodes pp_data cp_data)

computeBBAllocsForPIs :: RawPostParams
                         -> RawCPSolutionData
                         -> [(PatternInstanceID, NodeID)]
computeBBAllocsForPIs pp_data cp_data =
  let bb2labs = rawArrInd2LabNodeIDs pp_data
      maps = zipWith3
             (\p b bb -> if b
                          then Just (p, bb2labs !! (fromIntegral bb))
                          else Nothing)
             (rawArrInd2PattInstIDs pp_data)
             (rawIsPISelected cp_data)
             (rawBBAllocsForPIs cp_data)
  in catMaybes maps

computeSelectionOfPIs :: RawPostParams
                         -> RawCPSolutionData
                         -> [PatternInstanceID]
computeSelectionOfPIs pp_data cp_data =
  let keeps = zipWith
              (\p b -> if b then Just p else Nothing)
              (rawArrInd2PattInstIDs pp_data)
              (rawIsPISelected cp_data)
  in catMaybes keeps

computeOrderOfBBs :: RawPostParams
                     -> RawCPSolutionData
                     -> [NodeID]
computeOrderOfBBs pp_data cp_data =
  let lab_order = zip (rawArrInd2LabNodeIDs pp_data) (rawOrderOfBBs cp_data)
      sorted_labs = sortBy (\l1 l2 -> compare (snd l1) (snd l2)) lab_order
  in map fst sorted_labs

computeRegsOfDataNodes :: RawPostParams
                          -> RawCPSolutionData
                          -> [(NodeID, RegisterID)]
computeRegsOfDataNodes pp_data cp_data =
  let keeps = zipWith3
              (\n b r -> if b then Just (n, r) else Nothing)
              (rawArrInd2DataNodeIDs pp_data)
              (rawHasDataNodeRegister cp_data)
              (rawRegsSelectedForDataNodes cp_data)
  in catMaybes keeps

computeImmValuesOfDataNodes :: RawPostParams
                               -> RawCPSolutionData
                               -> [(NodeID, Integer)]
computeImmValuesOfDataNodes pp_data cp_data =
  let keeps = zipWith3
              (\n b r -> if b then Just (n, r) else Nothing)
              (rawArrInd2DataNodeIDs pp_data)
              (rawHasDataNodeImmValue cp_data)
              (rawImmValuesOfDataNodes cp_data)
  in catMaybes keeps
