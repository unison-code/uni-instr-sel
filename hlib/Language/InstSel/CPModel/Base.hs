--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.Base
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

module Language.InstSel.CPModel.Base
  ( BBLabelData (..)
  , CPModelParams (..)
  , CPSolutionData (..)
  , FunctionGraphData (..)
  , MachineData (..)
  , PatternInstanceData (..)
  , RawCPSolutionData (..)
  , RawPostParams (..)
  , findPatternInstanceData
  , fromRawCPSolutionData
  )
where

import Language.InstSel.Constraints
import Language.InstSel.Graphs
  ( Domset (..)
  , NodeID (..)
  )
import Language.InstSel.Patterns.IDs
  ( PatternID
  , PatternInstanceID
  )
import Language.InstSel.TargetMachine.IDs
import Language.InstSel.Utils
  (Natural)
import Data.List
  (sortBy)
import Data.Maybe
  (catMaybes)



--------------
-- Data types
--------------

-- | Wrapper for all model parameters.
data CPModelParams =
    CPModelParams
    { funcData :: FunctionGraphData
    , patInstData :: [PatternInstanceData]
    , machData :: MachineData
    }
  deriving (Show)

-- | Describes the necessary function graph data.
data FunctionGraphData =
    FunctionGraphData
    { funcActionNodes :: [NodeID]
      -- ^ The action nodes in the function graph.

    , funcDataNodes :: [NodeID]
      -- ^ The data nodes in the function graph.

    , funcStateNodes :: [NodeID]
      -- ^ The state nodes in the function graph.

    , funcLabelDoms :: [Domset NodeID]
      -- ^ The label nodes in the function graph, along with their dominator
      -- sets.

    , funcRootLabel :: NodeID
      -- ^ The root label, or entry point into the function.

    , funcBBLabels :: [BBLabelData]
      -- ^ The basic block labels of the label nodes.

    , funcConstraints :: [Constraint]
      -- ^ The function constraints, if any.
    }
  deriving (Show)

-- | Associates a basic block label with a label node.
data BBLabelData =
    BBLabelData
    { labNode :: NodeID
      -- ^ The node ID of the label node.

    , labBB :: BBLabelID
      -- ^ The basic block label of the label node.
    }
  deriving (Show)

-- | Describes the necessary pattern instance data.
data PatternInstanceData =
    PatternInstanceData
    { patInstructionID :: InstructionID
      -- ^ The instruction ID of this pattern instance.

    , patPatternID :: PatternID
      -- ^ The pattern ID of this pattern instance.

    , patInstanceID :: PatternInstanceID
      -- ^ The matchset ID of this pattern instance.

    , patActionNodesCovered :: [NodeID]
      -- ^The action nodes in the function graph which are covered by this
      -- pattern instance.

    , patDataNodesDefined :: [NodeID]
      -- ^ The data nodes in the function graph which are defined by this
      -- pattern instance.

    , patDataNodesUsed :: [NodeID]
      -- ^ The data nodes in the function graph which are used by this pattern
      -- instance. Unlike 'patDataNodesUsedByPhis', this list contains all data
      -- nodes used by any action node appearing in this pattern instance.

    , patDataNodesUsedByPhis :: [NodeID]
      -- ^ The data nodes in the function graph which are used by phi nodes
      -- appearing this pattern instance. This information is required during
      -- instruction emission in order to break cyclic data dependencies.

    , patStateNodesDefined :: [NodeID]
      -- ^ The state nodes in the function graph which are defined by this
      -- pattern instance.

    , patStateNodesUsed :: [NodeID]
      -- ^ The state nodes in the function graph which are used by this pattern
      -- instance.

    , patLabelNodesReferred :: [NodeID]
      -- ^ The label nodes in the function graph which are referred to by this
      -- pattern instance.

    , patConstraints :: [Constraint]
      -- ^ The pattern-specific constraints, if any. All node IDs used in the
      -- patterns refer to nodes in the function graph (not the pattern graph).

    , patAUDDC :: Bool
      -- ^ Whether the use-def-dom constraints apply to this pattern
      -- instance. This will typically always be set to 'True' for all patterns
      -- instances except those of the generic phi patterns.

    , patHasControlNodes :: Bool
      -- ^ Whether the pattern contains one or more control nodes.

    , patCodeSize :: Integer
      -- ^ The size of the instruction associated with this pattern instance.

    , patLatency :: Integer
      -- ^ The latency of the instruction associated with this pattern instance.

    , patAssIDMaps :: [NodeID]
      -- | Maps an 'AssemblyID', which is denoted as the index into the list,
      -- that appear in the 'AssemblyString' of the instruction, to a particular
      -- data node in the function graph according to the pattern's operation
      -- structure and matchset. See also 'InstPattern.patAssIDMaps'.
    }
  deriving (Show)

-- | Contains the necessary target machine data.
data MachineData =
    MachineData
    { machID :: TargetMachineID
      -- ^ The identifier of the target machine.

    , machRegisters :: [RegisterID]
      -- ^ The registers in the target machine.
    }
  deriving (Show)

-- | Contains the data for a solution to the CP model.
data RawCPSolutionData =
    RawCPSolutionData
    { rawBBAllocsForPIs :: [Natural]
      -- ^ The basic block (given as array indices) to which a particular
      -- pattern instance was allocated. An array index for a pattern instance
      -- corresponds to an index into the list.

    , rawIsPISelected :: [Bool]
      -- ^ Indicates whether a particular pattern instance was selected. An
      -- array index for a pattern instance corresponds to an index into the
      -- list.

    , rawOrderOfBBs :: [Natural]
      -- ^ The order of basic blocks. An array index for a label node in the
      -- function graph corresponds to an index into the list.

    , rawHasDataNodeRegister :: [Bool]
      -- ^ Indicates whether a register has been selected for a particular data
      -- node. An array index for a data node corresponds to an index into the
      -- list.

    , rawRegsSelectedForDataNodes :: [RegisterID]
      -- ^ Specifies the register selected for a particular data node. An array
      -- index for a data node corresponds to an index into the list.  The
      -- register value is only valid if the corresponding value in
      -- 'hasDataNodeRegister' is set to 'True'.

    , rawHasDataNodeImmValue :: [Bool]
      -- ^ Indicates whether an immediate value has been assigned to a
      -- particular data node. An array index for a data node corresponds to an
      -- index into the list.

    , rawImmValuesOfDataNodes :: [Integer]
      -- ^ Specifies the immediate value assigned to a particular data node. An
      -- array index for a data node corresponds to an index into the list. The
      -- immediate value is only valid if the corresponding value in
      -- 'hasDataNodeImmValue' is set to 'True'.
    }
  deriving (Show)

-- | Contains the post-processing parameters.
data RawPostParams =
    RawPostParams
    { rawModelParams :: CPModelParams
      -- ^ The CP model parameters.

    , rawArrInd2PattInstIDs :: [PatternInstanceID]
      -- ^ The array indices-to-pattern instance id mappings.

    , rawArrInd2LabNodeIDs :: [NodeID]
      -- ^ The array indices-to-label node ID mappings.

    , rawArrInd2DataNodeIDs :: [NodeID]
      -- ^ The array indices-to-data node ID mappings.
    }
  deriving (Show)

-- | Contains the data for a solution to the CP model, converted from the raw
-- solution and post-processing parameters data.
data CPSolutionData =
    CPSolutionData
    { modelParams :: CPModelParams
      -- ^ The CP model parameters.

    , bbAllocsForPIs :: [(PatternInstanceID, NodeID)]
      -- ^ The basic block (represented by the node ID of the corresponding
      -- label node) to which a particular pattern instance was allocated.  A
      -- missing entry means that the corresponding pattern instance ID was not
      -- selected and thus not allocated to a valid basic block.

    , selectedPIs :: [PatternInstanceID]
      -- ^ The selected pattern instances.

    , orderOfBBs :: [NodeID]
      -- ^ The order of basic blocks (represented by the node ID of the
      -- corresponding label node).

    , regsOfDataNodes :: [(NodeID, RegisterID)]
      -- ^ The registers assigned for certain data nodes. A missing entry means
      -- that no register was assigned to the corresponding data node.

    , immValuesOfDataNodes :: [(NodeID, Integer)]
      -- ^ The immediate values assigned for certain data nodes. A missing entry
      -- means that no immediate value was assigned to the corresponding data
      -- node.
    }
  deriving (Show)



-------------
-- Functions
-------------

-- | Converts raw CP solution and post-processing parameters data into a more
-- convenient form.
fromRawCPSolutionData ::
     RawPostParams
  -> RawCPSolutionData
  -> CPSolutionData
fromRawCPSolutionData pp_data cp_data =
  CPSolutionData
  (rawModelParams pp_data)
  (computeBBAllocsForPIs pp_data cp_data)
  (computeSelectionOfPIs pp_data cp_data)
  (computeOrderOfBBs pp_data cp_data)
  (computeRegsOfDataNodes pp_data cp_data)
  (computeImmValuesOfDataNodes pp_data cp_data)

computeBBAllocsForPIs ::
     RawPostParams
  -> RawCPSolutionData
  -> [(PatternInstanceID, NodeID)]
computeBBAllocsForPIs pp_data cp_data =
  let bb2labs = rawArrInd2LabNodeIDs pp_data
      maps = zipWith3
             ( \p b bb -> if b
                          then Just (p, bb2labs !! (fromIntegral bb))
                          else Nothing
             )
             (rawArrInd2PattInstIDs pp_data)
             (rawIsPISelected cp_data)
             (rawBBAllocsForPIs cp_data)
  in catMaybes maps

computeSelectionOfPIs ::
     RawPostParams
  -> RawCPSolutionData
  -> [PatternInstanceID]
computeSelectionOfPIs pp_data cp_data =
  let keeps = zipWith
              (\p b -> if b then Just p else Nothing)
              (rawArrInd2PattInstIDs pp_data)
              (rawIsPISelected cp_data)
  in catMaybes keeps

computeOrderOfBBs ::
     RawPostParams
  -> RawCPSolutionData
  -> [NodeID]
computeOrderOfBBs pp_data cp_data =
  let lab_order = zip (rawArrInd2LabNodeIDs pp_data) (rawOrderOfBBs cp_data)
      sorted_labs = sortBy (\l1 l2 -> compare (snd l1) (snd l2)) lab_order
  in map fst sorted_labs

computeRegsOfDataNodes ::
     RawPostParams
  -> RawCPSolutionData
  -> [(NodeID, RegisterID)]
computeRegsOfDataNodes pp_data cp_data =
  let keeps = zipWith3
              (\n b r -> if b then Just (n, r) else Nothing)
              (rawArrInd2DataNodeIDs pp_data)
              (rawHasDataNodeRegister cp_data)
              (rawRegsSelectedForDataNodes cp_data)
  in catMaybes keeps

computeImmValuesOfDataNodes ::
     RawPostParams
  -> RawCPSolutionData
  -> [(NodeID, Integer)]
computeImmValuesOfDataNodes pp_data cp_data =
  let keeps = zipWith3
              (\n b r -> if b then Just (n, r) else Nothing)
              (rawArrInd2DataNodeIDs pp_data)
              (rawHasDataNodeImmValue cp_data)
              (rawImmValuesOfDataNodes cp_data)
  in catMaybes keeps

-- | Given a list of pattern instance data, the function finds the
-- 'PatternInstanceData' entity with matching pattern instance ID. If there is
-- more than one match, the first found is returned. If no such entity is found,
-- 'Nothing' is returned.
findPatternInstanceData ::
     [PatternInstanceData]
  -> PatternInstanceID
  -> Maybe PatternInstanceData
findPatternInstanceData ps piid =
  let found = filter (\p -> patInstanceID p == piid) ps
  in if length found > 0
     then Just $ head found
     else Nothing
