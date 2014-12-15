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
  ( BasicBlockData (..)
  , CPModelParams (..)
  , CPSolutionData (..)
  , FunctionGraphData (..)
  , MachineData (..)
  , MatchData (..)
  , RawCPSolutionData (..)
  , RawPostParams (..)
  , findMatchData
  , fromRawCPSolutionData
  )
where

import Language.InstSel.Constraints
import Language.InstSel.Graphs
  ( Domset (..)
  , MatchID (..)
  , NodeID (..)
  )
import Language.InstSel.Patterns.IDs
  ( PatternID )
import Language.InstSel.ProgramModules
  ( BasicBlockLabel (..)
  , ExecFreq (..)
  )
import Language.InstSel.TargetMachine.IDs
import Language.InstSel.Utils
  ( Natural )
import Data.List
  ( sortBy )
import Data.Maybe
  ( catMaybes )



--------------
-- Data types
--------------

-- | Wrapper for all model parameters.
data CPModelParams =
    CPModelParams
      { functionData :: FunctionGraphData
      , matchData :: [MatchData]
      , machineData :: MachineData
      }
  deriving (Show)

-- | Describes the necessary function graph data.
data FunctionGraphData =
    FunctionGraphData
      { funcOpNodes :: [NodeID]
        -- ^ The operation nodes in the function graph.
      , funcEssentialOpNodes :: [NodeID]
        -- ^ Operation nodes that are essential, meaning they *must* be covered.
      , funcDataNodes :: [NodeID]
        -- ^ The data nodes in the function graph.
      , funcStateNodes :: [NodeID]
        -- ^ The state nodes in the function graph.
      , funcLabelNodes :: [Domset NodeID]
        -- ^ The label nodes in the function graph, along with their dominator
        -- sets.
      , funcDefPlaceEdges :: [(NodeID, NodeID)]
        -- ^ The definition placement edges in the function graph. The first
        -- node in the tuple is the entity node and the second node is the label
        -- node.
      , funcRootLabel :: NodeID
        -- ^ The root label, or entry point into the function.
      , funcBasicBlockData :: [BasicBlockData]
        -- ^ The basic block information.
      , funcConstraints :: [Constraint]
        -- ^ The function constraints, if any.
      }
  deriving (Show)

-- | Information about the basic blocks.
data BasicBlockData =
    BasicBlockData
      { bbLabel :: BasicBlockLabel
        -- ^ The label of this basic block.
      , bbLabelNode :: NodeID
        -- ^ The node ID of the label node that represents this basic block.
      , bbExecFrequency :: ExecFreq
        -- ^ The execution frequency of this basic block.
      }
  deriving (Show)

-- | Describes the necessary match data.
data MatchData =
    MatchData
      { mInstructionID :: InstructionID
        -- ^ The instruction ID of this match.
      , mPatternID :: PatternID
        -- ^ The pattern ID of this match.
      , mMatchID :: MatchID
        -- ^ The matchset ID of this match.
      , mOperationsCovered :: [NodeID]
        -- ^ The operations in the function graph which are covered by this
        -- match.
      , mDataNodesDefined :: [NodeID]
        -- ^ The data nodes in the function graph which are defined by this
        -- match.
      , mDataNodesUsed :: [NodeID]
        -- ^ The data nodes in the function graph which are used by this
        -- match. Unlike 'mDataNodesUsedByPhis', this list contains all data
        -- nodes used by any operation appearing in this match.
      , mDataNodesUsedByPhis :: [NodeID]
        -- ^ The data nodes in the function graph which are used by phi nodes
        -- appearing this match. This information is required during instruction
        -- emission in order to break cyclic data dependencies.
      , mStateNodesDefined :: [NodeID]
        -- ^ The state nodes in the function graph which are defined by this
        -- match.
      , mStateNodesUsed :: [NodeID]
        -- ^ The state nodes in the function graph which are used by this match.
      , mRootLabelNode :: Maybe NodeID
        -- ^ The label node in the function graph that appears as root label (if
        -- there is such a node) in this match.
      , mNonRootLabelNodes :: [NodeID]
        -- ^ The label nodes in the function graph that appears in this match
        -- but not as roots.
      , mConstraints :: [Constraint]
        -- ^ The pattern-specific constraints, if any. All node IDs used in the
        -- patterns refer to nodes in the function graph (not the pattern
        -- graph).
      , mADDUC :: Bool
        -- ^ Whether to apply the def-dom-use constraint to this match. This
        -- will typically always be set to 'True' for all matches except those
        -- of the generic phi patterns.
      , mHasControlNodes :: Bool
        -- ^ Whether the pattern contains one or more control nodes.
      , mCodeSize :: Integer
        -- ^ The size of the instruction associated with this match.
      , mLatency :: Integer
        -- ^ The latency of the instruction associated with this match.
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
      { rawBBAllocsForMatches :: [Natural]
        -- ^ The basic block (given as array indices) to which a particular
        -- match was allocated. An array index for a match corresponds to an
        -- index into the list.
      , rawIsMatchSelected :: [Bool]
        -- ^ Indicates whether a particular match was selected. An array index
        -- for a match corresponds to an index into the list.
      , rawOrderOfBBs :: [Natural]
        -- ^ The order of basic blocks. An array index for a label node in the
        -- function graph corresponds to an index into the list.
      , rawHasDataNodeRegister :: [Bool]
        -- ^ Indicates whether a register has been selected for a particular
        -- data node. An array index for a data node corresponds to an index
        -- into the list.
      , rawRegsSelectedForDataNodes :: [RegisterID]
        -- ^ Specifies the register selected for a particular data node. An
        -- array index for a data node corresponds to an index into the list.
        -- The register value is only valid if the corresponding value in
        -- 'hasDataNodeRegister' is set to 'True'.
      , rawHasDataNodeImmValue :: [Bool]
        -- ^ Indicates whether an immediate value has been assigned to a
        -- particular data node. An array index for a data node corresponds to
        -- an index into the list.
      , rawImmValuesOfDataNodes :: [Integer]
        -- ^ Specifies the immediate value assigned to a particular data
        -- node. An array index for a data node corresponds to an index into the
        -- list. The immediate value is only valid if the corresponding value in
        -- 'hasDataNodeImmValue' is set to 'True'.
      , rawCost :: Integer
        -- ^ The cost metric of the found solution.
      }
  deriving (Show)

-- | Contains the post-processing parameters.
data RawPostParams =
    RawPostParams
      { rawModelParams :: CPModelParams
        -- ^ The CP model parameters.
      , rawArrInd2MatchIDs :: [MatchID]
        -- ^ The array indices-to-match id mappings.
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
      , bbAllocsForMatches :: [(MatchID, NodeID)]
        -- ^ The basic block (represented by the node ID of the corresponding
        -- label node) to which a particular match was allocated.  A missing
        -- entry means that the corresponding match ID was not selected and thus
        -- not allocated to a valid basic block.
      , selectedMatches :: [MatchID]
        -- ^ The selected matchs.
      , orderOfBBs :: [NodeID]
        -- ^ The order of basic blocks (represented by the node ID of the
        -- corresponding label node).
      , regsOfDataNodes :: [(NodeID, RegisterID)]
        -- ^ The registers assigned for certain data nodes. A missing entry
        -- means that no register was assigned to the corresponding data node.
      , immValuesOfDataNodes :: [(NodeID, Integer)]
        -- ^ The immediate values assigned for certain data nodes. A missing
        -- entry means that no immediate value was assigned to the corresponding
        -- data node.
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
fromRawCPSolutionData m_data cp_data =
  CPSolutionData
    (rawModelParams m_data)
    (computeBBAllocsForMatches m_data cp_data)
    (computeSelectionOfMatches m_data cp_data)
    (computeOrderOfBBs m_data cp_data)
    (computeRegsOfDataNodes m_data cp_data)
    (computeImmValuesOfDataNodes m_data cp_data)

computeBBAllocsForMatches ::
     RawPostParams
  -> RawCPSolutionData
  -> [(MatchID, NodeID)]
computeBBAllocsForMatches m_data cp_data =
  let bb2labs = rawArrInd2LabNodeIDs m_data
      maps = zipWith3
               ( \p b bb -> if b
                            then Just (p, bb2labs !! (fromIntegral bb))
                            else Nothing
               )
               (rawArrInd2MatchIDs m_data)
               (rawIsMatchSelected cp_data)
               (rawBBAllocsForMatches cp_data)
  in catMaybes maps

computeSelectionOfMatches ::
     RawPostParams
  -> RawCPSolutionData
  -> [MatchID]
computeSelectionOfMatches m_data cp_data =
  let keeps = zipWith
                (\p b -> if b then Just p else Nothing)
                (rawArrInd2MatchIDs m_data)
                (rawIsMatchSelected cp_data)
  in catMaybes keeps

computeOrderOfBBs ::
     RawPostParams
  -> RawCPSolutionData
  -> [NodeID]
computeOrderOfBBs m_data cp_data =
  let lab_order = zip (rawArrInd2LabNodeIDs m_data) (rawOrderOfBBs cp_data)
      sorted_labs = sortBy (\l1 l2 -> compare (snd l1) (snd l2)) lab_order
  in map fst sorted_labs

computeRegsOfDataNodes ::
     RawPostParams
  -> RawCPSolutionData
  -> [(NodeID, RegisterID)]
computeRegsOfDataNodes m_data cp_data =
  let keeps = zipWith3
                (\n b r -> if b then Just (n, r) else Nothing)
                (rawArrInd2DataNodeIDs m_data)
                (rawHasDataNodeRegister cp_data)
                (rawRegsSelectedForDataNodes cp_data)
  in catMaybes keeps

computeImmValuesOfDataNodes ::
     RawPostParams
  -> RawCPSolutionData
  -> [(NodeID, Integer)]
computeImmValuesOfDataNodes m_data cp_data =
  let keeps = zipWith3
                (\n b r -> if b then Just (n, r) else Nothing)
                (rawArrInd2DataNodeIDs m_data)
                (rawHasDataNodeImmValue cp_data)
                (rawImmValuesOfDataNodes cp_data)
  in catMaybes keeps

-- | Given a list of match data, the function finds the 'MatchData' entity with
-- matching match ID. If there is more than one match, the first found is
-- returned. If no such entity is found, 'Nothing' is returned.
findMatchData ::
     [MatchData]
  -> MatchID
  -> Maybe MatchData
findMatchData ps piid =
  let found = filter (\p -> mMatchID p == piid) ps
  in if length found > 0
     then Just $ head found
     else Nothing
