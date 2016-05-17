--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.ConstraintModels.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data structures representing CP model instances and solutions to
-- these instances.
--
-- There are two kinds of instances and solutions: high-level versions, and
-- low-level versions. In a high-level version, all IDs (such as node IDs, match
-- IDs, location IDs, etc.) appearing the model are left intact. In a low-level
-- version, these IDs are instead represented as array indices. The reason for
-- having a low-level version is because this simplifies the implementation of
-- the solver backend; an ID is often used to identify a specific domain
-- variable, which are typically organized as domain variable arrays, but since
-- IDs are not required to be contiguous they cannot be used as array indices
-- directly. By lowering a high-level version into a low-level version, the IDs
-- are converted into corresponding array indices such that the array indices
-- later can be converted back into the original IDs.
--
-- In addition, low-level CP model instances have a flatter hierarchy than their
-- high-level counterparts, and information appearing in the high-level CP model
-- instance that is not strictly needed for solving is removed from the
-- low-level CP model instance.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.ConstraintModels.Base
  ( ArrayIndexMaplists (..)
  , HighLevelModel (..)
  , HighLevelBlockParams (..)
  , HighLevelFunctionParams (..)
  , HighLevelMachineParams (..)
  , HighLevelMatchParams (..)
  , HighLevelSolution (..)
  , LowLevelModel (..)
  , LowLevelSolution (..)
  )
where

import Language.InstrSel.ConstraintModels.IDs

import Language.InstrSel.Constraints
import Language.InstrSel.Graphs
  ( DomSet
  , MatchID
  , NodeID
  )
import Language.InstrSel.Functions
  ( BlockName
  , ExecFreq
  )
import Language.InstrSel.TargetMachines.IDs
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Contains a high-level CP model.
data HighLevelModel
  = HighLevelModel
      { hlFunctionParams :: HighLevelFunctionParams
      , hlMachineParams :: HighLevelMachineParams
      , hlMatchParams :: [HighLevelMatchParams]
      }
  deriving (Show)

-- | Contains the high-level function graph parameters.
data HighLevelFunctionParams
  = HighLevelFunctionParams
      { hlFunOperations :: [NodeID]
        -- ^ The operations in the function graph.
      , hlFunReuses :: [NodeID]
        -- ^ The reuse nodes in the function graph.
      , hlFunData :: [NodeID]
        -- ^ The data in the function graph.
      , hlFunStates :: [NodeID]
        -- ^ The state nodes in the function graph.
      , hlFunBlocks :: [NodeID]
        -- ^ The block nodes in the function graph.
      , hlFunEntryBlock :: NodeID
        -- ^ The entry block of the function graph.
      , hlFunBlockDomSets :: [DomSet NodeID]
        -- ^ The dominator sets of the block in the function graph.
      , hlFunDefEdges :: [(NodeID, NodeID)]
        -- ^ The definition edges in the function graph. The first element is
        -- assumed to always be a block node and the second element is assumed
        -- to always be a node denoting a datum.
      , hlValueRelatedCopies :: [[NodeID]]
        -- ^ A collection of copy nodes that copy the same value.
      , hlFunBlockParams :: [HighLevelBlockParams]
        -- ^ The block information.
      , hlFunValueIntConstData :: [(NodeID, Integer)]
        -- ^ The value nodes which represent integer constants together with
        -- their values.
      , hlFunValueOriginData :: [(NodeID, String)]
        -- ^ The value nodes whose value has an origin, together with their
        -- origins.
      , hlFunCallNameData :: [(NodeID, String)]
        -- ^ The function call nodes together with their function names.
      , hlFunConstraints :: [Constraint]
        -- ^ The function constraints, if any. No constraint in this list may
        -- use array indices.
      }
  deriving (Show)

-- | Contains the high-level block information.
data HighLevelBlockParams
  = HighLevelBlockParams
      { hlBlockName :: BlockName
        -- ^ The name of this block.
      , hlBlockNode :: NodeID
        -- ^ The ID of the node representing this block.
      , hlBlockExecFrequency :: ExecFreq
        -- ^ The execution frequency of this block.
      }
  deriving (Show)

-- | Contains the high-level match parameters.
data HighLevelMatchParams
  = HighLevelMatchParams
      { hlMatchInstructionID :: InstructionID
        -- ^ The instruction ID of this match.
      , hlMatchPatternID :: PatternID
        -- ^ The pattern ID of this match.
      , hlMatchID :: MatchID
        -- ^ The matchset ID of this match.
      , hlMatchOperationsCovered :: [NodeID]
        -- ^ The operations in the function graph which are covered by this
        -- match.
      , hlMatchDataDefined :: [NodeID]
        -- ^ The data in the function graph which are defined by this match.
      , hlMatchDataUsed :: [NodeID]
        -- ^ The data in the function graph which are used by this match.
      , hlMatchEntryBlock :: Maybe NodeID
        -- ^ A block in the function graph that appears as entry block
        -- (if there is such a block) of this match.
      , hlMatchSpannedBlocks :: [NodeID]
        -- ^ Block in the function graph spanned by this match.
      , hlMatchConsumedBlocks :: [NodeID]
        -- ^ Block in the function graph consumed by this match.
      , hlMatchCodeSize :: Integer
        -- ^ The size of the instruction associated with this match.
      , hlMatchLatency :: Integer
        -- ^ The latency of the instruction associated with this match.
      , hlMatchConstraints :: [Constraint]
        -- ^ The pattern-specific constraints, if any. All node IDs used in the
        -- patterns refer to nodes in the function graph (not the pattern
        -- graph). No constraint in this list may use array indices.
      , hlMatchADDUC :: Bool
        -- ^ Whether to apply the def-dom-use constraint to this match. This
        -- will typically always be set to 'True' for all matches except those
        -- of the generic phi patterns.
      , hlMatchIsCopyInstruction :: Bool
        -- ^ Whether the corresponding instruction is a copy instruction.
      , hlMatchIsNullInstruction :: Bool
        -- ^ Whether the corresponding instruction is a null instruction.
      , hlMatchHasControlFlow :: Bool
        -- ^ Whether the corresponding pattern contains any control flow.
      , hlMatchDataUsedByPhis :: [NodeID]
        -- ^ The data in the function graph which are used by phi nodes
        -- appearing this match. This information is required during instruction
        -- emission in order to break cyclic data dependencies.
      , hlMatchEmitStrNodeMaplist :: [[Maybe NodeID]]
        -- ^ A list of mappings of the node IDs that appears in the
        -- instruction's emit string template (which refer to nodes in the
        -- pattern graph) to the node IDs in the function graph which are
        -- covered by this pattern. The map list has the following appearance:
        -- the outer list corresponds to the outer list within the
        -- 'EmitStringTemplate', and each element in the inner list corresponds
        -- to an 'EmitStringPart' with the same index.
      }
  deriving (Show)

-- | Contains the high-level target machine parameters.
data HighLevelMachineParams
  = HighLevelMachineParams
      { hlMachineID :: TargetMachineID
        -- ^ The identifier of the target machine.
      , hlMachineLocations :: [LocationID]
        -- ^ The locations in the target machine.
      }
  deriving (Show)

-- | Contains a low-level CP model.
data LowLevelModel
  = LowLevelModel
      { llFunNumOperations :: Integer
        -- ^ The number of operations in the function graph.
      , llFunNumData :: Integer
        -- ^ The number of data in the function graph.
      , llFunNumBlocks :: Integer
        -- ^ The number of blocks in the function graph.
      , llFunStates :: [ArrayIndex]
        -- ^ The data that are states of the function graph.
      , llFunEntryBlock :: ArrayIndex
        -- ^ The entry block of the function graph.
      , llFunBlockDomSets :: [[ArrayIndex]]
        -- ^ The dominator set for each block in the function graph. An index
        -- into the outer list corresponds to the array index of a particular
        -- block.
      , llFunDefEdges :: [[ArrayIndex]]
        -- ^ The list of data for each block in the function graph between which
        -- there is a definition edge. An index into the outer list corresponds
        -- to the array index of a particular block.
      , llFunBBExecFreqs :: [ExecFreq]
        -- ^ The execution frequency of each block. An index into the list
        -- corresponds to the array index of a particular block in the function
        -- graph.
      , llFunConstraints :: [Constraint]
        -- ^ The constraints of the function graph. No constraint in this list
        -- may use IDs.
      , llNumLocations :: Integer
        -- ^ The number of locations available in the target machine.
      , llNumMatches :: Integer
        -- ^ The number of matches.
      , llMatchOperationsCovered :: [[ArrayIndex]]
        -- ^ The list of operation in the function graph that are covered by
        -- each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchDataDefined :: [[ArrayIndex]]
        -- ^ The list of data in the function graph that are defined by each
        -- match. An index into the outer list corresponds to the array index of
        -- a particular match.
      , llMatchDataUsed :: [[ArrayIndex]]
        -- ^ The list of data in the function graph that are used by each
        -- match. An index into the outer list corresponds to the array index of
        -- a particular match.
      , llMatchEntryBlocks :: [Maybe ArrayIndex]
        -- ^ The block in the function graph which is the entry block (if any)
        -- of each match. An index into the list corresponds to the array index
        -- of a particular match.
      , llMatchSpannedBlocks :: [[ArrayIndex]]
        -- ^ Block in the function graph spanned by this match. An index into
        -- the outer list corresponds to the array index of a particular match.
      , llMatchConsumedBlocks :: [[ArrayIndex]]
        -- ^ Block in the function graph consumed by this match. An index into
        -- the outer list corresponds to the array index of a particular match.
      , llMatchCodeSizes :: [Integer]
        -- ^ The code size of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchLatencies :: [Integer]
        -- ^ The latency of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchCopyInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to copy instructions.
      , llMatchNullInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to null instructions.
      , llMatchADDUCs :: [Bool]
        -- ^ Whether to apply the def-dom-use constraint to some match. An index
        -- into the list corresponds to the array index of a particular match.
      , llMatchConstraints :: [[Constraint]]
        -- ^ The list of constraints for each match. An index into the outer
        -- list corresponds to the array index of a particular match.
      }
  deriving (Show)

-- | Contains a solution to a high-level CP model instance.
data HighLevelSolution
  = HighLevelSolution
      { hlSolOrderOfBlocks :: [NodeID]
        -- ^ The order of blocks (represented by the node ID of the
        -- corresponding block).
      , hlSolSelMatches :: [MatchID]
        -- ^ The selected matchs.
      , hlSolBlocksOfSelMatches :: [(MatchID, NodeID)]
        -- ^ The block (represented by the node ID of the corresponding block)
        -- to which a particular match was moved. A missing entry means that the
        -- corresponding match ID was not selected and thus not moved to a valid
        -- block.
      , hlSolLocationsOfData :: [(NodeID, LocationID)]
        -- ^ The locations assigned for certain datum. A missing entry means
        -- that no location was assigned to the corresponding datum.
      , hlSolCost :: Integer
        -- ^ The cost metric of the found solution.
      , hlIsOptimal :: Bool
        -- ^ Whether this solution is optimal.
      , hlSolTime :: Maybe Double
        -- ^ Time to compute the solution.
      , hlCoreSolTime :: Maybe Double
        -- ^ Time to compute the solution within the core constraint solver.
      }
  | NoHighLevelSolution
  deriving (Show)

-- | Contains a solution to a low-level CP model instance.
data LowLevelSolution
  = LowLevelSolution
      { llSolOrderOfBlocks :: [ArrayIndex]
        -- ^ The order of blocks. An index into the list corresponds to the
        -- array index of the node in the function graph which represents a
        -- particular block.
      , llSolIsMatchSelected :: [Bool]
        -- ^ Indicates whether a particular match was selected. An index into
        -- the list corresponds to the array index of a particular match.
      , llSolBlocksOfMatches :: [ArrayIndex]
        -- ^ The array index of the block wherein a particular match was
        -- placed. An index into the list corresponds to the array index of a
        -- particular match, but this datum is only valid if the corresponding
        -- datum in 'llIsMatchSelected' is set to 'True'.
      , llSolHasDatumLocation :: [Bool]
        -- ^ Indicates whether a location has been selected for a particular
        -- datum. An index into the list corresponds to the array index of a
        -- particular datum.
      , llSolLocationsOfData :: [ArrayIndex]
        -- ^ Specifies the location of a particular datum. An index into the
        -- list corresponds to the array index of a particular datum, but this
        -- datum is only valid if the corresponding datum in
        -- 'llHasDatumLocation' is set to 'True'.
      , llSolCost :: Integer
        -- ^ The cost metric of the found solution.
      , llIsOptimal :: Bool
        -- ^ Whether this solution is optimal.
      , llSolTime :: Maybe Double
        -- ^ Time to compute the solution.
      , llCoreSolTime :: Maybe Double
        -- ^ Time to compute the solution within the core constraint solver.
      }
  | NoLowLevelSolution
  deriving (Show)

-- | Contains mappings from an array index to some ID. This is used when
-- generating the CP model instance, where we want all identifiers to be array
-- indices, which must be contiguous, instead of node IDs, match IDs, location
-- IDs, etc., which may be sparse.
data ArrayIndexMaplists
  = ArrayIndexMaplists
      { ai2OperationNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of operations.
      , ai2DatumNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of data.
      , ai2BlockNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of blocks.
      , ai2MatchIDs :: [MatchID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to match IDs.
      , ai2LocationIDs :: [LocationID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to location IDs.
      , ai2InstructionIDs :: [InstructionID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to instruction IDs.
      }



--------------------------------
-- JSON-related class instances
--------------------------------

instance FromJSON HighLevelModel where
  parseJSON (Object v) =
    HighLevelModel
      <$> v .: "function-params"
      <*> v .: "machine-params"
      <*> v .: "match-params"
  parseJSON _ = mzero

instance ToJSON HighLevelModel where
  toJSON m =
    object [ "function-params" .= (hlFunctionParams m)
           , "machine-params"  .= (hlMachineParams m)
           , "match-params"    .= (hlMatchParams m)
           ]

instance FromJSON HighLevelFunctionParams where
  parseJSON (Object v) =
    HighLevelFunctionParams
      <$> v .: "operations"
      <*> v .: "reuses"
      <*> v .: "data"
      <*> v .: "states"
      <*> v .: "blocks"
      <*> v .: "entry-block"
      <*> v .: "block-dom-sets"
      <*> v .: "def-edges"
      <*> v .: "value-related-copies"
      <*> v .: "block-params"
      <*> v .: "int-constant-data"
      <*> v .: "value-origin-data"
      <*> v .: "call-name-data"
      <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON HighLevelFunctionParams where
  toJSON d =
    object [ "operations"           .= (hlFunOperations d)
           , "reuses"               .= (hlFunReuses d)
           , "data"                 .= (hlFunData d)
           , "states"               .= (hlFunStates d)
           , "blocks"               .= (hlFunBlocks d)
           , "entry-block"          .= (hlFunEntryBlock d)
           , "block-dom-sets"       .= (hlFunBlockDomSets d)
           , "def-edges"            .= (hlFunDefEdges d)
           , "value-related-copies" .= (hlValueRelatedCopies d)
           , "block-params"         .= (hlFunBlockParams d)
           , "int-constant-data"    .= (hlFunValueIntConstData d)
           , "value-origin-data"    .= (hlFunValueOriginData d)
           , "call-name-data"       .= (hlFunCallNameData d)
           , "constraints"          .= (hlFunConstraints d)
           ]

instance FromJSON HighLevelBlockParams where
  parseJSON (Object v) =
    HighLevelBlockParams
      <$> v .: "block-name"
      <*> v .: "block-node"
      <*> v .: "exec-frequency"
  parseJSON _ = mzero

instance ToJSON HighLevelBlockParams where
  toJSON d =
    object [ "block-name"     .= (hlBlockName d)
           , "block-node"     .= (hlBlockNode d)
           , "exec-frequency" .= (hlBlockExecFrequency d)
           ]

instance FromJSON HighLevelMatchParams where
  parseJSON (Object v) =
    HighLevelMatchParams
      <$> v .: "instruction-id"
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "operations-covered"
      <*> v .: "data-defined"
      <*> v .: "data-used"
      <*> v .: "entry-block"
      <*> v .: "spanned-blocks"
      <*> v .: "consumed-blocks"
      <*> v .: "code-size"
      <*> v .: "latency"
      <*> v .: "constraints"
      <*> v .: "apply-def-dom-use-constraint"
      <*> v .: "is-copy-instr"
      <*> v .: "is-null-instr"
      <*> v .: "has-control-flow"
      <*> v .: "data-used-by-phis"
      <*> v .: "emit-str-node-maps"
  parseJSON _ = mzero

instance ToJSON HighLevelMatchParams where
  toJSON d =
    object [ "instruction-id"               .= (hlMatchInstructionID d)
           , "pattern-id"                   .= (hlMatchPatternID d)
           , "match-id"                     .= (hlMatchID d)
           , "operations-covered"           .= (hlMatchOperationsCovered d)
           , "data-defined"                 .= (hlMatchDataDefined d)
           , "data-used"                    .= (hlMatchDataUsed d)
           , "entry-block"                  .= (hlMatchEntryBlock d)
           , "spanned-blocks"               .= (hlMatchSpannedBlocks d)
           , "consumed-blocks"              .= (hlMatchConsumedBlocks d)
           , "code-size"                    .= (hlMatchCodeSize d)
           , "latency"                      .= (hlMatchLatency d)
           , "constraints"                  .= (hlMatchConstraints d)
           , "apply-def-dom-use-constraint" .= (hlMatchADDUC d)
           , "is-copy-instr"                .= (hlMatchIsCopyInstruction d)
           , "is-null-instr"                .= (hlMatchIsNullInstruction d)
           , "has-control-flow"             .= (hlMatchHasControlFlow d)
           , "data-used-by-phis"            .= (hlMatchDataUsedByPhis d)
           , "emit-str-node-maps"           .= (hlMatchEmitStrNodeMaplist d)
           ]

instance FromJSON HighLevelMachineParams where
  parseJSON (Object v) =
    HighLevelMachineParams
      <$> v .: "target-machine-id"
      <*> v .: "locations"
  parseJSON _ = mzero

instance ToJSON HighLevelMachineParams where
  toJSON d =
    object [ "target-machine-id" .= (hlMachineID d)
           , "locations"         .= (hlMachineLocations d)
           ]

instance FromJSON LowLevelModel where
  parseJSON (Object v) =
    LowLevelModel
      <$> v .: "fun-num-operations"
      <*> v .: "fun-num-data"
      <*> v .: "fun-num-blocks"
      <*> v .: "fun-states"
      <*> v .: "fun-entry-block"
      <*> v .: "fun-block-dom-sets"
      <*> v .: "fun-def-edges"
      <*> v .: "fun-block-exec-freqs"
      <*> v .: "fun-constraints"
      <*> v .: "num-locations"
      <*> v .: "num-matches"
      <*> v .: "match-operations-covered"
      <*> v .: "match-data-defined"
      <*> v .: "match-data-used"
      <*> v .: "match-entry-blocks"
      <*> v .: "match-spanned-blocks"
      <*> v .: "match-consumed-blocks"
      <*> v .: "match-code-sizes"
      <*> v .: "match-latencies"
      <*> v .: "match-copy-instrs"
      <*> v .: "match-null-instrs"
      <*> v .: "match-adduc-settings"
      <*> v .: "match-constraints"
  parseJSON _ = mzero

instance ToJSON LowLevelModel where
  toJSON m =
    object [ "fun-num-operations"          .= (llFunNumOperations m)
           , "fun-num-data"                .= (llFunNumData m)
           , "fun-num-blocks"              .= (llFunNumBlocks m)
           , "fun-states"                  .= (llFunStates m)
           , "fun-entry-block"             .= (llFunEntryBlock m)
           , "fun-block-dom-sets"          .= (llFunBlockDomSets m)
           , "fun-def-edges"               .= (llFunDefEdges m)
           , "fun-block-exec-freqs"        .= (llFunBBExecFreqs m)
           , "fun-constraints"             .= (llFunConstraints m)
           , "num-locations"               .= (llNumLocations m)
           , "num-matches"                 .= (llNumMatches m)
           , "match-operations-covered"    .= (llMatchOperationsCovered m)
           , "match-data-defined"          .= (llMatchDataDefined m)
           , "match-data-used"             .= (llMatchDataUsed m)
           , "match-entry-blocks"          .= (llMatchEntryBlocks m)
           , "match-spanned-blocks"        .= (llMatchSpannedBlocks m)
           , "match-consumed-blocks"       .= (llMatchConsumedBlocks m)
           , "match-code-sizes"            .= (llMatchCodeSizes m)
           , "match-latencies"             .= (llMatchLatencies m)
           , "match-copy-instrs"           .= (llMatchCopyInstructions m)
           , "match-null-instrs"           .= (llMatchNullInstructions m)
           , "match-adduc-settings"        .= (llMatchADDUCs m)
           , "match-constraints"           .= (llMatchConstraints m)
           ]

instance FromJSON HighLevelSolution where
  parseJSON (Object v) =
    do has_solution <- v .: "has-solution"
       if has_solution
       then HighLevelSolution
              <$> v .:  "order-of-blocks"
              <*> v .:  "selected-matches"
              <*> v .:  "blocks-of-sel-matches"
              <*> v .:  "locs-of-data"
              <*> v .:  "cost"
              <*> v .:  "is-solution-optimal"
              <*> v .:? "time"
              <*> v .:? "core-time"
       else return NoHighLevelSolution
  parseJSON _ = mzero

instance ToJSON HighLevelSolution where
  toJSON d@(HighLevelSolution {}) =
    object [ "order-of-blocks"       .= (hlSolOrderOfBlocks d)
           , "selected-matches"      .= (hlSolSelMatches d)
           , "blocks-of-sel-matches" .= (hlSolBlocksOfSelMatches d)
           , "locs-of-data"          .= (hlSolLocationsOfData d)
           , "cost"                  .= (hlSolCost d)
           , "has-solution"          .= True
           , "is-solution-optimal"   .= (hlIsOptimal d)
           , "time"                  .= (hlSolTime d)
           , "core-time"             .= (hlCoreSolTime d)
           ]
  toJSON NoHighLevelSolution =
    object [ "has-solution" .= False ]

instance FromJSON LowLevelSolution where
  parseJSON (Object v) =
    do has_solution <- v .: "has-solution"
       if has_solution
       then LowLevelSolution
              <$> v .:  "order-of-blocks"
              <*> v .:  "is-match-selected"
              <*> v .:  "block-of-match"
              <*> v .:  "has-datum-loc"
              <*> v .:  "loc-of-datum"
              <*> v .:  "cost"
              <*> v .:  "is-solution-optimal"
              <*> v .:? "time"
              <*> v .:? "core-time"
       else return NoLowLevelSolution
  parseJSON _ = mzero

instance ToJSON ArrayIndexMaplists where
  toJSON d =
    object [ "array-index-to-op-node-id-maps"     .= (ai2OperationNodeIDs d)
           , "array-index-to-datum-node-id-maps"  .= (ai2DatumNodeIDs d)
           , "array-index-to-block-node-id-maps"  .= (ai2BlockNodeIDs d)
           , "array-index-to-match-id-maps"       .= (ai2MatchIDs d)
           , "array-index-to-location-id-maps"    .= (ai2LocationIDs d)
           , "array-index-to-instruction-id-maps" .= (ai2InstructionIDs d)
           ]

instance FromJSON ArrayIndexMaplists where
  parseJSON (Object v) =
    ArrayIndexMaplists
      <$> v .: "array-index-to-op-node-id-maps"
      <*> v .: "array-index-to-datum-node-id-maps"
      <*> v .: "array-index-to-block-node-id-maps"
      <*> v .: "array-index-to-match-id-maps"
      <*> v .: "array-index-to-location-id-maps"
      <*> v .: "array-index-to-instruction-id-maps"
  parseJSON _ = mzero
