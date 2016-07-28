{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.ConstraintModels.Base
  ( ArrayIndexMaplists (..)
  , HighLevelModelNoOp (..)
  , HighLevelModelWOp (..)
  , HighLevelBlockParams (..)
  , HighLevelFunctionParams (..)
  , HighLevelMachineParams (..)
  , HighLevelMatchParamsNoOp (..)
  , HighLevelMatchParamsWOp (..)
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

-- | Contains a high-level CP model that does not make use of operands.
data HighLevelModelNoOp
  = HighLevelModelNoOp
      { hlNoOpFunctionParams :: HighLevelFunctionParams
      , hlNoOpMachineParams :: HighLevelMachineParams
      , hlNoOpMatchParams :: [HighLevelMatchParamsNoOp]
      }
  deriving (Show)

-- | Contains a high-level CP model that makes use of operands.
data HighLevelModelWOp
  = HighLevelModelWOp
      { hlWOpFunctionParams :: HighLevelFunctionParams
      , hlWOpMachineParams :: HighLevelMachineParams
      , hlWOpMatchParams :: [HighLevelMatchParamsWOp]
      }
  deriving (Show)

-- | Contains the high-level function graph parameters.
data HighLevelFunctionParams
  = HighLevelFunctionParams
      { hlFunOperations :: [NodeID]
        -- ^ The operations in the function graph.
      , hlFunCopies :: [NodeID]
        -- ^ The copy nodes in the function graph.
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
      , hlFunBlockParams :: [HighLevelBlockParams]
        -- ^ The block information.
      , hlFunStateDefEdges :: [(NodeID, NodeID)]
        -- ^ The definition edges in the function graph that involve states. The
        -- first element is a block node and the second element is a state node.
      , hlFunValidValueLocs :: [(NodeID, [LocationID])]
        -- ^ The value nodes together with a list of valid locations.
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

-- | Contains the high-level match parameters, without operands.
data HighLevelMatchParamsNoOp
  = HighLevelMatchParamsNoOp
      { hlNoOpMatchInstructionID :: InstructionID
        -- ^ The instruction ID of this match.
      , hlNoOpMatchPatternID :: PatternID
        -- ^ The pattern ID of this match.
      , hlNoOpMatchID :: MatchID
        -- ^ The matchset ID of this match.
      , hlNoOpMatchOperationsCovered :: [NodeID]
        -- ^ The operations in the function graph which are covered by this
        -- match.
      , hlNoOpMatchDataDefined :: [NodeID]
        -- ^ The data in the function graph which are defined by this match.
      , hlNoOpMatchDataUsed :: [NodeID]
        -- ^ The data in the function graph which are used by this match.
      , hlNoOpMatchExternalData :: [NodeID]
        -- ^ The data in the function graph which are external to this match
        -- (i.e. either input or output).
      , hlNoOpMatchInternalData :: [NodeID]
        -- ^ The data in the function graph which are external to this match
        -- (i.e. neither input nor output).
      , hlNoOpMatchValidValueLocs :: [(NodeID, [LocationID])]
        -- ^ The data in the function graph together with a list of locations
        -- that are valid for this match.
      , hlNoOpMatchEntryBlock :: Maybe NodeID
        -- ^ A block in the function graph that appears as entry block
        -- (if there is such a block) of this match.
      , hlNoOpMatchSpannedBlocks :: [NodeID]
        -- ^ Block in the function graph spanned by this match.
      , hlNoOpMatchConsumedBlocks :: [NodeID]
        -- ^ Block in the function graph consumed by this match.
      , hlNoOpMatchCodeSize :: Integer
        -- ^ The size of the instruction associated with this match.
      , hlNoOpMatchLatency :: Integer
        -- ^ The latency of the instruction associated with this match.
      , hlNoOpMatchConstraints :: [Constraint]
        -- ^ The pattern-specific constraints, if any. All node IDs used in the
        -- patterns refer to nodes in the function graph (not the pattern
        -- graph). No constraint in this list may use array indices.
      , hlNoOpMatchIsPhiInstruction :: Bool
        -- ^ Whether the corresponding instruction is a phi instruction.
      , hlNoOpMatchIsCopyInstruction :: Bool
        -- ^ Whether the corresponding instruction is a copy instruction.
      , hlNoOpMatchIsInactiveInstruction :: Bool
        -- ^ Whether the corresponding instruction is an inactive instruction.
      , hlNoOpMatchIsNullInstruction :: Bool
        -- ^ Whether the corresponding instruction is a null instruction.
      , hlNoOpMatchHasControlFlow :: Bool
        -- ^ Whether the corresponding pattern contains any control flow.
      , hlNoOpMatchDataUsedByPhis :: [(NodeID, NodeID)]
        -- ^ The data, together with the blocks that appear in the definition
        -- edges, in the function graph which are used by phi nodes appearing
        -- this match. The first element is a block node and the second element
        -- is a datum node. This information is required for adding the
        -- necessary constraints as well as during instruction emission in order
        -- to break cyclic data dependencies.
      , hlNoOpMatchDataDefinedByPhis :: [(NodeID, NodeID)]
        -- ^ The data, together with the blocks that appear in the definition
        -- edges, in the function graph which are defined by phi nodes appearing
        -- this match. The first element is a block node and the second element
        -- is a datum node. This information is required for adding the
        -- necessary constraints.
      , hlNoOpMatchEmitStrNodeMaplist :: [[Maybe NodeID]]
        -- ^ A list of mappings of the node IDs that appears in the
        -- instruction's emit string template (which refer to nodes in the
        -- pattern graph) to the node IDs in the function graph which are
        -- covered by this pattern. The map list has the following appearance:
        -- the outer list corresponds to the outer list within the
        -- 'EmitStringTemplate', and each element in the inner list corresponds
        -- to an 'EmitStringPart' with the same index.
      }
  deriving (Show)

-- | Contains the high-level match parameters, with operands. Most fields
-- are identical to those of 'HighLevelMatchParamsWithoutOperands'.
data HighLevelMatchParamsWOp
  = HighLevelMatchParamsWOp
      { hlWOpMatchInstructionID :: InstructionID
      , hlWOpMatchPatternID :: PatternID
      , hlWOpMatchID :: MatchID
      , hlWOpOperandNodeMaps :: [(OperandID, [NodeID])]
        -- ^ Maps an operand to a list of value nodes in the function graph.
      , hlWOpMatchOperationsCovered :: [NodeID]
      , hlWOpMatchOperandsDefined :: [OperandID]
      , hlWOpMatchOperandsUsed :: [OperandID]
      , hlWOpMatchExternalOperands :: [OperandID]
      , hlWOpMatchInternalOperands :: [OperandID]
      , hlWOpMatchValidValueLocs :: [(OperandID, [LocationID])]
      , hlWOpMatchEntryBlock :: Maybe NodeID
      , hlWOpMatchSpannedBlocks :: [NodeID]
      , hlWOpMatchConsumedBlocks :: [NodeID]
      , hlWOpMatchCodeSize :: Integer
      , hlWOpMatchLatency :: Integer
      , hlWOpMatchConstraints :: [Constraint]
      , hlWOpMatchIsPhiInstruction :: Bool
      , hlWOpMatchIsCopyInstruction :: Bool
      , hlWOpMatchIsInactiveInstruction :: Bool
      , hlWOpMatchIsNullInstruction :: Bool
      , hlWOpMatchHasControlFlow :: Bool
      , hlWOpMatchOperandsUsedByPhis :: [(NodeID, OperandID)]
      , hlWOpMatchOperandsDefinedByPhis :: [(NodeID, OperandID)]
      , hlWOpMatchEmitStrNodeMaplist :: [[Maybe (Either OperandID NodeID)]]
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
      , llFunCopies :: [ArrayIndex]
        -- ^ The copy nodes of the function graph.
      , llFunStates :: [ArrayIndex]
        -- ^ The data that are state nodes of the function graph.
      , llFunValidValueLocs :: [(ArrayIndex, ArrayIndex)]
        -- ^ The valid locations for each datum in the function graph (an empty
        -- list means that all locations are valid). The first element is the
        -- array index of a particular datum, and the second element is a
        -- location.
      , llFunEntryBlock :: ArrayIndex
        -- ^ The entry block of the function graph.
      , llFunBlockDomSets :: [[ArrayIndex]]
        -- ^ The dominator set for each block in the function graph. An index
        -- into the outer list corresponds to the array index of a particular
        -- block.
      , llFunBBExecFreqs :: [ExecFreq]
        -- ^ The execution frequency of each block. An index into the list
        -- corresponds to the array index of a particular block in the function
        -- graph.
      , llFunStateDefEdges :: [(ArrayIndex, ArrayIndex)]
        -- ^ The state definition edges that appear in this match. The first
        -- element is the array index of a particular block, and the second
        -- element is the array index of a particular state.
      , llFunConstraints :: [Constraint]
        -- ^ The constraints of the function graph. No constraint in this list
        -- may use IDs.
      , llNumLocations :: Integer
        -- ^ The number of locations available in the target machine.
      , llNumMatches :: Integer
        -- ^ The number of matches.
      , llNumOperands :: Integer
        -- ^ The number of operands.
      , llOperandAlternatives :: [[ArrayIndex]]
        -- ^ The list of data in the function graph that are selectable for each
        -- operand. An index into the outer list corresponds to the array index
        -- of a particular operand.
      , llMatchOperationsCovered :: [[ArrayIndex]]
        -- ^ The list of operation in the function graph that are covered by
        -- each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchOperandsDefined :: [[ArrayIndex]]
        -- ^ The list of operands that are defined by each match. An index into
        -- the outer list corresponds to the array index of a particular match.
      , llMatchOperandsUsed :: [[ArrayIndex]]
        -- ^ The list of operands that are used by each match. An index into the
        -- outer list corresponds to the array index of a particular match.
      , llMatchExternalOperands :: [[ArrayIndex]]
        -- ^ The list of operands that are external to each match. An index into
        -- the outer list corresponds to the array index of a particular match.
      , llMatchInternalOperands :: [[ArrayIndex]]
        -- ^ The list of operands that are internal to each match. An index into
        -- the outer list corresponds to the array index of a particular match.
      , llMatchValidValueLocs :: [(ArrayIndex, ArrayIndex, ArrayIndex)]
        -- ^ The locations that are valid for a particular operand in a certain
        -- match. The first element is the array index of a particular match,
        -- the second element is an operand, and the third element is a
        -- location.
      , llMatchEntryBlocks :: [Maybe ArrayIndex]
        -- ^ The block in the function graph which is the entry block (if any)
        -- of each match. An index into the list corresponds to the array index
        -- of a particular match.
      , llMatchSpannedBlocks :: [[ArrayIndex]]
        -- ^ The blocks in the function graph spanned by each match. An index
        -- into the outer list corresponds to the array index of a particular
        -- match.
      , llMatchConsumedBlocks :: [[ArrayIndex]]
        -- ^ The blocks in the function graph consumed by each match. An index
        -- into the outer list corresponds to the array index of a particular
        -- match.
      , llMatchInputDefinitionEdges :: [(ArrayIndex, ArrayIndex, ArrayIndex)]
        -- ^ The input definition edges that appear in a particular match. The
        -- first element is the array index of a particular match, the second
        -- element is a block, and the third element is an operand.
      , llMatchOutputDefinitionEdges :: [(ArrayIndex, ArrayIndex, ArrayIndex)]
        -- ^ The output definition edges that appear in a particular match. The
        -- first element is the array index of a particular match, the second
        -- element is a block, and the third element is an operand.
      , llMatchCodeSizes :: [Integer]
        -- ^ The code size of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchLatencies :: [Integer]
        -- ^ The latency of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchCopyInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to copy instructions.
      , llMatchInactiveInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to inactive instructions.
      , llMatchNullInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to null instructions.
      , llMatchPhiInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to phi instructions.
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
      , hlSolNodesOfOperands :: [(OperandID, NodeID)]
        -- ^ The value node selected for a particular operand.
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
      , llSolHasOperandAlt :: [Bool]
        -- ^ Indicates whether an alternative has been selected for a particular
        -- operand. An index into the list corresponds to the array index of a
        -- particular operand.
      , llSolAltsOfOperands :: [ArrayIndex]
        -- ^ The array index of the alternative selected for a particular
        -- operand. An index into the list corresponds to the array index of a
        -- particular operand.
      , llSolBlocksOfMatches :: [ArrayIndex]
        -- ^ The array index of the block wherein a particular match was
        -- placed. An index into the list corresponds to the array index of a
        -- particular match, but this value is only valid if the corresponding
        -- match in 'llIsMatchSelected' is set to 'True'.
      , llSolHasDatumLocation :: [Bool]
        -- ^ Indicates whether a location has been selected for a particular
        -- datum. An index into the list corresponds to the array index of a
        -- particular datum.
      , llSolLocationsOfData :: [ArrayIndex]
        -- ^ Specifies the location of a particular datum. An index into the
        -- list corresponds to the array index of a particular datum, but this
        -- value is only valid if the corresponding datum in
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
      , ai2OperandIDs :: [OperandID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the operand IDs.
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

instance FromJSON HighLevelModelNoOp where
  parseJSON (Object v) =
    HighLevelModelNoOp
       <$> v .: "function-params"
       <*> v .: "machine-params"
       <*> v .: "match-params-no-op"
  parseJSON _ = mzero

instance ToJSON HighLevelModelNoOp where
  toJSON m@(HighLevelModelNoOp {}) =
    object [ "function-params"    .= (hlNoOpFunctionParams m)
           , "machine-params"     .= (hlNoOpMachineParams m)
           , "match-params-no-op" .= (hlNoOpMatchParams m)
           ]

instance FromJSON HighLevelModelWOp where
  parseJSON (Object v) =
    HighLevelModelWOp
      <$> v .: "function-params"
      <*> v .: "machine-params"
      <*> v .: "match-params-w-op"
  parseJSON _ = mzero

instance ToJSON HighLevelModelWOp where
  toJSON m@(HighLevelModelWOp {}) =
    object [ "function-params"   .= (hlWOpFunctionParams m)
           , "machine-params"    .= (hlWOpMachineParams m)
           , "match-params-w-op" .= (hlWOpMatchParams m)
           ]

instance FromJSON HighLevelFunctionParams where
  parseJSON (Object v) =
    HighLevelFunctionParams
      <$> v .: "operations"
      <*> v .: "copies"
      <*> v .: "data"
      <*> v .: "states"
      <*> v .: "blocks"
      <*> v .: "entry-block"
      <*> v .: "block-dom-sets"
      <*> v .: "block-params"
      <*> v .: "state-def-edges"
      <*> v .: "valid-value-locs"
      <*> v .: "int-constant-data"
      <*> v .: "value-origin-data"
      <*> v .: "call-name-data"
      <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON HighLevelFunctionParams where
  toJSON p =
    object [ "operations"               .= (hlFunOperations p)
           , "copies"                   .= (hlFunCopies p)
           , "data"                     .= (hlFunData p)
           , "states"                   .= (hlFunStates p)
           , "blocks"                   .= (hlFunBlocks p)
           , "entry-block"              .= (hlFunEntryBlock p)
           , "block-dom-sets"           .= (hlFunBlockDomSets p)
           , "block-params"             .= (hlFunBlockParams p)
           , "state-def-edges"          .= (hlFunStateDefEdges p)
           , "valid-value-locs"         .= (hlFunValidValueLocs p)
           , "int-constant-data"        .= (hlFunValueIntConstData p)
           , "value-origin-data"        .= (hlFunValueOriginData p)
           , "call-name-data"           .= (hlFunCallNameData p)
           , "constraints"              .= (hlFunConstraints p)
           ]

instance FromJSON HighLevelBlockParams where
  parseJSON (Object v) =
    HighLevelBlockParams
      <$> v .: "block-name"
      <*> v .: "block-node"
      <*> v .: "exec-frequency"
  parseJSON _ = mzero

instance ToJSON HighLevelBlockParams where
  toJSON p =
    object [ "block-name"     .= (hlBlockName p)
           , "block-node"     .= (hlBlockNode p)
           , "exec-frequency" .= (hlBlockExecFrequency p)
           ]

instance FromJSON HighLevelMatchParamsNoOp where
  parseJSON (Object v) =
    HighLevelMatchParamsNoOp
      <$> v .: "instruction-id"
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "operations-covered"
      <*> v .: "data-defined"
      <*> v .: "data-used"
      <*> v .: "external-data"
      <*> v .: "internal-data"
      <*> v .: "valid-value-locs"
      <*> v .: "entry-block"
      <*> v .: "spanned-blocks"
      <*> v .: "consumed-blocks"
      <*> v .: "code-size"
      <*> v .: "latency"
      <*> v .: "constraints"
      <*> v .: "is-phi-instr"
      <*> v .: "is-copy-instr"
      <*> v .: "is-inactive-instr"
      <*> v .: "is-null-instr"
      <*> v .: "has-control-flow"
      <*> v .: "data-used-by-phis"
      <*> v .: "data-defined-by-phis"
      <*> v .: "emit-str-node-maps"
  parseJSON _ = mzero

instance FromJSON HighLevelMatchParamsWOp where
  parseJSON (Object v) =
    HighLevelMatchParamsWOp
      <$> v .: "instruction-id"
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "operand-node-maps"
      <*> v .: "operations-covered"
      <*> v .: "operands-defined"
      <*> v .: "operands-used"
      <*> v .: "external-operands"
      <*> v .: "internal-operands"
      <*> v .: "valid-value-locs"
      <*> v .: "entry-block"
      <*> v .: "spanned-blocks"
      <*> v .: "consumed-blocks"
      <*> v .: "code-size"
      <*> v .: "latency"
      <*> v .: "constraints"
      <*> v .: "is-phi-instr"
      <*> v .: "is-copy-instr"
      <*> v .: "is-inactive-instr"
      <*> v .: "is-null-instr"
      <*> v .: "has-control-flow"
      <*> v .: "operands-used-by-phis"
      <*> v .: "operands-defined-by-phis"
      <*> v .: "emit-str-node-maps"
  parseJSON _ = mzero

instance ToJSON HighLevelMatchParamsNoOp where
  toJSON p@(HighLevelMatchParamsNoOp {}) =
    object [ "instruction-id"       .= (hlNoOpMatchInstructionID p)
           , "pattern-id"           .= (hlNoOpMatchPatternID p)
           , "match-id"             .= (hlNoOpMatchID p)
           , "operations-covered"   .= (hlNoOpMatchOperationsCovered p)
           , "data-defined"         .= (hlNoOpMatchDataDefined p)
           , "data-used"            .= (hlNoOpMatchDataUsed p)
           , "external-data"        .= (hlNoOpMatchExternalData p)
           , "internal-data"        .= (hlNoOpMatchInternalData p)
           , "valid-value-locs"     .= (hlNoOpMatchValidValueLocs p)
           , "entry-block"          .= (hlNoOpMatchEntryBlock p)
           , "spanned-blocks"       .= (hlNoOpMatchSpannedBlocks p)
           , "consumed-blocks"      .= (hlNoOpMatchConsumedBlocks p)
           , "code-size"            .= (hlNoOpMatchCodeSize p)
           , "latency"              .= (hlNoOpMatchLatency p)
           , "constraints"          .= (hlNoOpMatchConstraints p)
           , "is-phi-instr"         .= (hlNoOpMatchIsPhiInstruction p)
           , "is-copy-instr"        .= (hlNoOpMatchIsCopyInstruction p)
           , "is-inactive-instr"    .= (hlNoOpMatchIsInactiveInstruction p)
           , "is-null-instr"        .= (hlNoOpMatchIsNullInstruction p)
           , "has-control-flow"     .= (hlNoOpMatchHasControlFlow p)
           , "data-used-by-phis"    .= (hlNoOpMatchDataUsedByPhis p)
           , "data-defined-by-phis" .= (hlNoOpMatchDataDefinedByPhis p)
           , "emit-str-node-maps"   .= (hlNoOpMatchEmitStrNodeMaplist p)
           ]

instance ToJSON HighLevelMatchParamsWOp where
  toJSON p@(HighLevelMatchParamsWOp {}) =
    object [ "instruction-id"           .= (hlWOpMatchInstructionID p)
           , "pattern-id"               .= (hlWOpMatchPatternID p)
           , "match-id"                 .= (hlWOpMatchID p)
           , "operand-node-maps"        .= (hlWOpOperandNodeMaps p)
           , "operations-covered"       .= (hlWOpMatchOperationsCovered p)
           , "operands-defined"         .= (hlWOpMatchOperandsDefined p)
           , "operands-used"            .= (hlWOpMatchOperandsUsed p)
           , "external-operands"        .= (hlWOpMatchExternalOperands p)
           , "internal-operands"        .= (hlWOpMatchInternalOperands p)
           , "valid-value-locs"         .= (hlWOpMatchValidValueLocs p)
           , "entry-block"              .= (hlWOpMatchEntryBlock p)
           , "spanned-blocks"           .= (hlWOpMatchSpannedBlocks p)
           , "consumed-blocks"          .= (hlWOpMatchConsumedBlocks p)
           , "code-size"                .= (hlWOpMatchCodeSize p)
           , "latency"                  .= (hlWOpMatchLatency p)
           , "constraints"              .= (hlWOpMatchConstraints p)
           , "is-phi-instr"             .= (hlWOpMatchIsPhiInstruction p)
           , "is-copy-instr"            .= (hlWOpMatchIsCopyInstruction p)
           , "is-inactive-instr"        .= (hlWOpMatchIsInactiveInstruction p)
           , "is-null-instr"            .= (hlWOpMatchIsNullInstruction p)
           , "has-control-flow"         .= (hlWOpMatchHasControlFlow p)
           , "operands-used-by-phis"    .= (hlWOpMatchOperandsUsedByPhis p)
           , "operands-defined-by-phis" .= (hlWOpMatchOperandsDefinedByPhis p)
           , "emit-str-node-maps"       .= (hlWOpMatchEmitStrNodeMaplist p)
           ]

instance FromJSON HighLevelMachineParams where
  parseJSON (Object v) =
    HighLevelMachineParams
      <$> v .: "target-machine-id"
      <*> v .: "locations"
  parseJSON _ = mzero

instance ToJSON HighLevelMachineParams where
  toJSON p =
    object [ "target-machine-id" .= (hlMachineID p)
           , "locations"         .= (hlMachineLocations p)
           ]

instance FromJSON LowLevelModel where
  parseJSON (Object v) =
    LowLevelModel
      <$> v .: "fun-num-operations"
      <*> v .: "fun-num-data"
      <*> v .: "fun-num-blocks"
      <*> v .: "fun-copies"
      <*> v .: "fun-states"
      <*> v .: "fun-valid-value-locs"
      <*> v .: "fun-entry-block"
      <*> v .: "fun-block-dom-sets"
      <*> v .: "fun-block-exec-freqs"
      <*> v .: "fun-state-def-edges"
      <*> v .: "fun-constraints"
      <*> v .: "num-locations"
      <*> v .: "num-matches"
      <*> v .: "num-operands"
      <*> v .: "operand-alternatives"
      <*> v .: "match-operations-covered"
      <*> v .: "match-operands-defined"
      <*> v .: "match-operands-used"
      <*> v .: "match-external-operands"
      <*> v .: "match-internal-operands"
      <*> v .: "match-valid-value-locs"
      <*> v .: "match-entry-blocks"
      <*> v .: "match-spanned-blocks"
      <*> v .: "match-consumed-blocks"
      <*> v .: "match-input-def-edges"
      <*> v .: "match-output-def-edges"
      <*> v .: "match-code-sizes"
      <*> v .: "match-latencies"
      <*> v .: "match-copy-instrs"
      <*> v .: "match-inactive-instrs"
      <*> v .: "match-null-instrs"
      <*> v .: "match-phi-instrs"
      <*> v .: "match-constraints"
  parseJSON _ = mzero

instance ToJSON LowLevelModel where
  toJSON m =
    object [ "fun-num-operations"       .= (llFunNumOperations m)
           , "fun-num-data"             .= (llFunNumData m)
           , "fun-num-blocks"           .= (llFunNumBlocks m)
           , "fun-copies"               .= (llFunCopies m)
           , "fun-states"               .= (llFunStates m)
           , "fun-valid-value-locs"     .= (llFunValidValueLocs m)
           , "fun-entry-block"          .= (llFunEntryBlock m)
           , "fun-block-dom-sets"       .= (llFunBlockDomSets m)
           , "fun-block-exec-freqs"     .= (llFunBBExecFreqs m)
           , "fun-state-def-edges"      .= (llFunStateDefEdges m)
           , "fun-constraints"          .= (llFunConstraints m)
           , "num-locations"            .= (llNumLocations m)
           , "num-matches"              .= (llNumMatches m)
           , "num-operands"             .= (llNumOperands m)
           , "operand-alternatives"     .= (llOperandAlternatives m)
           , "match-operations-covered" .= (llMatchOperationsCovered m)
           , "match-operands-defined"   .= (llMatchOperandsDefined m)
           , "match-operands-used"      .= (llMatchOperandsUsed m)
           , "match-external-operands"  .= (llMatchExternalOperands m)
           , "match-internal-operands"  .= (llMatchInternalOperands m)
           , "match-valid-value-locs"   .= (llMatchValidValueLocs m)
           , "match-entry-blocks"       .= (llMatchEntryBlocks m)
           , "match-spanned-blocks"     .= (llMatchSpannedBlocks m)
           , "match-consumed-blocks"    .= (llMatchConsumedBlocks m)
           , "match-input-def-edges"    .= (llMatchInputDefinitionEdges m)
           , "match-output-def-edges"   .= (llMatchOutputDefinitionEdges m)
           , "match-code-sizes"         .= (llMatchCodeSizes m)
           , "match-latencies"          .= (llMatchLatencies m)
           , "match-copy-instrs"        .= (llMatchCopyInstructions m)
           , "match-inactive-instrs"    .= (llMatchInactiveInstructions m)
           , "match-null-instrs"        .= (llMatchNullInstructions m)
           , "match-phi-instrs"         .= (llMatchPhiInstructions m)
           , "match-constraints"        .= (llMatchConstraints m)
           ]

instance FromJSON HighLevelSolution where
  parseJSON (Object v) =
    do has_solution <- v .: "has-solution"
       if has_solution
       then HighLevelSolution
              <$> v .:  "order-of-blocks"
              <*> v .:  "selected-matches"
              <*> v .:  "nodes-of-operands"
              <*> v .:  "blocks-of-sel-matches"
              <*> v .:  "locs-of-data"
              <*> v .:  "cost"
              <*> v .:  "is-solution-optimal"
              <*> v .:? "time"
              <*> v .:? "core-time"
       else return NoHighLevelSolution
  parseJSON _ = mzero

instance ToJSON HighLevelSolution where
  toJSON s@(HighLevelSolution {}) =
    object [ "order-of-blocks"       .= (hlSolOrderOfBlocks s)
           , "selected-matches"      .= (hlSolSelMatches s)
           , "nodes-of-operands"     .= (hlSolNodesOfOperands s)
           , "blocks-of-sel-matches" .= (hlSolBlocksOfSelMatches s)
           , "locs-of-data"          .= (hlSolLocationsOfData s)
           , "cost"                  .= (hlSolCost s)
           , "has-solution"          .= True
           , "is-solution-optimal"   .= (hlIsOptimal s)
           , "time"                  .= (hlSolTime s)
           , "core-time"             .= (hlCoreSolTime s)
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
              <*> v .:  "has-operand-alt"
              <*> v .:  "alt-of-operand"
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
  toJSON s =
    object [ "array-index-to-op-node-id-maps"     .= (ai2OperationNodeIDs s)
           , "array-index-to-datum-node-id-maps"  .= (ai2DatumNodeIDs s)
           , "array-index-to-operand-id-maps"     .= (ai2OperandIDs s)
           , "array-index-to-block-node-id-maps"  .= (ai2BlockNodeIDs s)
           , "array-index-to-match-id-maps"       .= (ai2MatchIDs s)
           , "array-index-to-location-id-maps"    .= (ai2LocationIDs s)
           , "array-index-to-instruction-id-maps" .= (ai2InstructionIDs s)
           ]

instance FromJSON ArrayIndexMaplists where
  parseJSON (Object v) =
    ArrayIndexMaplists
      <$> v .: "array-index-to-op-node-id-maps"
      <*> v .: "array-index-to-datum-node-id-maps"
      <*> v .: "array-index-to-operand-id-maps"
      <*> v .: "array-index-to-block-node-id-maps"
      <*> v .: "array-index-to-match-id-maps"
      <*> v .: "array-index-to-location-id-maps"
      <*> v .: "array-index-to-instruction-id-maps"
  parseJSON _ = mzero
