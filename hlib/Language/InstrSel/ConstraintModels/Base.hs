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
      { hlFunOpNodes :: [NodeID]
        -- ^ The operation nodes in the function graph.
      , hlFunEntityNodes :: [NodeID]
        -- ^ The entity nodes in the function graph.
      , hlFunStateNodes :: [NodeID]
        -- ^ The state nodes in the function graph.
      , hlFunBlockNodes :: [NodeID]
        -- ^ The block nodes in the function graph.
      , hlFunEntryBlockNode :: NodeID
        -- ^ The block that is the entry point into the function.
      , hlFunBlockDomSets :: [DomSet NodeID]
        -- ^ The dominator sets of the block nodes in the function graph.
      , hlFunDefEdges :: [(NodeID, NodeID)]
        -- ^ The definition edges in the function graph. The first element is
        -- assumed to always be a block node and the second element is assumed
        -- to always be an entity node.
      , hlFunBlockParams :: [HighLevelBlockParams]
        -- ^ The block information.
      , hlFunIntConstData :: [(NodeID, Integer)]
        -- ^ The value nodes representing integer constants and their values.
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
        -- ^ The node ID of the block node that represents this block.
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
      , hlMatchOpNodesCovered :: [NodeID]
        -- ^ The operations in the function graph which are covered by this
        -- match.
      , hlMatchEntityNodesDefined :: [NodeID]
        -- ^ The entities in the function graph which are defined by this match.
      , hlMatchEntityNodesUsed :: [NodeID]
        -- ^ The entities in the function graph which are used by this
        -- match.
      , hlMatchEntryBlockNode :: Maybe NodeID
        -- ^ The block node in the function graph that appears as entry block
        -- (if there is such a node) in this match.
      , hlMatchNonEntryBlockNodes :: [NodeID]
        -- ^ The block nodes in the function graph that appears in this match
        -- but not as entries.
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
      , hlMatchIsNonCopyInstruction :: Bool
        -- ^ Whether the corresponding instruction is a non-copy instruction.
      , hlMatchHasControlNodes :: Bool
        -- ^ Whether the corresponding pattern contains one or more
        -- control nodes.
      , hlMatchValueNodesUsedByPhis :: [NodeID]
        -- ^ The value nodes in the function graph which are used by phi nodes
        -- appearing this match. This information is required during instruction
        -- emission in order to break cyclic data dependencies.
      , hlMatchAsmStrNodeMaplist :: [Maybe NodeID]
        -- ^ A list of mappings of the node IDs that appears in the assembly
        -- instruction template (which refer to nodes in the pattern graph) to
        -- the node IDs in the function graph which are covered by this
        -- pattern. The map list has the following appearance: each element in
        -- the list corresponds to a assembly string part with the same index in
        -- the assembly instruction template.
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
      { llNumFunOpNodes :: Integer
        -- ^ The number of operation nodes appearing in the function graph.
      , llNumFunEntityNodes :: Integer
        -- ^ The number of entity nodes appearing in the function graph.
      , llNumFunBlockNodes :: Integer
        -- ^ The number of block nodes appearing in the function graph.
      , llFunStateNodes :: [ArrayIndex]
        -- ^ The entities that are state nodes of the function graph.
      , llFunEntryBlockNode :: ArrayIndex
        -- ^ The entry block node of the function graph.
      , llFunBlockDomSets :: [[ArrayIndex]]
        -- ^ The dominator set for each block node in the function graph.
        -- An index into the outer list corresponds to the array index of a
        -- particular block node.
      , llFunDefEdges :: [[ArrayIndex]]
        -- ^ The list of entity nodes for each block node in the function graph
        -- between which there is a definition edge. An index into the outer
        -- list corresponds to the array index of a particular block node.
      , llFunBBExecFreqs :: [ExecFreq]
        -- ^ The execution frequency of each block. An index into the list
        -- corresponds to the array index of a particular block node in the
        -- function graph.
      , llFunConstraints :: [Constraint]
        -- ^ The constraints of the function graph. No constraint in this list
        -- may use IDs.
      , llNumLocations :: Integer
        -- ^ The number of locations available in the target machine.
      , llNumMatches :: Integer
        -- ^ The number of matches.
      , llMatchOpNodesCovered :: [[ArrayIndex]]
        -- ^ The list of operation nodes in the function graph that are covered
        -- by each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchEntityNodesDefined :: [[ArrayIndex]]
        -- ^ The list of entity nodes in the function graph that are defined by
        -- each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchEntityNodesUsed :: [[ArrayIndex]]
        -- ^ The list of entity nodes in the function graph that are used by
        -- each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchEntryBlockNode :: [Maybe ArrayIndex]
        -- ^ The block node in the function graph that is the entry block (if
        -- any) of each match. An index into the list corresponds to the array
        -- index of a particular match.
      , llMatchNonEntryBlockNodes :: [[ArrayIndex]]
        -- ^ The block nodes in the function graph that are non-entry blocks of
        -- each match. An index into the outer list corresponds to the array
        -- index of a particular match.
      , llMatchCodeSizes :: [Integer]
        -- ^ The code size of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchLatencies :: [Integer]
        -- ^ The latency of each match. An index into the list corresponds to
        -- the array index of a particular match.
      , llMatchNonCopyInstructions :: [ArrayIndex]
        -- ^ The matches that correspond to non-copy instructions.
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
      { hlSolOrderOfBBs :: [NodeID]
        -- ^ The order of blocks (represented by the node ID of the
        -- corresponding block node).
      , hlSolSelMatches :: [MatchID]
        -- ^ The selected matchs.
      , hlSolBBsOfSelMatches :: [(MatchID, NodeID)]
        -- ^ The block (represented by the node ID of the corresponding
        -- block node) to which a particular match was moved. A missing entry
        -- means that the corresponding match ID was not selected and thus not
        -- moved to a valid block.
      , hlSolLocsOfValueNodes :: [(NodeID, LocationID)]
        -- ^ The locations assigned for certain value nodes. A missing entry
        -- means that no location was assigned to the corresponding value node.
      , hlSolCost :: Integer
        -- ^ The cost metric of the found solution.
      , hlIsOptimal :: Bool
        -- ^ Whether this solution is optimal.
      }
  | NoHighLevelSolution
  deriving (Show)

-- | Contains a solution to a low-level CP model instance.
data LowLevelSolution
  = LowLevelSolution
      { llSolOrderOfBBs :: [ArrayIndex]
        -- ^ The order of blocks. An index into the list corresponds to
        -- the array index of the block node in the function graph which
        -- represents a particular block.
      , llSolIsMatchSelected :: [Bool]
        -- ^ Indicates whether a particular match was selected. An index into
        -- the list corresponds to the array index of a particular match.
      , llSolBBsOfMatches :: [ArrayIndex]
        -- ^ The array index of the block to which a particular match was
        -- moved. An index into the list corresponds to the array index of a
        -- particular match, but this value is only valid if the corresponding
        -- value in @llIsMatchSelected@ is set to @True@.
      , llSolHasValueNodeLocation :: [Bool]
        -- ^ Indicates whether a location has been selected for a particular
        -- value node. An index into the list corresponds to the array index of
        -- a particular value node.
      , llSolLocsOfValueNodes :: [ArrayIndex]
        -- ^ Specifies the location of a particular value node. An index into
        -- the list corresponds to the array index of a particular value node,
        -- but this value is only valid if the corresponding value in
        -- @llHasValueNodeLocation@ is set to @True@.
      , llSolCost :: Integer
        -- ^ The cost metric of the found solution.
      , llIsOptimal :: Bool
        -- ^ Whether this solution is optimal.
      }
  | NoLowLevelSolution
  deriving (Show)

-- | Contains mappings from an array index to some ID. This is used when
-- generating the CP model instance, where we want all identifiers to be array
-- indices, which must be contiguous, instead of node IDs, match IDs, location
-- IDs, etc., which may be sparse.
data ArrayIndexMaplists
  = ArrayIndexMaplists
      { ai2OpNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of operation nodes.
      , ai2EntityNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of entity nodes.
      , ai2BlockNodeIDs :: [NodeID]
        -- ^ The list of mappings from array indices (represented as list
        -- indices) to the node IDs of block nodes.
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
      <$> v .: "operation-nodes"
      <*> v .: "entity-nodes"
      <*> v .: "state-nodes"
      <*> v .: "block-nodes"
      <*> v .: "entry-block"
      <*> v .: "block-dom-sets"
      <*> v .: "def-edges"
      <*> v .: "bb-params"
      <*> v .: "int-constant-data"
      <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON HighLevelFunctionParams where
  toJSON d =
    object [ "operation-nodes"   .= (hlFunOpNodes d)
           , "entity-nodes"      .= (hlFunEntityNodes d)
           , "state-nodes"       .= (hlFunStateNodes d)
           , "block-nodes"       .= (hlFunBlockNodes d)
           , "entry-block"       .= (hlFunEntryBlockNode d)
           , "block-dom-sets"    .= (hlFunBlockDomSets d)
           , "def-edges"         .= (hlFunDefEdges d)
           , "bb-params"         .= (hlFunBlockParams d)
           , "int-constant-data" .= (hlFunIntConstData d)
           , "constraints"       .= (hlFunConstraints d)
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
      <*> v .: "operation-nodes-covered"
      <*> v .: "entity-nodes-defined"
      <*> v .: "entity-nodes-used"
      <*> v .: "entry-block-node"
      <*> v .: "non-entry-block-nodes"
      <*> v .: "code-size"
      <*> v .: "latency"
      <*> v .: "constraints"
      <*> v .: "apply-def-dom-use-constraint"
      <*> v .: "is-non-copy-instr"
      <*> v .: "has-control-nodes"
      <*> v .: "value-nodes-used-by-phis"
      <*> v .: "asm-str-node-maps"
  parseJSON _ = mzero

instance ToJSON HighLevelMatchParams where
  toJSON d =
    object [ "instruction-id"               .= (hlMatchInstructionID d)
           , "pattern-id"                   .= (hlMatchPatternID d)
           , "match-id"                     .= (hlMatchID d)
           , "operation-nodes-covered"      .= (hlMatchOpNodesCovered d)
           , "entity-nodes-defined"         .= (hlMatchEntityNodesDefined d)
           , "entity-nodes-used"            .= (hlMatchEntityNodesUsed d)
           , "entry-block-node"             .= (hlMatchEntryBlockNode d)
           , "non-entry-block-nodes"        .= (hlMatchNonEntryBlockNodes d)
           , "code-size"                    .= (hlMatchCodeSize d)
           , "latency"                      .= (hlMatchLatency d)
           , "constraints"                  .= (hlMatchConstraints d)
           , "apply-def-dom-use-constraint" .= (hlMatchADDUC d)
           , "is-non-copy-instr"            .= (hlMatchIsNonCopyInstruction d)
           , "has-control-nodes"            .= (hlMatchHasControlNodes d)
           , "value-nodes-used-by-phis"     .= (hlMatchValueNodesUsedByPhis d)
           , "asm-str-node-maps"            .= (hlMatchAsmStrNodeMaplist d)
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
      <$> v .: "fun-num-op-nodes"
      <*> v .: "fun-num-entity-nodes"
      <*> v .: "fun-num-block-nodes"
      <*> v .: "fun-state-nodes"
      <*> v .: "fun-entry-block-node"
      <*> v .: "fun-block-dom-sets"
      <*> v .: "fun-def-edges"
      <*> v .: "fun-bb-exec-freqs"
      <*> v .: "fun-constraints"
      <*> v .: "num-locations"
      <*> v .: "num-matches"
      <*> v .: "match-op-nodes-covered"
      <*> v .: "match-entity-nodes-defined"
      <*> v .: "match-entity-nodes-used"
      <*> v .: "match-entry-block-nodes"
      <*> v .: "match-non-entry-block-nodes"
      <*> v .: "match-code-sizes"
      <*> v .: "match-latencies"
      <*> v .: "match-non-copy-instrs"
      <*> v .: "match-adduc-settings"
      <*> v .: "match-constraints"
  parseJSON _ = mzero

instance ToJSON LowLevelModel where
  toJSON m =
    object [ "fun-num-op-nodes"            .= (llNumFunOpNodes m)
           , "fun-num-entity-nodes"        .= (llNumFunEntityNodes m)
           , "fun-num-block-nodes"         .= (llNumFunBlockNodes m)
           , "fun-state-nodes"             .= (llFunStateNodes m)
           , "fun-entry-block-node"        .= (llFunEntryBlockNode m)
           , "fun-block-dom-sets"          .= (llFunBlockDomSets m)
           , "fun-def-edges"               .= (llFunDefEdges m)
           , "fun-bb-exec-freqs"           .= (llFunBBExecFreqs m)
           , "fun-constraints"             .= (llFunConstraints m)
           , "num-locations"               .= (llNumLocations m)
           , "num-matches"                 .= (llNumMatches m)
           , "match-op-nodes-covered"      .= (llMatchOpNodesCovered m)
           , "match-entity-nodes-defined"  .= (llMatchEntityNodesDefined m)
           , "match-entity-nodes-used"     .= (llMatchEntityNodesUsed m)
           , "match-entry-block-nodes"     .= (llMatchEntryBlockNode m)
           , "match-non-entry-block-nodes" .= (llMatchNonEntryBlockNodes m)
           , "match-code-sizes"            .= (llMatchCodeSizes m)
           , "match-latencies"             .= (llMatchLatencies m)
           , "match-non-copy-instrs"       .= (llMatchNonCopyInstructions m)
           , "match-adduc-settings"        .= (llMatchADDUCs m)
           , "match-constraints"           .= (llMatchConstraints m)
           ]

instance FromJSON HighLevelSolution where
  parseJSON (Object v) =
    do has_solution <- v .: "has-solution"
       if has_solution
       then HighLevelSolution
              <$> v .: "order-of-bbs"
              <*> v .: "selected-matches"
              <*> v .: "bbs-of-sel-matches"
              <*> v .: "locs-of-dnodes"
              <*> v .: "cost"
              <*> v .: "is-solution-optimal"
       else return NoHighLevelSolution
  parseJSON _ = mzero

instance ToJSON HighLevelSolution where
  toJSON d@(HighLevelSolution {}) =
    object [ "order-of-bbs"         .= (hlSolOrderOfBBs d)
           , "selected-matches"     .= (hlSolSelMatches d)
           , "bbs-of-sel-matches"   .= (hlSolBBsOfSelMatches d)
           , "locs-of-dnodes"       .= (hlSolLocsOfValueNodes d)
           , "cost"                 .= (hlSolCost d)
           , "has-solution"         .= True
           , "is-solution-optimal"  .= (hlIsOptimal d)
           ]
  toJSON NoHighLevelSolution =
    object [ "has-solution" .= False ]

instance FromJSON LowLevelSolution where
  parseJSON (Object v) =
    do has_solution <- v .: "has-solution"
       if has_solution
       then LowLevelSolution
              <$> v .: "order-of-bbs"
              <*> v .: "is-match-selected"
              <*> v .: "bb-of-match"
              <*> v .: "has-dnode-loc"
              <*> v .: "loc-of-dnode"
              <*> v .: "cost"
              <*> v .: "is-solution-optimal"
       else return NoLowLevelSolution
  parseJSON _ = mzero

instance ToJSON ArrayIndexMaplists where
  toJSON d =
    object [ "array-index-to-op-node-id-maps"     .= (ai2OpNodeIDs d)
           , "array-index-to-entity-node-id-maps" .= (ai2EntityNodeIDs d)
           , "array-index-to-block-node-id-maps"  .= (ai2BlockNodeIDs d)
           , "array-index-to-match-id-maps"       .= (ai2MatchIDs d)
           , "array-index-to-location-id-maps"    .= (ai2LocationIDs d)
           , "array-index-to-instruction-id-maps" .= (ai2InstructionIDs d)
           ]

instance FromJSON ArrayIndexMaplists where
  parseJSON (Object v) =
    ArrayIndexMaplists
      <$> v .: "array-index-to-op-node-id-maps"
      <*> v .: "array-index-to-entity-node-id-maps"
      <*> v .: "array-index-to-block-node-id-maps"
      <*> v .: "array-index-to-match-id-maps"
      <*> v .: "array-index-to-location-id-maps"
      <*> v .: "array-index-to-instruction-id-maps"
  parseJSON _ = mzero
