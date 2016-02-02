--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstrSel.TargetMachines.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing a target machine.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstrSel.TargetMachines.Base
  ( module Language.InstrSel.Graphs.IDs
  , module Language.InstrSel.TargetMachines.IDs
  , AssemblyStringTemplate (..)
  , AssemblyStringPart (..)
  , Instruction (..)
  , InstrPattern (..)
  , InstrProperties (..)
  , Location (..)
  , TargetMachine (..)
  , findInstruction
  , findInstrPattern
  , findLocation
  , flattenAsmStrParts
  , updateNodeInAsmStrTemplate
  )
where

import Language.InstrSel.Graphs.IDs
  ( NodeID )
import Language.InstrSel.OpStructures
import Language.InstrSel.TargetMachines.IDs



--------------
-- Data types
--------------

-- | Represents the assembly string template, which are used to produce the
-- assembly instructions during code emission.
data AssemblyStringTemplate
  = ASSTemplate { asmStrParts :: [AssemblyStringPart] }
  | ASSMultiTemplate { asmStrTemplates :: [AssemblyStringTemplate] }
  deriving (Show)

-- | Represents parts of the assembly string template.
data AssemblyStringPart
    -- | Denotes string which is meant to be output verbatim.
  = ASVerbatim String
    -- | Denotes the integer constant of a given value node.
  | ASIntConstOfValueNode NodeID
    -- | Denotes the location assigned to a given value node.
  | ASLocationOfValueNode NodeID
    -- | Denotes the name a given block node.
  | ASNameOfBlockNode NodeID
    -- | Denotes the block in which the definer of a given value node has been
    -- placed.
  | ASBlockOfValueNode NodeID
  deriving (Show)

-- | Defines a machine instruction.
data Instruction
  = Instruction
      { instrID :: InstructionID
        -- ^ The ID of this instruction. The ID must be globally unique across
        -- all instructions, but not necessarily contiguous.
      , instrPatterns :: [InstrPattern]
        -- ^ Patterns which correspond to the instruction. There must be at
        -- least one pattern. Each pattern also has a corresponding ID which
        -- must be globally unique across all patterns and all instructions, but
        -- not necessarily contiguous.
      , instrProps :: InstrProperties
        -- ^ Instruction properties.
      }
  deriving (Show)

-- | Contains the various properties of an instruction, such as code size and
-- latency.
data InstrProperties
  = InstrProperties
      { instrCodeSize :: Integer
        -- ^ Instruction code size (in bytes).
      , instrLatency :: Integer
        -- ^ Instruction latency (in cycles).
      , instrIsNonCopy :: Bool
        -- ^ Whether the instruction is a non-copy instruction.
      , instrIsNullCopy :: Bool
        -- ^ Whether the instruction is a null-copy instruction.
      }
  deriving (Show)

-- | Defines a pattern for a machine instruction.
data InstrPattern
  = InstrPattern
      { patID :: PatternID
        -- ^ The ID of this pattern. The ID must be unique within the same
        -- instruction, but not necessarily contiguous.
      , patOS :: OpStructure
        -- ^ The operation structure of the pattern.
      , patADDUC :: Bool
        -- ^ Indicates whether the def-dom-use constraints apply to this
        -- pattern. This will typically always be set to 'True' for all patterns
        -- except the generic phi patterns.
      , patAsmStrTemplate :: AssemblyStringTemplate
        -- ^ The assembly string template, from which the assembly instruction
        -- will be produced upon code emission if this pattern is selected.
      }
  deriving (Show)

-- | Represents a target machine.
data TargetMachine
  = TargetMachine
      { tmID :: TargetMachineID
        -- ^ The identifier of the target machine.
      , tmInstructions :: [Instruction]
        -- ^ The set of assembly instructions supported by the target machine.
      , tmLocations :: [Location]
        -- ^ The machine locations, given as pairs of location IDs and location
        -- names (which are needed during instruction emission). Each must be
        -- given a unique location ID, but not necessarily in a contiguous
        -- order.
      }
  deriving (Show)

-- | Represents a machine location.
data Location
  = Location
      { locID :: LocationID
        -- ^ The ID of this location. This must be unique for every location
        -- within the same target machine.
      , locName :: LocationName
        -- ^ The name of this location (as it shall appear in the assembly
        -- string).
      , locValue :: Maybe Integer
        -- ^ The fixed value, if any, represented by this location (such as a
        -- register that always contains the value zero).
      }
  deriving (Show, Eq)



-------------
-- Functions
-------------

-- | Given a list of instructions, the function finds the 'Instruction' entity
-- with matching instruction ID. If there is more than one match, the first
-- found is returned. If no such entity is found, 'Nothing' is returned.
findInstruction :: [Instruction] -> InstructionID -> Maybe Instruction
findInstruction is iid =
  let found = filter (\i -> instrID i == iid) is
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Given a list of instruction patterns, the function finds the 'InstrPattern'
-- entity with matching pattern ID. If there is more than one match, the first
-- found is returned. If no such entity is found, 'Nothing' is returned.
findInstrPattern :: [InstrPattern] -> PatternID -> Maybe InstrPattern
findInstrPattern ps pid =
  let found = filter (\p -> patID p == pid) ps
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Given a list of locations, the function finds the 'Location' with matching
-- location ID. If there is more than one match, the first found is returned. If
-- no such entity is found, 'Nothing' is returned.
findLocation :: [Location] -> LocationID -> Maybe Location
findLocation rs rid =
  let found = filter (\r -> locID r == rid) rs
  in if length found > 0
     then Just $ head found
     else Nothing

-- | Flattens a (potentially multi-level) 'AssemblyStringTemplate'.
flattenAsmStrParts :: AssemblyStringTemplate -> [[AssemblyStringPart]]
flattenAsmStrParts (ASSTemplate []) = []
flattenAsmStrParts (ASSTemplate asps) = [asps]
flattenAsmStrParts (ASSMultiTemplate ats) = map (head . flattenAsmStrParts) ats

-- | Replaces a node reference used in the template with another reference.
updateNodeInAsmStrTemplate
  :: NodeID
     -- ^ The new node ID.
  -> NodeID
     -- ^ The old node ID to be replaced.
  -> AssemblyStringTemplate
  -> AssemblyStringTemplate
updateNodeInAsmStrTemplate new_n old_n (ASSMultiTemplate ts) =
  ASSMultiTemplate (map (updateNodeInAsmStrTemplate new_n old_n) ts)
updateNodeInAsmStrTemplate new_n old_n (ASSTemplate parts) =
  ASSTemplate (map update parts)
  where checkAndReplace nid = if nid == old_n then new_n else nid
        update (ASIntConstOfValueNode n) =
          ASIntConstOfValueNode (checkAndReplace n)
        update (ASLocationOfValueNode n) =
          ASLocationOfValueNode (checkAndReplace n)
        update (ASNameOfBlockNode n) =
          ASNameOfBlockNode (checkAndReplace n)
        update (ASBlockOfValueNode n) =
          ASBlockOfValueNode (checkAndReplace n)
        update p = p
