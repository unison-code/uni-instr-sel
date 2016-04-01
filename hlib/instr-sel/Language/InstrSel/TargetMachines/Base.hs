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
  , EmitStringTemplate (..)
  , EmitStringPart (..)
  , Instruction (..)
  , InstrPattern (..)
  , InstrProperties (..)
  , Location (..)
  , TargetMachine (..)
  , findInstruction
  , findInstrPattern
  , findLocation
  , updateNodeInEmitStrTemplate
  )
where

import Language.InstrSel.Graphs.IDs
  ( NodeID )
import Language.InstrSel.OpStructures
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.IDs



--------------
-- Data types
--------------

-- | Represents the emit string template, which are used to produce the assembly
-- instructions during code emission. Each element in the outer list corresponds
-- to a single line of code.
data EmitStringTemplate
  = EmitStringTemplate { emitStrParts :: [[EmitStringPart]] }
  deriving (Show)

-- | Represents parts of the emit string template.
data EmitStringPart
    -- | Denotes string which is meant to be output verbatim.
  = ESVerbatim String
    -- | Denotes the integer constant of a given value node.
  | ESIntConstOfValueNode NodeID
    -- | Denotes the location assigned to a given value node.
  | ESLocationOfValueNode NodeID
    -- | Denotes the name a given block node.
  | ESNameOfBlockNode NodeID
    -- | Denotes the block in which the definer of a given value node has been
    -- placed.
  | ESBlockOfValueNode NodeID
    -- | Upon code emission, this will be replaced by a new, uniquely named
    -- temporary. If multiple 'ESTemporary's with identical identifiers appear
    -- within the same template, they will all be replaced by the same
    -- temporary.
  | ESTemporary Int
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
      , patEmitString :: EmitStringTemplate
        -- ^ The emit string from which the assembly instruction will be
        -- produced upon code emission if this pattern is selected.
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

instance PrettyShow Location where
  pShow (Location lid name val) = "Location (" ++ pShow lid ++ ", "
                                  ++ pShow name ++ ", " ++ pShow val ++ ")"



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

-- | Replaces a node reference used in the template with another reference.
updateNodeInEmitStrTemplate
  :: NodeID
     -- ^ The new node ID.
  -> NodeID
     -- ^ The old node ID to be replaced.
  -> EmitStringTemplate
  -> EmitStringTemplate
updateNodeInEmitStrTemplate new_n old_n (EmitStringTemplate ts) =
  EmitStringTemplate $ map (map update) ts
  where update (ESIntConstOfValueNode n) =
          ESIntConstOfValueNode (checkAndReplace n)
        update (ESLocationOfValueNode n) =
          ESLocationOfValueNode (checkAndReplace n)
        update (ESNameOfBlockNode n) = ESNameOfBlockNode (checkAndReplace n)
        update (ESBlockOfValueNode n) = ESBlockOfValueNode (checkAndReplace n)
        update p = p
        checkAndReplace nid = if nid == old_n then new_n else nid
