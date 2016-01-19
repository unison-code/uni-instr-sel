--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Generators.LLVM.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.TargetMachines.Generators.LLVM.Base where

import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils
  ( Range )



--------------
-- Data types
--------------

-- | Contains a machine description.
data MachineDescription
  = MachineDescription
      { mdInstructions :: [Instruction]
        -- ^ The instructions available on the target machine.
      , mdLocations :: [Location]
        -- ^ The locations available on the target machine.
      }
  deriving (Show)

-- | Contains the information regarding an instruction.
data Instruction
  = Instruction
      { instrAssemblyString :: String
        -- ^ The assembly string of this instruction. Input operands are
        -- prefixed with a @%@.
      , instrSize :: Integer
        -- ^ Instruction code size (in bytes).
      , instrLatency :: Integer
        -- ^ Instruction latency (in cycles).
      , instrOperands :: [InstrOperand]
      , instrSemantics :: [InstrSemantics]
        -- ^ The semantics of this instruction, expressed in LLVM IR.
      }
  deriving (Show)

-- | Contains the information regarding a location.
data Location
  = RegLocation
      { regName :: String
        -- ^ Register name.
      , regValue :: Maybe Integer
        -- ^ Fixed value of this register, if it has any.
      }
  deriving (Show)

-- | Contains the information regarding an instruction operand.
data InstrOperand

  -- | Represents a register operand.
  = RegInstrOperand
      { regOpName :: String
        -- ^ Name of this register operand.
      , regOpClass :: [String]
        -- ^ Register class, denoted as a list of register names.
      }

    -- | Represents an immediate operand.
  | ImmInstrOperand
      { immOpName :: String
        -- ^ Name of this immediate operand.
      , immOpRange :: Range Integer
        -- ^ Integer range of this immediate operand.
      }
  deriving (Show)

-- | Contains the information regarding the semantics of an instruction.
data InstrSemantics
  = InstrSemantics
      { instrSemIR :: String
        -- ^ The LLVM IR code.
      }
  deriving (Show)



--------------------------------
-- JSON-related class instances
--------------------------------

instance FromJSON MachineDescription where
  parseJSON (Object v) =
    MachineDescription
      <$> v .: "instructions"
      <*> v .: "locations"
  parseJSON _ = mzero

instance FromJSON Instruction where
  parseJSON (Object v) =
    Instruction
      <$> v .: "asm"
      <*> v .: "size"
      <*> v .: "latency"
      <*> v .: "operands"
      <*> v .: "semantics"
  parseJSON _ = mzero

instance FromJSON Location where
  parseJSON (Object v) =
    do t <- v .: "type"
       case (t :: String) of
         "reg" -> RegLocation
                    <$> v .: "name"
                    <*> v .:? "value"
         _     -> mzero
  parseJSON _ = mzero

instance FromJSON InstrOperand where
  parseJSON (Object v) =
    do t <- v .: "type"
       case (t :: String) of
         "reg" -> RegInstrOperand
                    <$> v .: "name"
                    <*> v .: "class"
         "imm" -> ImmInstrOperand
                    <$> v .: "name"
                    <*> v .: "range"
         _     -> mzero
  parseJSON _ = mzero

instance FromJSON InstrSemantics where
  parseJSON (String s) = return $ InstrSemantics $ unpack s
  parseJSON _ = mzero
