--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachines.IDs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing various IDs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstSel.TargetMachines.IDs
  ( AssemblyID (..)
  , InstructionID (..)
  , RegisterID (..)
  , RegisterFlagName (..)
  , RegisterName (..)
  , TargetMachineID (..)
  , fromAssemblyID
  , fromInstructionID
  , fromRegisterID
  , fromTargetMachineID
  , toAssemblyID
  , toInstructionID
  , toRegisterID
  , toTargetMachineID
  )
where

import Language.InstSel.Utils
  ( Natural
  , fromNatural
  , toNatural
  )
import Language.InstSel.Utils.Lisp
  hiding
  ( Lisp (..) )
import qualified Language.InstSel.Utils.Lisp as Lisp
  ( Lisp (..) )
import Language.InstSel.Utils.JSON
  hiding
  ( Value (..) )
import qualified Language.InstSel.Utils.JSON as JSON
  ( Value (..) )



--------------
-- Data types
--------------

-- | Represents an instruction ID.
newtype InstructionID
  = InstructionID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show InstructionID where
  show (InstructionID i) = show i

-- | Represents an ID to be used as place-holders inside an assembly string.
newtype AssemblyID
  = AssemblyID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show AssemblyID where
  show (AssemblyID i) = show i

-- | Represents a register ID.
newtype RegisterID
  = RegisterID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show RegisterID where
  show (RegisterID i) = show i

-- | Represents a register name.
newtype RegisterName
  = RegisterName String
  deriving (Eq)

instance Show RegisterName where
  show (RegisterName s) = s

-- | Represents a register flag name.
newtype RegisterFlagName
  = RegisterFlagName String
  deriving (Eq)

instance Show RegisterFlagName where
  show (RegisterFlagName s) = s

-- | Represents a target machine ID.
newtype TargetMachineID
  = TargetMachineID String
  deriving (Eq, Show)



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON InstructionID where
  parseJSON (JSON.Number sn) = return $ toInstructionID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON InstructionID where
  toJSON iid = toJSON (fromInstructionID iid)

instance FromJSON RegisterID where
  parseJSON (JSON.Number sn) = return $ toRegisterID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON RegisterID where
  toJSON rid = toJSON (fromRegisterID rid)

instance FromJSON AssemblyID where
  parseJSON (JSON.Number sn) = return $ toAssemblyID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON AssemblyID where
  toJSON aid = toJSON (fromAssemblyID aid)

instance FromJSON TargetMachineID where
  parseJSON (JSON.String s) = return $ toTargetMachineID $ unpack s
  parseJSON _ = mzero

instance ToJSON TargetMachineID where
  toJSON tmid = toJSON (fromTargetMachineID tmid)



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp InstructionID where
  parseLisp (Lisp.Number (I n)) = return $ toInstructionID n
  parseLisp _ = mzero

instance ToLisp InstructionID where
  toLisp (InstructionID nid) = Lisp.Number (I (fromNatural nid))

instance FromLisp RegisterID where
  parseLisp (Lisp.Number (I n)) = return $ toRegisterID n
  parseLisp _ = mzero

instance ToLisp RegisterID where
  toLisp (RegisterID nid) = Lisp.Number (I (fromNatural nid))

instance FromLisp AssemblyID where
  parseLisp (Lisp.Number (I n)) = return $ toAssemblyID n
  parseLisp _ = mzero

instance ToLisp AssemblyID where
  toLisp (AssemblyID aid) = Lisp.Number (I (fromNatural aid))



-------------
-- Functions
-------------

fromRegisterID :: RegisterID -> Natural
fromRegisterID (RegisterID i) = i

toRegisterID :: (Integral i) => i -> RegisterID
toRegisterID = RegisterID . toNatural

fromTargetMachineID :: TargetMachineID -> String
fromTargetMachineID (TargetMachineID i) = i

toTargetMachineID :: String -> TargetMachineID
toTargetMachineID = TargetMachineID

fromInstructionID :: InstructionID -> Natural
fromInstructionID (InstructionID i) = i

toInstructionID :: (Integral i) => i -> InstructionID
toInstructionID = InstructionID . toNatural

fromAssemblyID :: AssemblyID -> Natural
fromAssemblyID (AssemblyID i) = i

toAssemblyID :: (Integral i) => i -> AssemblyID
toAssemblyID = AssemblyID . toNatural
