--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.TargetMachine.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
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

module Language.InstructionSelection.TargetMachine.Base (
  RegisterID (..)
, TargetMachine (..)
, fromRegisterID
, toRegisterID
) where

import Language.InstructionSelection.Utils ( Natural
                                           , toNatural
                                           )



--------------
-- Data types
--------------

-- | Represents a register ID.

newtype RegisterID = RegisterID Natural
  deriving (Eq, Ord, Num, Enum)

instance Show RegisterID where
  show (RegisterID i) = show i

-- | Represents a target machine.

data TargetMachine
    = TargetMachine {

          -- | The machine registers. Each must be given a unique ID, but not
          -- necessarily in a contiguous order.

          tmRegisters :: [( String     -- ^ Register name (needed during code
                                       -- emission).
                          , RegisterID -- ^ Register ID (only used internally).
                          )]

      }
    deriving (Show)



-------------
-- Functions
-------------

fromRegisterID :: RegisterID -> Natural
fromRegisterID (RegisterID i) = i

toRegisterID :: (Integral i) => i -> RegisterID
toRegisterID = RegisterID . toNatural
