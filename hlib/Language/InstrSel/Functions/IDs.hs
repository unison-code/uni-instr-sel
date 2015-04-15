--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Functions.IDs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing various IDs.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Functions.IDs
  ( BlockName (..)
  , mkEmptyBlockName
  , isBlockNameEmpty
  )
where

import Language.InstrSel.DebugShow
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Represents a block name.
newtype BlockName
  = BlockName String
  deriving (Eq)

instance Show BlockName where
  show (BlockName str) = str

instance DebugShow BlockName where
  dShow = show



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON BlockName where
  parseJSON (String s) = return $ (BlockName $ unpack s)
  parseJSON _ = mzero

instance ToJSON BlockName where
  toJSON (BlockName s) = toJSON s



-------------
-- Functions
-------------

-- | Creates an empty block name.
mkEmptyBlockName :: BlockName
mkEmptyBlockName = BlockName ""

-- | Checks if block name is empty.
isBlockNameEmpty :: BlockName -> Bool
isBlockNameEmpty (BlockName str) = str == ""
