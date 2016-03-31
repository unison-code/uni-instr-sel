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
  , toBlockName
  , fromBlockName
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Represents a block name.
newtype BlockName
  = BlockName String
  deriving (Show, Eq)

instance PrettyShow BlockName where
  pShow (BlockName str) = str



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

toBlockName :: String -> BlockName
toBlockName = BlockName

fromBlockName :: BlockName -> String
fromBlockName (BlockName str) = str
