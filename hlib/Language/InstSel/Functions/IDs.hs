--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Functions.IDs
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

module Language.InstSel.Functions.IDs
  ( BasicBlockLabel (..)
  , mkEmptyBBLabel
  , isBBLabelEmpty
  )
where

import Language.InstSel.DebugShow
import Language.InstSel.Utils.JSON



--------------
-- Data types
--------------

-- | Represents a basic block label identifier.
newtype BasicBlockLabel
  = BasicBlockLabel String
  deriving (Eq)

instance Show BasicBlockLabel where
  show (BasicBlockLabel str) = str

instance DebugShow BasicBlockLabel where
  dShow = show



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON BasicBlockLabel where
  parseJSON (String s) = return $ (BasicBlockLabel $ unpack s)
  parseJSON _ = mzero

instance ToJSON BasicBlockLabel where
  toJSON (BasicBlockLabel s) = toJSON s



-------------
-- Functions
-------------

-- | Creates a basic block label that is essentially empty.
mkEmptyBBLabel :: BasicBlockLabel
mkEmptyBBLabel = BasicBlockLabel ""

-- | Checks if a basic block label is empty.
isBBLabelEmpty :: BasicBlockLabel -> Bool
isBBLabelEmpty (BasicBlockLabel str) = str == ""