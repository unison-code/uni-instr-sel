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
  ( BasicBlockLabel (..) )
where

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



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON BasicBlockLabel where
  parseJSON (String s) = return $ (BasicBlockLabel $ unpack s)
  parseJSON _ = mzero

instance ToJSON BasicBlockLabel where
  toJSON (BasicBlockLabel s) = toJSON s
