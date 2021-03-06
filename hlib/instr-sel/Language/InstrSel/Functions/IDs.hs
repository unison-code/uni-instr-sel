{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Functions.IDs
  ( BlockName (..)
  , FunctionName (..)
  , mkEmptyBlockName
  , isBlockNameEmpty
  , toBlockName
  , fromBlockName
  , toFunctionName
  , fromFunctionName
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

-- | Represents a function name.
newtype FunctionName
  = FunctionName String
  deriving (Show, Eq)

instance PrettyShow FunctionName where
  pShow (FunctionName str) = str



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON BlockName where
  parseJSON (String s) = return $ (BlockName $ unpack s)
  parseJSON _ = mzero

instance ToJSON BlockName where
  toJSON (BlockName s) = toJSON s

instance FromJSON FunctionName where
  parseJSON (String s) = return $ (FunctionName $ unpack s)
  parseJSON _ = mzero

instance ToJSON FunctionName where
  toJSON (FunctionName s) = toJSON s



-------------
-- Functions
-------------

-- | Creates an empty block name.
mkEmptyBlockName :: BlockName
mkEmptyBlockName = BlockName ""

-- | Checks if block name is empty.
isBlockNameEmpty :: BlockName -> Bool
isBlockNameEmpty b = b == mkEmptyBlockName

toBlockName :: String -> BlockName
toBlockName = BlockName

fromBlockName :: BlockName -> String
fromBlockName (BlockName str) = str

toFunctionName :: String -> FunctionName
toFunctionName = FunctionName

fromFunctionName :: FunctionName -> String
fromFunctionName (FunctionName str) = str
