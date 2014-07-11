--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Graphs.IDs
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

module Language.InstructionSelection.Graphs.IDs
  ( NodeID (..)
  , fromNodeID
  , toNodeID
  )
where

import Language.InstructionSelection.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

-- | Node ID data type.

newtype NodeID
    = NodeID Natural
    deriving (Eq, Ord, Num, Enum)

instance Show NodeID where
  show (NodeID i) = show i



-------------
-- Functions
-------------

fromNodeID :: NodeID -> Natural
fromNodeID (NodeID i) = i

toNodeID :: (Integral i) => i -> NodeID
toNodeID = NodeID . toNatural
