--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.IDs
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

module Language.InstSel.Graphs.IDs
  ( MatchID (..)
  , NodeID (..)
  , fromMatchID
  , fromNodeID
  , toMatchID
  , toNodeID
  )
where

import Language.InstSel.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

-- | Represents a match ID.

newtype MatchID = MatchID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show MatchID where
  show (MatchID i) = show i

-- | Node ID data type.
newtype NodeID =
  NodeID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show NodeID where
  show (NodeID i) = show i



-------------
-- Functions
-------------

fromMatchID :: MatchID -> Natural
fromMatchID (MatchID i) = i

toMatchID :: (Integral i) => i -> MatchID
toMatchID = MatchID . toNatural

fromNodeID :: NodeID -> Natural
fromNodeID (NodeID i) = i

toNodeID :: (Integral i) => i -> NodeID
toNodeID = NodeID . toNatural
