--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs.IDs
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstrSel.Graphs.IDs
  ( MatchID (..)
  , NodeID (..)
  , fromMatchID
  , fromNodeID
  , toMatchID
  , toNodeID
  )
where

import Language.InstrSel.Utils
  ( Natural
  , fromNatural
  , toNatural
  )
import Language.InstrSel.Utils.Lisp
  hiding
  ( Lisp (..) )
import qualified Language.InstrSel.Utils.Lisp as Lisp
  ( Lisp (..) )
import Language.InstrSel.Utils.JSON
  hiding
  ( Value (..) )
import qualified Language.InstrSel.Utils.JSON as JSON
  ( Value (..) )



--------------
-- Data types
--------------

newtype MatchID
  = MatchID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show MatchID where
  show (MatchID i) = show i

newtype NodeID
  = NodeID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show NodeID where
  show (NodeID i) = show i



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON MatchID where
  parseJSON (JSON.Number sn) = return $ toMatchID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON MatchID where
  toJSON mid = toJSON (fromMatchID mid)

instance FromJSON NodeID where
  parseJSON (JSON.Number sn) = return $ toNodeID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON NodeID where
  toJSON mid = toJSON (fromNodeID mid)



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp NodeID where
  parseLisp (Lisp.Number (I n)) = return $ toNodeID n
  parseLisp _ = mzero

instance ToLisp NodeID where
  toLisp (NodeID nid) = Lisp.Number (I (fromNatural nid))

instance FromLisp MatchID where
  parseLisp (Lisp.Number (I n)) = return $ toMatchID n
  parseLisp _ = mzero

instance ToLisp MatchID where
  toLisp (MatchID nid) = Lisp.Number (I (fromNatural nid))




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
