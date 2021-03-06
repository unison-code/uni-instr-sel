{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

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

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.Natural
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

import Control.DeepSeq
  ( NFData
  , rnf
  )



--------------
-- Data types
--------------

-- | Identifier for a 'Match'.
newtype MatchID
  = MatchID Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow MatchID where
  pShow (MatchID i) = pShow i

-- | Identifier for a 'Node'.
newtype NodeID
  = NodeID Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow NodeID where
  pShow (NodeID i) = pShow i

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



----------------------------------------
-- DeepSeq-related type class instances
--
-- These are needed to be able to time
-- how long it takes to produce the
-- matchsets
----------------------------------------

instance NFData MatchID where
  rnf (MatchID a) = rnf a

instance NFData NodeID where
  rnf (NodeID a) = rnf a



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
