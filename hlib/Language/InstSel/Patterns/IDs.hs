--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Patterns.IDs
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
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstSel.Patterns.IDs
  ( PatternID (..)
  , fromPatternID
  , toPatternID
  )
where

import Language.InstSel.Utils
  ( Natural
  , fromNatural
  , toNatural
  )
import Language.InstSel.Utils.Lisp
  hiding
  ( Lisp (..) )
import qualified Language.InstSel.Utils.Lisp as Lisp
  ( Lisp (..) )
import Language.InstSel.Utils.JSON
  hiding
  ( Value (..) )
import qualified Language.InstSel.Utils.JSON as JSON
  ( Value (..) )



--------------
-- Data types
--------------

newtype PatternID
  = PatternID Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show PatternID where
  show (PatternID i) = show i



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON PatternID where
  parseJSON (JSON.Number sn) = return $ toPatternID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON PatternID where
  toJSON pid = toJSON (fromPatternID pid)



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp PatternID where
  parseLisp (Lisp.Number (I n)) = return $ toPatternID n
  parseLisp _ = mzero

instance ToLisp PatternID where
  toLisp (PatternID nid) = Lisp.Number (I (fromNatural nid))



-------------
-- Functions
-------------

fromPatternID :: PatternID -> Natural
fromPatternID (PatternID i) = i

toPatternID :: (Integral i) => i -> PatternID
toPatternID = PatternID . toNatural
