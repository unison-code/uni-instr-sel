--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.ConstraintModels.IDs
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

module Language.InstrSel.ConstraintModels.IDs
  ( ArrayIndex (..)
  , toArrayIndex
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

newtype ArrayIndex
  = ArrayIndex Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show ArrayIndex where
  show (ArrayIndex i) = show i



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON ArrayIndex where
  parseJSON (JSON.Number sn) = return $ toArrayIndex $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON ArrayIndex where
  toJSON mid = toJSON (fromArrayIndex mid)



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp ArrayIndex where
  parseLisp (Lisp.Number (I n)) = return $ toArrayIndex n
  parseLisp _ = mzero

instance ToLisp ArrayIndex where
  toLisp (ArrayIndex nid) = Lisp.Number (I (fromNatural nid))




-------------
-- Functions
-------------

fromArrayIndex :: ArrayIndex -> Natural
fromArrayIndex (ArrayIndex i) = i

toArrayIndex :: (Integral i) => i -> ArrayIndex
toArrayIndex = ArrayIndex . toNatural
