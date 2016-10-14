{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.Natural where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.JSON
import Data.Maybe

import Control.DeepSeq
  ( NFData
  , rnf
  )



--------------
-- Data types
--------------

-- | A data type that allows numbers from 0 to positive infinity.
newtype Natural
  = Natural Integer
  deriving (Show, Eq, Ord)



----------------------------------------
-- Natural-related type class instances
----------------------------------------

instance PrettyShow Natural where
  pShow (Natural i) = pShow i

instance Num Natural where
    fromInteger = toNatural
    x + y = toNatural (fromNatural x + fromNatural y)
    x - y = let r = fromNatural x - fromNatural y
            in if r < 0
               then error "Subtraction yielded a negative value"
               else toNatural r
    x * y = toNatural (fromNatural x * fromNatural y)
    abs x = x
    signum x = toNatural $ signum $ fromNatural x

instance Enum Natural where
  toEnum = toNatural . toInteger
  fromEnum = fromInteger . fromNatural

instance Real Natural where
  toRational (Natural i) = toRational i

instance Integral Natural where
  quotRem (Natural x) (Natural y) =
    ( toNatural $ quot x y
    , toNatural $ rem x y
    )
  toInteger (Natural i) = i



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON Natural where
  parseJSON (Number sn) = return $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)



----------------------------------------
-- DeepSeq-related type class instances
--
-- These are needed to be able to time
-- how long it takes to produce the
-- matchsets
----------------------------------------

instance NFData Natural where
  rnf (Natural a) = rnf a



-------------
-- Functions
-------------

-- | Converts an 'Integral' into a 'Natural'. If conversion fails, 'Nothing' is
-- returned.
maybeToNatural :: (Integral i) => i -> Maybe Natural
maybeToNatural x
  | x < 0     = Nothing
  | otherwise = Just $ Natural $ toInteger x

-- | Converts an 'Integral' into a 'Natural'. If conversion fails, an error is
-- reported.
toNatural :: (Integral i) => i -> Natural
toNatural x =
  let n = maybeToNatural x
  in if isJust n
     then fromJust n
     else error $ "toNatural: negative number: " ++
                  show (toInteger x :: Integer)

-- | Converts a 'Natural' into an 'Integer'.
fromNatural :: Natural -> Integer
fromNatural (Natural i) = i

-- | Converts a scientific number to a natural number. If the number is not an
-- non-negative then an error occurs.
sn2nat :: Scientific -> Natural
sn2nat sn =
  let int_value = round sn
  in if fromInteger int_value /= sn
     then error $ "sn2nat: not an integer: " ++ show sn
     else toNatural int_value
