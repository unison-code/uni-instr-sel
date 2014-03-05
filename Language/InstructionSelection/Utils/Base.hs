--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Misc.Utils
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains generic data types and functions.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Utils.Base where


------------------------
-- Class and data types
------------------------

-- | Record for representing a value range.

data Range t
     = Range {
          -- | Smallest possible value (i.e. inclusive).

          lowerBound :: t

          -- | Largest possible value (i.e. inclusive).

        , upperBound :: t

       }
     deriving (Show, Eq)

-- | Creates a new data type that allows numbers from 0 to positive infinity.

newtype Natural = Natural Integer
    deriving (Eq,Ord)
instance Show Natural where
  show (Natural i) = show i

toNatural :: (Integral i) => i -> Natural
toNatural x | x < 0     = error "Natural cannot be negative"
            | otherwise = Natural $ toInteger x

fromNatural :: Natural -> Integer
fromNatural (Natural i) = i

instance Num Natural where
    fromInteger = toNatural
    x + y       = toNatural (fromNatural x + fromNatural y)
    x - y       = let r = fromNatural x - fromNatural y in
                      if r < 0 then error "Subtraction yielded a negative value"
                               else toNatural r
    x * y       = toNatural (fromNatural x * fromNatural y)
    abs x       = x
    signum x    = toNatural $ signum $ fromNatural x



-------------
-- Functions
-------------

-- | Removes duplicates from a list.

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rd []
  where rd seen [] = seen
        rd seen (x:xs)
           | x `elem` seen = rd seen xs
           | otherwise = rd (x:seen) xs
