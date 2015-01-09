--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Misc.Utils
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains generic data types and functions.
--
--------------------------------------------------------------------------------

module Language.InstSel.Utils.Base
  ( Natural (..)
  , Range (..)
  , computePosMapsOfPerm
  , fromLeft
  , fromNatural
  , fromRight
  , groupBy
  , isLeft
  , isRight
  , maybeToNatural
  , replace
  , splitOn
  , toLower
  , toNatural
  , toUpper
  )
where

import Data.List
  ( intercalate )
import Data.List.Split
  ( splitOn )
import Data.Maybe
import qualified Data.Char as Char
  ( toLower
  , toUpper
  )


--------------
-- Data types
--------------

-- | Record for representing a value range.
data Range t
  = Range
      { lowerBound :: t
        -- | Smallest possible value (i.e. inclusive).
      , upperBound :: t
        -- | Largest possible value (i.e. inclusive).
      }
  deriving (Show, Eq)

-- | Creates a new data type that allows numbers from 0 to positive infinity.
newtype Natural
  = Natural Integer
  deriving (Eq, Ord)



----------------------------------------
-- Natural-related type class instances
----------------------------------------

instance Show Natural where
  show (Natural i) = show i

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
     else error "Natural cannot be negative"

-- | Converts a 'Natural' into an 'Integer'.
fromNatural :: Natural -> Integer
fromNatural (Natural i) = i

-- | Takes two lists, where one list is a permutation of the another list, and
-- returns a list of position mappings such that one list can be transformed
-- into the other list just by rearranging the positions. If one list is not a
-- permutation of the other, an error will occur.
computePosMapsOfPerm ::
  Eq a
  => [a]
     -- ^ A list.
  -> [a]
     -- ^ A permutation of the first list.
  -> [Natural]
     -- ^ A list of position mappings.
computePosMapsOfPerm ol pl =
  let addPosMap e (index_in_ol, ms) =
        let checkIndex i = i `notElem` ms && (pl !! i == e)
            index_in_pl = until
                          (\i -> maybe True checkIndex i)
                          ( \(Just i) ->
                             let next_i = i + 1
                             in if next_i < length pl
                                then Just next_i
                                else Nothing
                          )
                          (Just 0)
        in if isJust index_in_pl
           then (index_in_ol - 1, (fromJust index_in_pl):ms)
           else error ("computePosMapsOfPerm: the lists do not contain the "
                       ++ "same elements")
      (_, maps) = foldr addPosMap (length ol - 1, []) ol
  in if length ol == length pl
     then map toNatural maps
     else error "computePosMapsOfPerm: the lists are not of equal lengths"

-- | Checks if an 'Either' is of type 'Left'.
isLeft :: Either l r -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Checks if an 'Either' is of type 'Right'.
isRight :: Either l r -> Bool
isRight (Right _) = True
isRight _ = False

-- | Gets the data contained by a 'Left'.
fromLeft :: Either l r -> l
fromLeft (Left l) = l
fromLeft _ = error "Either is not Left"

-- | Gets the data contained by a 'Right'.
fromRight :: Either l r -> r
fromRight (Right r) = r
fromRight _ = error "Either is not Right"

-- | Groups elements according to a predicate function such that a set of
-- elements, for which the predicate holds for every element pair in that set,
-- are grouped together. It is assumed that the predicate function is
-- commutative and associative.
groupBy :: (n -> n -> Bool) -> [n] -> [[n]]
groupBy f es =
  foldr (gr f) [] es
  where gr _ e [] = [[e]]
        gr f' e (p:ps) =
          if belongs f' e p then (e:p):ps else p:(gr f' e ps)
        belongs f'' e' es' = any (f'' e') es'

-- | Replaces a substring with another substring.
replace
  :: String
     -- ^ What to search for.
  -> String
     -- ^ What to replace with.
  -> String
     -- ^ What to search in.
  -> String
replace old new = intercalate new . splitOn old

-- | Converts a string to lower cases.
toLower :: String -> String
toLower = map Char.toLower

-- | Converts a string to upper cases.
toUpper :: String -> String
toUpper = map Char.toUpper
