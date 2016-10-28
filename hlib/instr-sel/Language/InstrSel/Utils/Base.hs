{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.Base
  ( fromLeft
  , fromRight
  , groupBy
  , isLeft
  , isRight
  , maybeRead
  , replace
  , splitOn
  , splitStartingOn
  , toLower
  , toUpper
  , capitalize
  , isNumeric
  , mapPair
  , removeAt
  , (===)
  , scanlM
  , combinations
  )
where

import Data.List
  ( intercalate
  , sort
  )
import qualified Data.List.Split as Split
import qualified Data.Char as Char
  ( toLower
  , toUpper
  , isDigit
  )
import Safe
  ( readMay )



-------------
-- Functions
-------------

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
  foldr gr [] es
  where gr e [] = [[e]]
        gr e (p:ps) = if belongs e p then ((e:p):ps) else (p:gr e ps)
        belongs e es' = f e (head es')

-- | Splits a given list into a list of sublists at points where a given
-- delimiter is found (the delimiters themselves are removed from the resulting
-- list). For example:
--
-- > splitOn ".." "a..b....c" == ["a", "b", "", "c"]
splitOn
  :: Eq a
  => [a]
     -- ^ The delimiter.
  -> [a]
     -- ^ List to be split.
  -> [[a]]
splitOn = Split.splitOn

-- | Splits a given list into a list of sublists at points where any of the
-- given delimiters are found. For example:
--
-- > splitStartingOn "['A'..'Z'] "AStringToBeSplit" == ["A", "String", "To", "Be", "Split"]
splitStartingOn
  :: Eq a
  => [a]
     -- ^ List of delimiters.
  -> [a]
     -- ^ List to be split.
  -> [[a]]
splitStartingOn = Split.split . Split.startsWithOneOf

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

-- | Converts a string to lowercase.
toLower :: String -> String
toLower = map Char.toLower

-- | Converts a string to uppercase.
toUpper :: String -> String
toUpper = map Char.toUpper

-- | Converts a string such that the first character is in uppercase and all
-- other characters are in lowercase. For example:
--
-- > capitalize "tEsT STRing" == "Test string"
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (Char.toUpper c:map Char.toLower cs)

-- | Checks whether a given string is numeric. An empty string is considered not
-- numeric.
isNumeric :: String -> Bool
isNumeric [] = False
isNumeric cs = all Char.isDigit cs

maybeRead :: Read a => String -> Maybe a
maybeRead = readMay

-- | Applies a function that takes two arguments on each pair of elements in a
-- list. For example:
--
-- > mapPair (+) [1, 2, 3, 4] == [3, 5, 7]
mapPair :: (a -> a -> b) -> [a] -> [b]
mapPair _ [] = []
mapPair _ [_] = error "mapPair: cannot be invoked on list with single element"
mapPair f as = zipWith f (init as) (tail as)

-- | Removes an element at a given index from a list.
removeAt :: [a] -> Int -> [a]
removeAt xs i
  | i < 0 = error "removeAt: negative index"
  | i >= length xs = error "removeAt: index out of bound"
  | otherwise = take i xs ++ drop (i + 1) xs

-- | Checks if two lists contain exactly the same elements.
(===) :: (Ord a, Eq a) => [a] -> [a] -> Bool
l1 === l2 = sort l1 == sort l2

-- | A monadic version of 'Prelude.scanl'.
scanlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM f q xs =
  do qs <- scanlM' q xs
     return (q:qs)
  where scanlM' _ [] = return []
        scanlM' q' (x':xs') =
          do q'' <- f q' x'
             qs'' <- scanlM' q'' xs'
             return (q'':qs'')

-- | Returns a list of combinations, each of length @n@ with elements taken from
-- a given list.
--
-- Written by Bergi:
-- http://stackoverflow.com/a/21288092/426092
combinations :: Int -> [a] -> [[a]]
combinations n xs = let l = length xs
                    in if n > l then [] else combinations' xs !! (l-n)
 where combinations' [] = [[[]]]
       combinations' (x:xs') =
         let next = combinations' xs'
         in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
