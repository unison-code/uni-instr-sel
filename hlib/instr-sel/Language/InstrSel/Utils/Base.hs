{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
  , group
  , groupBy
  , isLeft
  , isRight
  , mapPair
  , removeAt
  , (===)
  , scanlM
  , combinations
  , toPair
  )
where

import Data.List
  ( sort )



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

-- | Groups elements such that a set of elements, for which equality holds for
-- every element pair in that set, are grouped together.
group :: (Eq n) => [n] -> [[n]]
group es =
  foldr gr [] es
  where gr e [] = [[e]]
        gr e (p:ps) = if belongs e p then ((e:p):ps) else (p:gr e ps)
        belongs e es' = e == (head es')

-- | Groups elements according to a predicate function such that a set of
-- elements, for which the predicate holds for every element pair in that set,
-- are grouped together. It is assumed that the predicate function is
-- commutative and associative.
groupBy :: (n -> n -> Bool) -> [n] -> [[n]]
groupBy f es =
  foldr gr [] es
  where gr e [] = [[e]]
        gr e (p:ps) = if belongs e p then ((e:p):ps) else (p:gr e ps)
        belongs e es' = e `f` (head es')



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

-- | Converts a list of 2-element into a pair.
toPair :: [a] -> (a, a)
toPair xs = if length xs == 2
            then (head xs, last xs)
            else error "toPairs: list has not 2 elements"
