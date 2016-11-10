{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.Set
  ( module Data.Set
  , intersections
  )
where

import Data.Set
import Prelude hiding
  ( foldr )
import qualified Prelude as P
  ( foldr )



-------------
-- Functions
-------------

intersections :: (Ord a) => [Set a] -> Set a
intersections [] = empty
intersections ss =
  P.foldr (\s1 s2 -> s1 `intersection` s2)
          (head ss)
          (tail ss)
