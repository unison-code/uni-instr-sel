{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.PrettyShow.Base where

import Data.List
  ( intercalate )

import qualified Data.Set as Set



----------------
-- Type classes
----------------

-- | A class for pretty-printing data types, and the resulting string is
-- guaranteed to be parseable without ambiguity.
class PrettyShow a where
  pShow :: a -> String
  pShowList :: [a] -> String
  pShowList as = "[" ++ intercalate ", " (map pShow as) ++ "]"



-------------
-- Instances
-------------

instance (PrettyShow a) => PrettyShow [a] where
  pShow = pShowList

instance (PrettyShow a, PrettyShow b) => PrettyShow ((,) a b) where
  pShow (a, b) = "(" ++ pShow a ++ ", " ++ pShow b ++ ")"

instance PrettyShow Integer where
  pShow = show

instance PrettyShow Int where
  pShow = show

instance PrettyShow Float where
  pShow = show

instance PrettyShow Char where
  pShow = show
  pShowList = show

instance PrettyShow Bool where
  pShow = show

instance (PrettyShow a) => PrettyShow (Maybe a) where
  pShow (Just a) = pShow a
  pShow Nothing = "?"

instance (PrettyShow a) => PrettyShow (Set.Set a) where
  pShow = pShow . Set.toList
