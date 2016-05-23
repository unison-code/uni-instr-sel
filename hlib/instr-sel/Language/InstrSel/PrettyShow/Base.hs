--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.PrettyShow.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- The convention applied within this project is that the default implementation
-- of 'Show' type class is NEVER overridden (this is to ensure that valid
-- Haskell source code can always be generated). Instead, a type class called
-- 'PrettyShow' is used for printing data types, either for debugging purposes
-- or for later parsing.
--
--------------------------------------------------------------------------------

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
  pShowList as = "[" ++ intercalate "," (map pShow as) ++ "]"



-------------
-- Instances
-------------

instance (PrettyShow a) => PrettyShow [a] where
  pShow = pShowList

instance (PrettyShow a, PrettyShow b) => PrettyShow ((,) a b) where
  pShow (a, b) = "(" ++ pShow a ++ ", " ++ pShow b ++ ")"

instance PrettyShow Integer where
  pShow = show

instance PrettyShow Float where
  pShow = show

instance PrettyShow Char where
  pShow = show
  pShowList = show

instance (PrettyShow a) => PrettyShow (Maybe a) where
  pShow (Just a) = pShow a
  pShow Nothing = "?"

instance (PrettyShow a) => PrettyShow (Set.Set a) where
  pShow = pShow . Set.toList
