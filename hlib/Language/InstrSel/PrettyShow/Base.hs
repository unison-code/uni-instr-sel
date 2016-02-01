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
-- of 'Show' type class is *never* overridden (this is to ensure that valid
-- Haskell source code can always be generated). Instead, a type class called
-- 'PrettyShow' is used for printing data types, either for debugging purposes
-- or for later parsing.
--
--------------------------------------------------------------------------------

module Language.InstrSel.PrettyShow.Base where



----------------
-- Type classes
----------------

-- | A class for pretty-printing data types, and the resulting string is
-- guaranteed to be parseable without ambiguity.
class PrettyShow a where
  pShow :: a -> String



-------------
-- Instances
-------------

instance PrettyShow Integer where
  pShow = show

instance PrettyShow Float where
  pShow = show

instance (PrettyShow a, PrettyShow b) => PrettyShow ((,) a b) where
  pShow (a, b) = "(" ++ pShow a ++ ", " ++ pShow b ++ ")"

