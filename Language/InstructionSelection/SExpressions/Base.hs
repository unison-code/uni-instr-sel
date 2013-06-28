--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.SExpressions
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains types and functions for handling S-expressions (mostly for
-- printing).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.SExpressions.Base where

import Language.InstructionSelection.Utils (Natural, fromNatural)
import Data.List (intercalate)



--------------------------------------------------
-- Type classes
--------------------------------------------------

-- | A class for pretty-printing something as an S-expression.

class SExpressionable a where

  -- | Pretty-prints something as an S-expression by converting it into a
  -- string.

  prettySE :: a                 -- ^ Item to print.
              -> Natural        -- ^ Current depth level (starts at 0).
              -> String

indent :: Natural -> String
indent i = replicate (2 * (fromInteger $ fromNatural i)) ' '

prettySEList :: (SExpressionable a) => [a] -> Natural -> String
prettySEList as i = intercalate "\n" $ map (\a -> (indent i ++ prettySE a i)) as

showSE :: (SExpressionable a) => a -> String
showSE a = prettySE a 0

showSEList :: (SExpressionable a) => [a] -> String
showSEList a = prettySEList a 0



--------------------------------------------------
-- Basic instances
--------------------------------------------------

instance SExpressionable Integer where
  prettySE int _ = show int

instance SExpressionable String where
  prettySE str _ = str
