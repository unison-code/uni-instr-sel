--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.SExpressions
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains types and functions for handling S-expressions (mostly for
-- printing).
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Language.InstructionSelection.SExpressions.Base where

import Language.InstructionSelection.Utils (Range (..), Natural, fromNatural)
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

  -- | Pretty-prints a list. If the list is not empty, it first prints a new
  -- line, and each element in the list will be separated by a new line. Each
  -- element will also be indented according to the depth level.

  prettySEList :: [a] -> Natural -> String
  prettySEList [] _ = ""
  prettySEList as i =
    "\n" ++ (intercalate "\n" $ map (\a -> (indent i ++ prettySE a i)) as)

  -- | Pretty-prints a list but with no line breaks between each element. The
  -- list itself will also not be indented.

  prettySEListNoBreak :: [a] -> Natural -> String
  prettySEListNoBreak as i = intercalate " " $ map (\a -> (prettySE a i)) as



-- | Prints indentation according to the depth level (each level incurs 2
-- spaces).

indent :: Natural -> String
indent i = replicate (2 * (fromInteger $ fromNatural i)) ' '

showSE :: (SExpressionable a) => a -> String
showSE a = prettySE a 0

showSEList :: (SExpressionable a) => [a] -> String
showSEList a = prettySEList a 0



--------------------------------------------------
-- Basic instances
--------------------------------------------------

instance SExpressionable Integer where
  prettySE int _ = show int

instance (SExpressionable a) => SExpressionable [a] where
  prettySE as i = prettySEList as i

instance SExpressionable Char where
  prettySE c _ = [c]
  prettySEList str _ = str
  prettySEListNoBreak = prettySEList

instance (SExpressionable i) => SExpressionable (Range i) where
  prettySE (Range lower upper) i =
    "(" ++ prettySE lower i ++ " . " ++ prettySE upper i ++ ")"

instance (SExpressionable l, SExpressionable r) =>
         SExpressionable (Either l r) where
  prettySE (Left l) i = prettySE l i
  prettySE (Right r) i = prettySE r i