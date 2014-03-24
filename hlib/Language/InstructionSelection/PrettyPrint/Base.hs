--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.PrettyPrint.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.PrettyPrint.Base where



-- | A class for when to print stuff for viewing only, not for any later
-- parsing. This means that information can be lost without consequences (for
-- instance, many operations will be pretty-printed to the same string, but
-- that's okay since it's only for viewing when this fact is known).

class PrettyPrint a where
  prettyShow :: a -> String
