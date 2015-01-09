--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.DebugShow.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains data types for operations.
--
--------------------------------------------------------------------------------

module Language.InstSel.DebugShow.Base where



----------------
-- Type classes
----------------

-- | A class for when to print stuff for debugging-purposes only, not for any
-- later parsing. This means that information can be lost without consequences
-- (for instance, many operations will shown the same string, but that's okay
-- since it's only for viewing when this fact is known).
class DebugShow a where
  dShow :: a -> String
