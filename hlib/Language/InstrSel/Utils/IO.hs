--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Utils.IO
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Various IO-related functions.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Utils.IO where

import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

-- | Reports an error, and then terminates the program with an appropriate exit
-- code.
reportError :: String -> IO a
reportError str =
  do putStrLn str
     exitFailure
