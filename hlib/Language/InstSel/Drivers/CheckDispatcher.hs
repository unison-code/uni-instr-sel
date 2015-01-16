--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.CheckDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching check commands.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.CheckDispatcher
  ( run )
where

import Language.InstSel.Drivers.DispatcherTools



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (checkAction opts) opts

dispatch :: CheckAction -> Options -> IO [Output]
dispatch a opts
  | a == CheckNothing =
      reportError "No check action provided."
  | otherwise =
      reportError "CheckDispatcher: unsupported action"
