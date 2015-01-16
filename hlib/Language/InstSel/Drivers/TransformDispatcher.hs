--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.TransformDispatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for dispatching transformation commands.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.TransformDispatcher
  ( run )
where

import Language.InstSel.Drivers.DispatcherTools
import qualified Language.InstSel.Drivers.TransformFunctionGraph as TFun



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts = dispatch (transformAction opts) opts

dispatch :: TransformAction -> Options -> IO [Output]
dispatch a opts
  | a == TransformNothing =
      reportError "No transform action provided."
  | a `elem` [CopyExtendFunctionGraph, BranchExtendFunctionGraph] =
      do content <- loadFunctionFileContent opts
         function <- loadFromJson content
         TFun.run a function
  | otherwise =
      reportError "TransformDispatcher: unsupported action"
