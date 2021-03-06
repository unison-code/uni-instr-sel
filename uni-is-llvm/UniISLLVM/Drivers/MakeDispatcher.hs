{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniISLLVM.Drivers.MakeDispatcher
  ( run )
where

import UniISLLVM.Drivers.DispatcherTools
import qualified UniISLLVM.Drivers.MakeFunctionFromLLVM as MakeFunctionFromLLVM



-------------
-- Functions
-------------

-- | Function for executing this dispatcher.
run :: Options -> IO [Output]
run opts = dispatch (makeAction opts) opts

-- | Dispatches execution to the correct driver.
dispatch :: MakeAction -> Options -> IO [Output]
dispatch a opts
  | a == MakeNothing =
      reportErrorAndExit "No make action provided."
  | a `elem` [MakeFunctionGraphFromLLVM] =
      do content <- loadFunctionFileContent opts
         MakeFunctionFromLLVM.run a content
  | otherwise =
      reportErrorAndExit "MakeDispatcher: unsupported action"
