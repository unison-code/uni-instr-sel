--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.PatternMatcher
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function file and target machine as input, and performs pattern
-- matching of all instruction patterns on the function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.PatternMatcher
  ( run )
where

import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.TargetMachines.PatternMatching
  ( mkMatchsetInfo )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON

import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

run
  :: String
     -- ^ The function in JSON format.
  -> TargetMachine
     -- ^ The target machine.
  -> (String -> IO ())
     -- ^ The function that takes care of outputting the result.
  -> IO ()
run str target output =
  do let res = fromJson str
     when (isLeft res) $
       do putStrLn $ fromLeft res
          exitFailure
     let function = fromRight res
         matches = mkMatchsetInfo function target
     output $ toJson matches
