--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Plotter
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for dumping various information about the input.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Plotter
  ( run )
where

import Language.InstSel.TargetMachines
  ( TargetMachine )



-------------
-- Functions
-------------

run ::
     String
     -- ^ The content of some input file.
  -> TargetMachine
     -- ^ The target machine.
  -> [Bool]
     -- ^ List of Boolean arguments which determine what to plot. Note that the
     -- order is important.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the JSON data.
  -> IO ()
run = undefined
      -- TODO: implement
