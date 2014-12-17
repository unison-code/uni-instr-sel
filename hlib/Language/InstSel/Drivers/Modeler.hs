--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Modeler
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes an LLVM IR file and target machine as input, and produces the data
-- needed for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Modeler
  ( run )
where

import Language.InstSel.CPModel.Json
import Language.InstSel.CPModel.ParamMaker
import Language.InstSel.ProgramModules
  ( Function )
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Control.Monad
  ( when )



-------------
-- Functions
-------------

run ::
     Function
     -- ^ The function.
  -> TargetMachine
     -- ^ The target machine.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the JSON data.
  -> IO ()
run function target emit =
  do let params = mkParams target function
     emit $ toJson params
