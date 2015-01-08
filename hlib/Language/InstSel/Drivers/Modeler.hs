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
-- Takes a function graph, a set of pattern matches, and target machine as
-- input, and produces the data needed for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Modeler
  ( run )
where


import Language.InstSel.CPModel.ParamMaker
import Language.InstSel.ProgramModules
  ( Function )
import Language.InstSel.ProgramModules.LLVM
  ( mkFunctionsFromLlvmModule )
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON
import Control.Monad.Except
  ( runExceptT )
import LLVM.General
import LLVM.General.Context
import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

run ::
     String
     -- ^ The function in JSON format.
  -> String
     -- ^ The instruction pattern matches in JSON format.
  -> TargetMachine
     -- ^ The target machine.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the CP model parameters.
  -> IO ()
run f_str m_str target emit =
  do let f_res = fromJson f_str
         m_res = fromJson m_str
     when (isLeft f_res) $
       do putStrLn $ fromLeft f_res
          exitFailure
     when (isLeft m_res) $
       do putStrLn $ fromLeft m_res
          exitFailure
     let function = fromRight f_res :: Function
         matches = fromRight m_res :: [MatchData]
         params = mkParams target function matches
     emit $ toJson params
