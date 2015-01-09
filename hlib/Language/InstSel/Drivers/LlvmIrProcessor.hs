--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.LlvmIrProcessor
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes an LLVM IR file and processes it into the graph-based representation,
-- and outputs the result in JSON format.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.LlvmIrProcessor
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON

import Language.InstSel.ProgramModules.LLVM
  ( mkFunctionsFromLlvmModule )
import LLVM.General
import LLVM.General.Context
import Control.Monad.Except
  ( runExceptT )

import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

run
  :: String
     -- ^ The function in LLVM IR format.
  -> IO [Output]
     -- ^ The produced output.
run str =
  do llvm_module_result <-
       withContext
         ( \context ->
             runExceptT $ withModuleFromLLVMAssembly context str moduleAST
         )
     when (isLeft llvm_module_result) $
       do putStrLn $ fromLeft $ fromLeft llvm_module_result
          exitFailure
     let llvm_module = fromRight llvm_module_result
     let functions = mkFunctionsFromLlvmModule llvm_module
     when (length functions > 1) $
       do putStrLn "Only supports one function per module."
          exitFailure
     return [toOutputWithoutID $ toJson $ head functions]
