--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.MakeFunctionFromLLVM
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Produces a function from LLVM IR.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.MakeFunctionFromLLVM
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Functions.LLVM
  ( mkFunctionsFromLlvmModule )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstrSel.Utils.IO
  ( reportError )

import LLVM.General
import LLVM.General.Context
import Control.Monad.Except
  ( runExceptT )



-------------
-- Functions
-------------

run
  :: MakeAction
  -> String
     -- ^ The content of the LLVM IR file.
  -> IO [Output]

run MakeFunctionGraphFromLLVM str =
  do llvm_module_result <-
       withContext
         ( \context ->
             runExceptT $ withModuleFromLLVMAssembly context str moduleAST
         )
     when (isLeft llvm_module_result) $
       reportError $ fromLeft $ fromLeft llvm_module_result
     let llvm_module = fromRight llvm_module_result
     let functions = mkFunctionsFromLlvmModule llvm_module
     when (length functions > 1) $
       reportError "Only supports one function per module."
     return [toOutputWithoutID $ toJson $ head functions]

run _ _ = reportError "MakeFunctionFromLLVM: unsupported action"
