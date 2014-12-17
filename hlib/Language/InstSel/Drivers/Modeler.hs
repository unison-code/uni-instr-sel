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
import Language.InstSel.ProgramModules.LLVM
  ( mkFunctionsFromLlvmModule )
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.Utils
  ( isLeft )
import Control.Monad
  ( when )
import Control.Monad.Error
  ( runErrorT )
import LLVM.General
import LLVM.General.Context



-------------
-- Functions
-------------

run ::
     String
     -- ^ The content of the LLVM IR file.
  -> TargetMachine
     -- ^ The target machine.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the JSON data.
  -> IO ()
run str target emit =
  do function <- parseFunction str
     let params = mkParams target function
     emit $ toJson params

parseFunction :: String -> IO Function
parseFunction str =
  do llvm_module_result <-
       withContext
         ( \context ->
             runErrorT $ withModuleFromLLVMAssembly context str moduleAST
         )
     when (isLeft llvm_module_result) $
       do let (Left e) = llvm_module_result
          error $ show e
     let (Right llvm_module) = llvm_module_result
     let functions = mkFunctionsFromLlvmModule llvm_module
     when (length functions > 1) $
       error "Only supports one function per module."
     return $ head functions
