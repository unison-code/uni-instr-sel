{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniISLLVM.Drivers.MakeFunctionFromLLVM
  ( run )
where

import UniISLLVM.Drivers.Base
import Language.InstrSel.Functions.LLVM
  ( mkFunctionsFromLlvmModule )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import LLVM.General
import LLVM.General.Context
import Control.Monad.Except
  ( runExceptT )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run
  :: MakeAction
  -> BS.ByteString
     -- ^ The content of the LLVM IR file.
  -> IO [Output]

run MakeFunctionGraphFromLLVM str =
  do llvm_module_result <-
       withContext
         ( \context ->
           runExceptT $ withModuleFromLLVMAssembly context
                                                   (BS.unpack str)
                                                   moduleAST
         )
     when (isLeft llvm_module_result) $
       reportErrorAndExit $ fromLeft $ llvm_module_result
     let llvm_module = fromRight llvm_module_result
     let functions_res = mkFunctionsFromLlvmModule llvm_module
     when (isLeft functions_res) $
       reportErrorAndExit $ fromLeft functions_res
     let functions = fromRight functions_res
     when (length functions > 1) $
       reportErrorAndExit "Only supports one function per module."
     return [toOutput $ toJson $ head functions]

run _ _ = reportErrorAndExit "MakeFunctionFromLLVM: unsupported action"
