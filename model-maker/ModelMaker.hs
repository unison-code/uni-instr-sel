{-
Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-
Takes an LLVM IR file as input and produces a corresponding CP model for
instruction selection. The model is output in JSON format and written on STDOUT.
-}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

import Language.InstSel.CPModel.Json
import Language.InstSel.CPModel.ParamMaker
import Language.InstSel.ProgramModules.LLVM.FunctionMaker
import Language.InstSel.TargetMachine.IDs
import Language.InstSel.TargetMachine.Targets
import Control.Monad.Error
  ( runErrorT )
import Control.Monad
  ( when )
import Data.Maybe
  ( fromJust
  , isNothing
  )
import LLVM.General
import LLVM.General.Context
import System.Console.CmdArgs
import System.Exit
  (exitFailure)



---------------------------------
-- Help functions and data types
---------------------------------

data Options
    = Options
        { llvmFile :: Maybe String
        , targetName :: Maybe String
        }
    deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { llvmFile = Nothing
        &= typFile
        &= help "The LLVM IR file."
    , targetName = Nothing
        &= typ "TARGET"
        &= help "Name of the target machine."
    }

isError :: Either a b -> Bool
isError (Left _) = True
isError _ = False



----------------
-- Main program
----------------

main :: IO ()
main =
  do Options {..} <- cmdArgs parseArgs
     -- Read LLVM IR
     when (isNothing llvmFile) $
       do putStrLn "No LLVM IR file provided."
          exitFailure
     llvm_src <- readFile $ fromJust llvmFile
     llvm_module_result <-
       withContext
         ( \context ->
           do runErrorT $ withModuleFromLLVMAssembly context llvm_src moduleAST
         )
     when (isError llvm_module_result) $
       do let (Left e) = llvm_module_result
          putStrLn $ show e
          exitFailure
     let (Right llvm_module) = llvm_module_result
     let functions = mkFunctionsFromLlvmModule llvm_module
     when (length functions > 1) $
       do putStrLn "Only supports one function per module."
          exitFailure
     let function = head functions
     -- Get target machine
     when (isNothing targetName) $
       do putStrLn "No target provided."
          exitFailure
     let maybe_target = getTargetMachine $
                        toTargetMachineID (fromJust targetName)
     when (isNothing maybe_target) $
       do putStrLn "No such target."
          exitFailure
     let target = fromJust maybe_target
     -- Produce parameters
     let params = mkParams target function
     putStrLn $ toJson params
