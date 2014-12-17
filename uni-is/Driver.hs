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
Takes an LLVM IR file and target machine ID as input, and performs some action
on those. The action is determined on the command line.
-}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

import Language.InstSel.Drivers
import Language.InstSel.ProgramModules
  ( Function )
import Language.InstSel.TargetMachine
  ( TargetMachine )
import Language.InstSel.Utils
  ( isLeft )
import Control.Monad.Error
  ( runErrorT )
import Control.Monad
  ( when )
import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Console.CmdArgs



---------------------------------
-- Help functions and data types
---------------------------------

data Options
    = Options
        { command :: String
        , llvmFile :: Maybe String
        , targetName :: Maybe String
        }
    deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { command = def
        &= argPos 0
        &= typ "COMMAND"
    , llvmFile = def
        &= argPos 1
        &= typ "LLVM-FILE"
    , targetName = Nothing
        &= help "Name of the target machine."
        &= typ "TARGET"
        &= explicit
        &= name "t"
        &= name "target"
    }
    &=
    program "uni-is"
    &=
    summary ( "Unison (instruction selection) tool\n"
              ++
              "Gabriel Hjort Blindell   ghb@kth.se"
            )

readFunctionFromLLVM :: Maybe String -> IO (Function)
readFunctionFromLLVM arg =
  do when (isNothing arg) $
       error "No LLVM IR file provided."
     llvm_src <- readFile $ fromJust arg
     llvm_module_result <-
       withContext
         ( \context ->
             runErrorT $ withModuleFromLLVMAssembly context llvm_src moduleAST
         )
     when (isLeft llvm_module_result) $
       do let (Left e) = llvm_module_result
          error $ show e
     let (Right llvm_module) = llvm_module_result
     let functions = mkFunctionsFromLlvmModule llvm_module
     when (length functions > 1) $
       error "Only supports one function per module."
     return $ head functions

getTarget :: Maybe String -> IO (TargetMachine)
getTarget arg =
  do when (isNothing arg) $
       error "No target provided."
     let maybe_target =
           getTargetMachine $ toTargetMachineID $ fromJust arg
     when (isNothing maybe_target) $
       error "No such target."
     return $ fromJust maybe_target



----------------
-- Main program
----------------

main :: IO ()
main =
  do Options {..} <- cmdArgs parseArgs
     function <- readFunctionFromLLVM llvmFile
     target <- getTarget targetName
     case command of
       "model" -> Modeler.run function target
       "dump" ->
         -- TODO: implement
         return ()

       "lint" ->
         -- TODO: implement
         return ()

       otherwise ->
         error $ "Unrecognized command: " ++ show command
