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
The main driver for invoking various Unison commands related to instruction
selection.
-}

{-# LANGUAGE DeriveDataTypeable #-}

import qualified Language.InstSel.Drivers.Modeler as Modeler
import Language.InstSel.ProgramModules
  ( Function )
import Language.InstSel.ProgramModules.LLVM
  ( mkFunctionsFromLlvmModule )
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.TargetMachines.IDs
import Language.InstSel.TargetMachines.Targets
  ( getTargetMachine )
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
import LLVM.General
import LLVM.General.Context
import System.Console.CmdArgs
import System.Directory
  ( doesFileExist )



---------------------------------
-- Help functions and data types
---------------------------------

data Options
    = Options
        { command :: String
        , inFile :: Maybe String
        , outFile :: Maybe String
        , targetName :: Maybe String
        }
    deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { command = def
        &= argPos 0
        &= typ "COMMAND"
    , inFile = def
        &= help "File that contains the input."
        &= typFile
        &= explicit
        &= name "i"
        &= name "input"
    , outFile = Nothing
        &= help "File for writing the result."
        &= typFile
        &= explicit
        &= name "o"
        &= name "output"
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
    summary ( "Unison (instruction selection) tool\n\n"
              ++
              "Gabriel Hjort Blindell   ghb@kth.se"
            )

getFunctionFromLLVM :: Options -> IO (Function)
getFunctionFromLLVM opts =
  do let file = inFile opts
     when (isNothing file) $
       error "No LLVM IR file is provided as input."
     exists_file <- doesFileExist $ fromJust file
     when (not exists_file) $
       error $ "File " ++ show (fromJust file) ++ " does not exist."
     llvm_src <- readFile $ fromJust file
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

getTarget :: Options -> IO (TargetMachine)
getTarget opts =
  do let tname = targetName opts
     when (isNothing tname) $
       error "No target provided."
     let target = getTargetMachine $ toTargetMachineID $ fromJust tname
     when (isNothing target) $
       error $ "Unrecognized target: " ++ (show $ fromJust tname)
     return $ fromJust target

-- | If an output file is given as part of the options, then the returned
-- function will write all data to the output file. Otherwise the data will be
-- written to 'STDOUT'.
getEmitFunction :: Options -> IO (String -> IO ())
getEmitFunction opts =
  do let file = outFile opts
     if isNothing file
     then return putStrLn
     else return $ writeFile (fromJust file)



----------------
-- Main program
----------------

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     case command opts of
       "model" -> do function <- getFunctionFromLLVM opts
                     target <- getTarget opts
                     emitter <- getEmitFunction opts
                     Modeler.run function target emitter
       "dump" ->
         -- TODO: implement
         return ()

       "lint" ->
         -- TODO: implement
         return ()

       cmd -> error $ "Unrecognized command: " ++ show cmd
