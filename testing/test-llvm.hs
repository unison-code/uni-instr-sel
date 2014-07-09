{-
Copyright (c) 2013-2014, Gabriel Hjort Blindell <ghb@kth.se>
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

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Monad
import Control.Monad.Error
import Data.GraphViz hiding (parse)
import Data.GraphViz.Commands.IO
import Data.Maybe
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Graphs.GraphViz
import Language.InstructionSelection.OpStructures
import qualified Language.InstructionSelection.ProgramModules as PM
import qualified Language.InstructionSelection.ProgramModules.LLVM as LLVMPro
import qualified Language.InstructionSelection.PrettyPrint as MyPP
import Language.InstructionSelection.SExpressions
import LLVM.General
import LLVM.General.AST
import LLVM.General.Context
import LLVM.General.PrettyPrint
import System.Console.CmdArgs
import System.Exit
import System.FilePath



data MyArgs
    = MyArgs {
          llFile :: String
      }
    deriving (Data, Typeable, Show)

parseArgs =
  MyArgs {
    llFile = "" &= argPos 0 &= typFile
  }

isError (Left _) = True
isError _ = False

main :: IO ()
main =
  do MyArgs {..} <- cmdArgs parseArgs
     src <- readFile llFile
     result <- withContext
       $ \context ->
         do runErrorT $ withModuleFromLLVMAssembly context src
              $ \mod -> moduleAST mod
     when (isError result) $ do let (Left e) = result
                                putStrLn $ show e
                                exitFailure
     let (Right ast) = result
     putStrLn "Module AST:"
     putStrLn (showPretty ast)
     putStrLn ""
     let fs = LLVMPro.mkFunctionsFromLlvmModule (LLVMPro.lowerModule ast)
         f = head fs
     putStrLn (show f)
     writeDotFile "test.dot" (toDotGraph $ osGraph $ PM.functionOS f)
     return ()
