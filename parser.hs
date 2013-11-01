{-
Copyright (c) 2013, Gabriel Hjort Blindell <ghb@kth.se>
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
Parses a file of S-expressions containing a set of instruction patterns
expressed as LLVM IR statements. The LLVM IR statements of each instruction is
then transformed into a corresponding DAG and then output as S-expressions.
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import Control.Monad
import Control.Monad.Error
import Data.GraphViz hiding (parse)
import Data.GraphViz.Commands.IO
import Data.Maybe
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OperationStructures
import qualified Language.InstructionSelection.Patterns as Pat
import qualified Language.InstructionSelection.Patterns.LLVM as LLVMPat
import qualified Language.InstructionSelection.ProgramModules as PM
import qualified Language.InstructionSelection.ProgramModules.LLVM as LLVMPro
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
          llFile :: Maybe String
      }
    deriving (Data, Typeable, Show)

parseArgs =
  MyArgs {
    llFile = Nothing &= typFile &= help "LLVM IR file (mandatory)"
  }

isError (Left _) = True
isError _ = False

processOpStructure :: OpStructure -> IO ()
processOpStructure os =
  do let resolved_os = resolveAliases os
         normalized_os = normalize resolved_os
         dot = graphToDot params $ intGraph $ graph normalized_os
     putStrLn "After make:"
     putStrLn (show os)
     putStrLn ""
     putStrLn "After alias resolving:"
     putStrLn (show resolved_os)
     putStrLn ""
     putStrLn "After normalization:"
     putStrLn (show normalized_os)
     putStrLn ""
     writeDotFile "test.dot" dot
     return ()

--main :: IO ()
--main =
--  do MyArgs {..} <- cmdArgs parseArgs
--     when (isNothing llFile) $ do putStrLn "No LLVM IR file"
--                                  exitFailure
--     src <- readFile $ fromJust llFile
--     result <- withContext $ \context ->
--       do runErrorT $ withModuleFromString context src $ \mod -> moduleAST mod
--     when (isError result) $ do let (Left e) = result
--                                putStrLn $ show e
--                                exitFailure
--     let (Right ast) = result
--     putStrLn "Module AST:"
--     putStrLn (showPretty ast)
--     putStrLn ""
--     let m = LLVMPro.mkProgramModule ast
--     processOpStructures $ PM.functions m
--     return ()

getPatterns :: LLVMPat.Instruction -> [LLVMPat.Pattern]
getPatterns (LLVMPat.Instruction _ ps) = ps

--main :: IO ()
main =
  do contents <- getContents
     putStrLn ""
     let result = LLVMPat.parse contents
     when (isError result) $ do let (Left e) = result
                                putStr $ show e
                                exitFailure

     let (Right llvm_insts) = result
         insts = map LLVMPat.mkInstruction llvm_insts
         os = head $ Pat.patterns $ head insts
     processOpStructure os

params = nonClusteredParams { fmtNode = nodeAttr }
nodeAttr n@(_, (NodeLabel _ (NodeInfo NTData _ _))) =
  [makeLabel n, shape BoxShape]
nodeAttr n@(_, (NodeLabel _ (NodeInfo _ _ _))) = [makeLabel n]
makeLabel (_, (NodeLabel _ (NodeInfo _ (BBLabel l) str))) =
  toLabel (l ++ ":" ++ str)