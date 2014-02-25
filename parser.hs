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

processOpStructure :: OpStructure -> IO ()
processOpStructure os =
  do putStrLn "After make:"
     putStrLn (show os)
     putStrLn ""
     let resolved_os = resolveAliases os
     putStrLn "After alias resolving:"
     putStrLn (show resolved_os)
     putStrLn ""
     let normalized_os = normalize resolved_os
     putStrLn "After normalization:"
     putStrLn (show normalized_os)
     putStrLn ""
     let dot = graphToDot params $ intGraph $ graph normalized_os
     writeDotFile "test.dot" dot
     return ()

main :: IO ()
main =
  do MyArgs {..} <- cmdArgs parseArgs
     src <- readFile $ llFile
     result <- withContext $ \context ->
       do runErrorT $ withModuleFromLLVMAssembly context src $ \mod -> moduleAST mod
     when (isError result) $ do let (Left e) = result
                                putStrLn $ show e
                                exitFailure
     let (Right ast) = result
     putStrLn "Module AST:"
     putStrLn (showPretty ast)
     putStrLn ""
     let lowered_ast = LLVMPro.lower ast
     putStrLn "Lowered module AST:"
     putStrLn (showPretty lowered_ast)
     putStrLn ""
     let m = LLVMPro.mkProgramModule lowered_ast
     processOpStructure $ PM.getFunctionOS $ head $ PM.getFunctions m
     return ()

getPatterns :: LLVMPat.Instruction -> [LLVMPat.Pattern]
getPatterns (LLVMPat.Instruction _ ps) = ps

--main :: IO ()
--main =
--  do contents <- getContents
--     putStrLn ""
--     let result = LLVMPat.parse contents
--     when (isError result) $ do let (Left e) = result
--                                putStr $ show e
--                                exitFailure
--
--     let (Right llvm_insts) = result
--     putStrLn "After parsing:"
--     putStrLn $ show result
--     putStrLn ""
--     let insts = map LLVMPat.mkInstruction llvm_insts
--     putStrLn "After mkinstruction:"
--     putStrLn $ show insts
--     putStrLn ""
--     let os = head $ Pat.patterns $ head insts
--     processOpStructure os

params = nonClusteredParams { fmtNode = nodeAttr }
nodeAttr n@(_, (NodeLabel _ (NodeInfo (NTData _) _ _))) =
  [makeLabel n, shape BoxShape]
nodeAttr n@(_, (NodeLabel _ (NodeInfo _ _ _))) = [makeLabel n]
makeLabel (_, (NodeLabel i (NodeInfo nt (BBLabel l) str))) =
  let topstr = if isDataNodeType nt
                  then let NTData dt = nt
                           typestr = if isJust dt
                                        then MyPP.prettyShow $ fromJust dt
                                        else "?"
                       in typestr ++ ":" ++ str
                  else str
  in toLabel $ topstr ++ "\n" ++ show i ++ " : " ++ l
