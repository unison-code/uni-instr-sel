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

import qualified Language.InstructionSelection.Patterns.LLVM as LLVM
import Language.InstructionSelection.SExpressions
import Language.InstructionSelection.Patterns.LLVM.OSMaker
import Language.InstructionSelection.Patterns.LLVM.SExprParser
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OperationStructures
import Control.Monad
import System.Exit
import Data.GraphViz hiding (parse)
import Data.GraphViz.Commands.IO

import System.FilePath
import System.Console.CmdArgs
import LLVM.General
import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.PrettyPrint
import Control.Monad.Error
import Debug.Trace

data MyArgs
    = MyArgs {
          llFile :: String
      }
    deriving (Data, Typeable, Show)

parseArgs =
  MyArgs {
    llFile = "" &= typFile
  }

isError (Left _) = True
isError _ = False

main :: IO ()
main =
  do MyArgs {..} <- cmdArgs parseArgs
     src <- readFile llFile
     result <- withContext $ \context ->
       do runErrorT $ withModuleFromString context src $ \mod -> moduleAST mod
     when (isError result) $ do let (Left e) = result
                                putStr $ show e
                                exitFailure
     let (Right ast) = result
     putStrLn (showPretty ast)
     return ()

--isError :: Either ParseError [LLVM.Instruction] -> Bool
--isError (Left _) = True
--isError _ = False
--
--getPatterns :: LLVM.Instruction -> [LLVM.Pattern]
--getPatterns (LLVM.Instruction _ ps) = ps
--
----main :: IO ()
--main =
--  do contents <- getContents
--     putStr "\n"
--     let result = parse contents
--     when (isError result) $ do let (Left e) = result
--                                putStr $ show e
--                                exitFailure
--
--     let (Right instructions) = result
--         llvm_patterns = concat $ map getPatterns instructions
--         ops = map mkOpStructure llvm_patterns
--         resolved_ops = map resolveAliases ops
--         normalized_ops = map normalize resolved_ops
--         dots = map (graphToDot params . intGraph . graph) normalized_ops
--     putStr "After make:\n"
--     putStr (show ops)
--     putStr "\n\n"
--     putStr "After alias resolving:\n"
--     putStr (show resolved_ops)
--     putStr "\n\n"
--     putStr "After normalization:\n"
--     putStr (show normalized_ops)
--     putStr "\n\n"
--     mapM_ (writeDotFile "test.dot") dots

params = nonClusteredParams { fmtNode = nodeAttr }
nodeAttr n@(_, (NodeLabel _ (NodeInfo NTData _ _))) =
  [makeLabel n, shape BoxShape]
nodeAttr n@(_, (NodeLabel _ (NodeInfo _ _ _))) = [makeLabel n]
makeLabel (_, (NodeLabel _ (NodeInfo _ (BBLabel l) str))) =
  toLabel (l ++ ":" ++ str)