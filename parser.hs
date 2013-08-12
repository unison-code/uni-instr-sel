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

import Language.InstructionSelection.Patterns.LLVM.SExprParser
import Language.InstructionSelection.Patterns.LLVM.Legalizer
import Language.InstructionSelection.SExpressions
import Control.Monad
import System.Exit

isLeft (Left _) = True
isLeft _ = False

main =
  do contents <- getContents
     putStr "\n"
     let result = parse contents
     when (isLeft result) $ do let (Left error) = result
                               putStr $ show error
                               exitFailure

     let (Right instructions) = result
         resolved_instructions = map resolveAliases instructions
     putStr $ showSEList resolved_instructions

     putStr "\n"
