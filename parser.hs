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

module Main (
                main,
                parsePatterns
              ) where

import Text.ParserCombinators.Parsec
import Data.String.Utils

type AssemblyCode = String
type LlvmConstraint = String
type LlvmStatement = String
type LlvmCode = [LlvmStatement]
data LlvmPattern = LlvmPattern {
                        constraints :: [LlvmConstraint]
                      , code :: LlvmCode
                   } deriving (Show)
data LlvmInstruction = LlvmInstruction {
                        assembly :: AssemblyCode
                      , patterns :: [LlvmPattern]
                   } deriving (Show)

istrip :: String -> String
istrip = join " " . filter (\x -> x /= "") . split " "

whitespace = " \r\n\t"

llvmInstructions :: GenParser Char st [LlvmInstruction]
llvmInstructions = 
  do instructions <- many llvmInstruction
     eof
     return instructions
     
llvmInstruction :: GenParser Char st LlvmInstruction
llvmInstruction = labeledData "instruction" llvmInstruction'

llvmInstruction' :: GenParser Char st LlvmInstruction
llvmInstruction' =
  do whiteSpace
     assembly <- assemblyCode
     patterns <- many llvmPattern
     return (LlvmInstruction assembly patterns)

assemblyCode :: GenParser Char st AssemblyCode
assemblyCode = many1 (noneOf $ whitespace ++ "(")

llvmPattern :: GenParser Char st LlvmPattern
llvmPattern = labeledData "pattern" llvmPattern'

llvmPattern' :: GenParser Char st LlvmPattern
llvmPattern' =
  do constraints <- labeledData "constraints" llvmAllConstraints
     code <- labeledData "code" llvmAllCode
     return (LlvmPattern constraints code)

llvmAllConstraints :: GenParser Char st [LlvmConstraint]
llvmAllConstraints = many llvmConstraint

llvmConstraint :: GenParser Char st LlvmConstraint
llvmConstraint = parens pData
     
llvmAllCode :: GenParser Char st LlvmCode
llvmAllCode = many llvmStatement

llvmStatement :: GenParser Char st LlvmStatement
llvmStatement = parens pData

labeledData :: String -> GenParser Char st a -> GenParser Char st a
labeledData str p = parens (labeledData' str p)

labeledData' :: String -> GenParser Char st a -> GenParser Char st a
labeledData' str p = 
  do string str
     whiteSpace
     result <- p
     return result

pData :: GenParser Char st String
pData =
  do list <- many1 morePData
     return $ istrip $ Prelude.foldr (++) [] list

morePData :: GenParser Char st String
morePData =
      try (do nested <- parens pData
              return $ ['('] ++ nested ++ [')']
          )
  <|> do whiteSpace
         many1 (noneOf "()")

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf whitespace)

parens :: GenParser Char st a -> GenParser Char st a
parens c = do whiteSpace
              result <- between (char '(') (char ')') c
              whiteSpace
              return result

parsePatterns :: String -> Either ParseError [LlvmInstruction]
parsePatterns input = parse llvmInstructions "" input



main = do
  contents <- getContents
  putStr "\n"
  putStr $ show (parsePatterns contents)
  putStr "\n"
