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

module Parser (
                parseLlvmPatterns
              ) where

import Text.ParserCombinators.Parsec
import Data.String.Utils

type LlvmInstruction = String
type LlvmConstraint = String
type LlvmStatement = String
type LlvmCode = [LlvmStatement]
data LlvmPattern = LlvmPattern {
                        instruction :: LlvmInstruction 
                      , constraints :: [LlvmConstraint]
                      , code :: LlvmCode
                   } deriving (Show)

istrip :: String -> String
istrip = join " " . filter (\x -> x /= "") . split " "

whitespace = " \r\n\t"

llvmPatternFile :: GenParser Char st [LlvmPattern]
llvmPatternFile = 
  do patterns <- many llvmPattern
     eof
     return patterns
     
llvmPattern :: GenParser Char st LlvmPattern
llvmPattern =
  do whiteSpace
     char '('
     inst <- llvmInstruction
     constraints <- llvmAllConstraints
     code <- llvmAllCode
     char ')'
     whiteSpace
     return (LlvmPattern inst constraints code)
  
llvmInstruction :: GenParser Char st LlvmInstruction
llvmInstruction = labeledData "instruction" pData

llvmAllConstraints :: GenParser Char st [LlvmConstraint]
llvmAllConstraints = labeledData "constraints" (many llvmConstraint)

llvmConstraint :: GenParser Char st LlvmConstraint
llvmConstraint = parens pData
     
llvmAllCode :: GenParser Char st LlvmCode
llvmAllCode = labeledData "code" (many llvmStatement)

llvmStatement :: GenParser Char st LlvmStatement
llvmStatement = parens pData

labeledData :: String -> GenParser Char st a -> GenParser Char st a
labeledData str p =
  do whiteSpace
     string str
     whiteSpace
     result <- parens p
     whiteSpace
     return result

pData :: GenParser Char st String
pData =
  do list <- many1 morePData
     return $ istrip $ Prelude.foldr (++) [] list

morePData :: GenParser Char st String
morePData =
  do first <- many1 (noneOf "()")
     nested <- nestedPData
     return $ first ++ nested

nestedPData :: GenParser Char st String
nestedPData =
      do char '('
         pdata <- pData
         char ')'
         return $ ['('] ++ pdata ++ [')']
  <|> (return "")

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf whitespace)

parens :: GenParser Char st a -> GenParser Char st a
parens c = do whiteSpace
              result <- between (char '(') (char ')') c
              whiteSpace
              return result

parseLlvmPatterns :: String -> Either ParseError [LlvmPattern]
parseLlvmPatterns input = parse llvmPatternFile "" input



main = do
  contents <- getContents
  putStr "\n"
  putStr $ show (parseLlvmPatterns contents)
  putStr "\n"
