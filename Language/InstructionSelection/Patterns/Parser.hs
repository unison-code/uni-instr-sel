--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.Patterns.Parser
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Parses a file of instruction patterns formatted as S-expressions. The
-- instruction patterns consists of a set of constraints and one or more
-- patterns expressed as LLVM IR code.
--
--------------------------------------------------------------------------------

module Language.InstSel.Patterns.Parser (
      parsePatterns
    ) where

import Language.InstSel.LLVM.IR
import Language.InstSel.Patterns.Base
import Text.ParserCombinators.Parsec
import Data.String.Utils



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
  do constraints <- labeledData "constraints" allPatternConstraints
     code <- labeledData "code" allLlvmCode
     return (LlvmPattern constraints code)

allPatternConstraints :: GenParser Char st [PatternConstraint]
allPatternConstraints = many patternConstraint

patternConstraint :: GenParser Char st PatternConstraint
patternConstraint = parens patternConstraint'

patternConstraint' :: GenParser Char st PatternConstraint
patternConstraint' =
  do whiteSpace
     (    try allocPatternConstraint
      <|> try immPatternConstraint
      <|> try aliasPatternConstraint
      <|> try assertPatternConstraint)

allocPatternConstraint :: GenParser Char st PatternConstraint
allocPatternConstraint =
  do string "allocate-in"
     whiteSpace
     var <- variable
     whiteSpace
     reg <- registerClass
     whiteSpace
     return (AllocateIn var reg)

immPatternConstraint :: GenParser Char st PatternConstraint
immPatternConstraint =
  do try (string "zimm") <|> try (string "imm")
     whiteSpace
     lower <- many1 digit
     whiteSpace
     upper <- many1 digit
     whiteSpace
     imm <- immediate
     whiteSpace
     return (ImmediateRange imm (read lower) (read upper))

aliasPatternConstraint :: GenParser Char st PatternConstraint
aliasPatternConstraint =
  do string "alias"
     whiteSpace
     temp1 <- temporary
     whiteSpace
     temp2 <- temporary
     whiteSpace
     return (Alias temp1 temp2)

assertPatternConstraint :: GenParser Char st PatternConstraint
assertPatternConstraint =
  do string "assert"
     code <- parens pData
     whiteSpace
     return (Assert code)

allLlvmCode :: GenParser Char st LlvmCode
allLlvmCode = many llvmStatement

llvmStatement :: GenParser Char st LlvmStatement
llvmStatement = parens pData

variable :: GenParser Char st Variable
variable =
  do (    try (do temp <- temporary
                  return (VTemporary temp))
      <|> try (do param <- parameter
                  return (VParameter param)))

temporary :: GenParser Char st Temporary
temporary = labeledData "tmp" temporary'

temporary' :: GenParser Char st Temporary
temporary' =
  do int <- many1 digit
     return (Temporary (read int))

immediate :: GenParser Char st Immediate
immediate =
  do imm <- many1 alphaNum
     return (Immediate (read imm))

parameter :: GenParser Char st Parameter
parameter =
  do param <- many1 alphaNum
     return (Parameter param)

registerClass :: GenParser Char st RegisterClass
registerClass =
  do reg <- many1 (try alphaNum <|> try (char '_'))
     return (RegisterClass reg)

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
