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
