import Text.ParserCombinators.Parsec
import Data.Text

type LlvmInstruction = String
type LlvmConstraint = String
type LlvmStatement = String
type LlvmCode = [LlvmStatement]
data LlvmPattern = LlvmPattern {
                        instruction :: LlvmInstruction 
                      , constraints :: [LlvmConstraint]
                      , code :: LlvmCode
                   } deriving (Show)

trim :: String -> String
trim = unpack . strip . pack

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
llvmInstruction = 
  do inst <- labeledData "instruction" (many (noneOf ")"))
     return (trim inst)

llvmAllConstraints :: GenParser Char st [LlvmConstraint]
llvmAllConstraints = labeledData "constraints" (many llvmConstraint)

llvmConstraint :: GenParser Char st LlvmConstraint
llvmConstraint =
  do constraint <- parens (many (noneOf ")"))
     return (trim constraint)
     
llvmAllCode :: GenParser Char st LlvmCode
llvmAllCode = labeledData "code" (many llvmStatement)

llvmStatement :: GenParser Char st LlvmStatement
llvmStatement =
  do statement <- parens (many (noneOf ")"))
     return (trim statement)

labeledData :: String -> GenParser Char st a -> GenParser Char st a
labeledData str p =
  do whiteSpace
     string str
     result <- parens p
     return result

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf whitespace)

parens :: GenParser Char st a -> GenParser Char st a
parens c = do whiteSpace
              result <- between (char '(') (char ')') c
              whiteSpace
              return result

parseLlvmPatterns :: String -> Either ParseError [LlvmPattern]
parseLlvmPatterns input = parse llvmPatternFile "" input
