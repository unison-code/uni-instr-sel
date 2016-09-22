{-# LANGUAGE DeriveDataTypeable #-}

import Language.InstrSel.Functions
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.OpStructures
import Language.InstrSel.Utils
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO

import System.Console.CmdArgs
import System.Console.CmdArgs.Text

import Data.Maybe
  ( fromJust )

data Options
  = Options { file :: String }
  deriving (Data, Typeable)

parseArgs :: Options
parseArgs =
  Options
    { file = def
        &= argPos 0
    }

loadFromJson :: (FromJSON a) => String -> IO a
loadFromJson str =
  do let res = fromJson str
     when (isLeft res) $
       reportErrorAndExit $ fromLeft res
     return $ fromRight res

mkGenericValueNodeType :: NodeType
mkGenericValueNodeType = ValueNode { typeOfValue = AnyType
                                   , originOfValue = Nothing
                                   }

mkGenericBlockNodeType :: NodeType
mkGenericBlockNodeType = BlockNode mkEmptyBlockName

mkPattern :: Graph
mkPattern =
  mkGraph ( map Node $
            [ ( 0, NodeLabel 0 PhiNode )
            , ( 1, NodeLabel 1 mkGenericValueNodeType )
            , ( 2, NodeLabel 2 mkGenericValueNodeType )
            , ( 3, NodeLabel 3 mkGenericValueNodeType )
            , ( 4, NodeLabel 4 mkGenericBlockNodeType )
            , ( 5, NodeLabel 5 mkGenericBlockNodeType )
--            , ( 6, NodeLabel 6 mkGenericBlockNodeType )
            ]
          )
          ( map Edge $
            [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
            , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
            , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
            , ( 1, 4, EdgeLabel DefEdge 0 0 )
            , ( 2, 5, EdgeLabel DefEdge 0 0 )
            , ( 4, 3, EdgeLabel DefEdge 0 0 )
            ]
          )

main :: IO ()
main =
  do opts <- cmdArgs parseArgs
     str <- readFileContent $ file opts
     f <- loadFromJson str
     let fg = osGraph $ functionOS f
         ms = findMatches fg mkPattern
     putStrLn ""
     putStrLn $ show ms
     putStrLn ""
