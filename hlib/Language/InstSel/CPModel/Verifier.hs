--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.Verifier
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains functions for doing sanity checks on the CP model.
--
--------------------------------------------------------------------------------

module Language.InstSel.CPModel.Verifier
  ( Errors (..)
  , foundErrors
  , printAndDie
  , sanityCheck
  )
where



import Language.InstSel.CPModel.Base
import qualified Language.InstSel.Graphs as G
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
import System.Exit



--------------
-- Data types
--------------

data Errors
    = Errors { errors :: [String] }
    | NoErrors



-------------
-- Functions
-------------

-- | Concatenates two error lists.
(<+>) :: Errors -> Errors -> Errors
(<+>) NoErrors es = es
(<+>) es NoErrors = es
(<+>) (Errors { errors = es1 }) (Errors { errors = es2 }) =
  Errors { errors = es1 ++ es2 }

-- | Checks whether any errors were found.
foundErrors :: Errors -> Bool
foundErrors NoErrors = False
foundErrors _ = True

-- | Prints all errors and then exists.
printAndDie :: Errors -> IO ()
printAndDie NoErrors = exitSuccess
printAndDie Errors { errors = es } =
  do mapM_ (\s -> putStrLn $ "ERROR: " ++ s) es
     exitFailure

-- | Performs a series of sanity checks on the given function and CP model.
sanityCheck :: Function -> CPModelParams -> Errors
sanityCheck = areAllOpNodesCoverable

-- | Checks that every operation node in the function graph has at least one
-- match that can cover it.
areAllOpNodesCoverable :: Function -> CPModelParams -> Errors
areAllOpNodesCoverable f cp =
  let g = osGraph $ functionOS f
      op_nodes = funcOpNodes $ functionData cp
      match_data = matchData cp
      checkHasMatchToCover n =
        let matches = filter (\m -> n `elem` mOperationsCovered m) match_data
            ns = G.getNodeWithNodeID g n
            nt_str = if length ns > 0
                     then show $ head ns
                     else "?"
        in if length matches > 0
           then NoErrors
           else Errors { errors = [ "Op node " ++ show n
                                    ++ " (" ++ nt_str ++ ") "
                                    ++ "is not covered by any match!"
                                  ]
                       }
  in foldl (\es n -> es <+> checkHasMatchToCover n) NoErrors op_nodes
