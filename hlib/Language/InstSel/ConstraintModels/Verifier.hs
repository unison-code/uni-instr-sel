--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ConstraintModels.Verifier
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

module Language.InstSel.ConstraintModels.Verifier
  ( Errors (..)
  , hasErrors
  , printErrors
  , sanityCheck
  )
where

import Language.InstSel.ConstraintModels.Base
import Language.InstSel.OpStructures
import Language.InstSel.Functions

import qualified Language.InstSel.Graphs as G



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
hasErrors :: Errors -> Bool
hasErrors NoErrors = False
hasErrors _ = True

-- | Prints all errors as a string.
printErrors :: Errors -> String
printErrors NoErrors = ""
printErrors Errors { errors = es } =
  foldl (\rest s -> rest ++ "ERROR: " ++ s ++ "\n") "" es

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
                     then show $ G.getNodeType $ head ns
                     else "?"
        in if length matches > 0
           then NoErrors
           else Errors { errors = [ "Op node " ++ show n
                                    ++ " (" ++ nt_str ++ ") "
                                    ++ "is not covered by any match!"
                                  ]
                       }
  in foldl (\es n -> es <+> checkHasMatchToCover n) NoErrors op_nodes
