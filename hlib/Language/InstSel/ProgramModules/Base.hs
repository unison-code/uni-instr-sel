--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.ProgramModules.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing program modules, which
-- basically consist of a list of functions. This is the format on which
-- subsequent preparation for instruction selection will build on (i.e. other
-- programs forms, such as those based on LLVM, will be converted into this
-- format).
--
-- Since only the function name is retained, the names of overloaded functions
-- must have been resolved such that each is given a unique name.
--
--------------------------------------------------------------------------------

module Language.InstSel.ProgramModules.Base
  ( Function (..) )
where

import Language.InstSel.Graphs
  ( NodeID )
import Language.InstSel.OpStructures



-- | The record of representing a program function.
data Function =
    Function { functionName :: Maybe String
               -- ^ The function name.

             , functionOS :: OpStructure
               -- ^ The semantics of the function.

             , functionInputs :: [NodeID]
               -- ^ The IDs of the data nodes in the operation structure which
               -- represent the function input arguments. The order of the list
               -- is the same as the order specified in the original code from
               -- which the semantics have been derived.
             }
  deriving (Show)
