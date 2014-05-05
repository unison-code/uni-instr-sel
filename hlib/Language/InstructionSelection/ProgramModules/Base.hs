--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.Base
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
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.Base (
  Function (..)
) where

import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OpStructures
import Data.Maybe



-- | The record of representing a program function.

data Function
    = Function {

          -- | The function name.

          functionName :: String

          -- | If the function returns some data, the return type is specified
          -- through the node in the operation structure that represents the
          -- return data.

        , functionRetType :: Maybe NodeId

          -- | The nodes in the operation structure which represent the input
          -- arguments to the function.

        , functionArgs :: [NodeId]

          -- | The semantics of the function.

        , functionOS :: OpStructure

      }
    deriving (Show)
