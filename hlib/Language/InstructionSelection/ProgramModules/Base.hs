--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing program modules. This is
-- the format on which subsequent preparation for instruction selection will
-- build on (i.e. other programs forms, such as those based on LLVM, will be
-- converted into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.Base (
  Module (..)
, Function (..)
) where

import Language.InstructionSelection.OpStructures



data Function
    = Function {
          functionName :: String
        , functionOS :: OpStructure
      }
    deriving (Show)

data Module
    = Module {
          functions :: [Function]
      }
    deriving (Show)
