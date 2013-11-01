--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Programs.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing programs. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other programs forms, such as those based on LLVM, will be converted
-- into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Programs.Base (
  Program (..)
) where

import Language.InstructionSelection.OperationStructures



data Program
    = Program {
          functions :: [OpStructure]
      }
    deriving (Show)
