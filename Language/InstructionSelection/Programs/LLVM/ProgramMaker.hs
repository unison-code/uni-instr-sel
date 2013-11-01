--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Programs.LLVM.ProgramMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts LLVM IR code into the internal program format.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Programs.LLVM.ProgramMaker (
  mkProgram
) where

import qualified LLVM.General.AST as LLVM
import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.Programs.Base as P
import Language.InstructionSelection.Utils
import Data.Maybe



mkProgram :: LLVM.Module -> P.Program
mkProgram m = undefined
