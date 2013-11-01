--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM.PMMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts and LLVM IR module into the internal program format.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.ProgramModules.LLVM.PMMaker (
  mkProgramModule
) where

import qualified LLVM.General.AST as LLVM
import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.ProgramModules.Base as PM
import Language.InstructionSelection.Utils
import Data.Maybe



mkProgramModule :: LLVM.Module -> PM.Module
mkProgramModule m = undefined
-- TODO: implement