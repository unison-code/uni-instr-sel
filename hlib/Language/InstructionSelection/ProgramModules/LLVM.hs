--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.LLVM (
  module Language.InstructionSelection.ProgramModules.LLVM.PMMaker
, module Language.InstructionSelection.ProgramModules.LLVM.Lowerer
) where

import Language.InstructionSelection.ProgramModules.LLVM.PMMaker
import Language.InstructionSelection.ProgramModules.LLVM.Lowerer
