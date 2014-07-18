--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.ProgramModules.LLVM
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Language.InstSel.ProgramModules.LLVM (
  module Language.InstSel.ProgramModules.LLVM.FunctionMaker
, module Language.InstSel.ProgramModules.LLVM.Lowerer
) where

import Language.InstSel.ProgramModules.LLVM.FunctionMaker
import Language.InstSel.ProgramModules.LLVM.Lowerer
