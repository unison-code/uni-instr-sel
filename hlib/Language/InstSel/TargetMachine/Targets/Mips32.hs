--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachine.Targets.Mips32
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- The ISA of 32-bit MIPS.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstSel.TargetMachine.Targets.Mips32
  ( tmMips32 )
where

import Language.InstSel.Constraints
import Language.InstSel.Constraints.PCBuilder
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.ProgramModules.IDs
  ( BasicBlockLabel (..) )
import Language.InstSel.TargetMachine



-------------
-- Functions
-------------

tmMips32 :: TargetMachine
tmMips32 = undefined
