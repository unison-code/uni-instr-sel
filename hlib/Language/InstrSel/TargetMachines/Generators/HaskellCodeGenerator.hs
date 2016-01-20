--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a target machine and generators corresponding Haskell code.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator where

import Language.InstrSel.TargetMachines.Base
  ( TargetMachine )



-------------
-- Functions
-------------

generateHaskellCode :: TargetMachine -> String
generateHaskellCode tm =
  -- TODO: implement
  undefined
