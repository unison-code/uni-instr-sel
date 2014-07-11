--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.TargetMachine.Targets
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for retrieving specific target machine data.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstructionSelection.TargetMachine.Targets
  (getTargetMachine)
where

import Language.InstructionSelection.TargetMachine.Base
import Language.InstructionSelection.TargetMachine.IDs



-------------
-- Functions
-------------

-- | Retrieves a specific target machine. If no machine exists with such an
-- identifier, 'Nothing' is returned.

getTargetMachine :: TargetMachineID -> Maybe TargetMachine
getTargetMachine s =
  case (fromTargetMachineID s) of
    "test" -> Just tmTest
    _ -> Nothing

tmTest :: TargetMachine
tmTest =
  TargetMachine
  (toTargetMachineID "test")
  []
  []
