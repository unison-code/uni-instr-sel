--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachine.Targets.Base
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

module Language.InstSel.TargetMachine.Targets.Base
  ( getTargetMachine )
where

import Language.InstSel.TargetMachine.Base
import Language.InstSel.TargetMachine.IDs

import Language.InstSel.TargetMachine.Targets.Test


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
