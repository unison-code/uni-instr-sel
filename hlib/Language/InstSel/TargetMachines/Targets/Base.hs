--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachines.Targets.Base
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

module Language.InstSel.TargetMachines.Targets.Base
  ( retrieveTargetMachine )
where

import Language.InstSel.TargetMachines.Base
import Language.InstSel.TargetMachines.Transformations
import Language.InstSel.Utils
  ( splitOn )

import Language.InstSel.TargetMachines.Targets.Mips32
import Language.InstSel.TargetMachines.Targets.Test



-------------
-- Functions
-------------

-- | Retrieves a target machine with a specific ID. If the name contains a dot
-- ('.'), and the suffix is "ce", then the returned target machine will have
-- been copy-extended. If no machine exists with such an identifier, 'Nothing'
-- is returned.
retrieveTargetMachine :: TargetMachineID -> Maybe TargetMachine
retrieveTargetMachine tmid =
  getTM $ splitOn "." (fromTargetMachineID tmid)
  where getTM [m] = case m of
                      "mips32" -> Just tmMips32
                      "test"   -> Just tmTest
                      _        -> Nothing
        getTM [m, suf] = if suf == "ce"
                         then do tm <- getTM [m]
                                 return $ copyExtend tm
                         else Nothing
        getTM _ = Nothing
