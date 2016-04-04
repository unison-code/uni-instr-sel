--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Targets.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- A module for retrieving specific target machine data.
--
--------------------------------------------------------------------------------

module UniIS.Targets.Base
  ( retrieveTargetMachine )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.TargetMachines.Transformations
import Language.InstrSel.Utils
  ( toLower )

import qualified UniIS.Targets.Mips32 as Mips32



-------------
-- Functions
-------------

-- | Retrieves a target machine with a specific (case-insensitive) ID. If no
-- machine exists with such an identifier, 'Nothing' is returned.
retrieveTargetMachine :: TargetMachineID -> Maybe TargetMachine
retrieveTargetMachine tmid =
  do let machines = map (\t -> (convertID $ tmID t, t))
                        [ Mips32.theTM ]
     tm <- lookup (convertID tmid) machines
     return $ copyExtend tm
  where convertID tid = toTargetMachineID $ toLower $ fromTargetMachineID tid
