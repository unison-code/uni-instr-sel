{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Targets.Base
  ( retrieveTargetMachine )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.Utils.String
  ( toLower )

import qualified UniIS.Targets.Mips32 as Mips32
import qualified UniIS.Targets.Hexagon5 as Hexagon5
import qualified UniIS.Targets.X86Minus64 as X86Minus64



-------------
-- Functions
-------------

-- | Retrieves a target machine with a specific (case-insensitive) ID. If no
-- machine exists with such an identifier, 'Nothing' is returned.
retrieveTargetMachine :: TargetMachineID -> Maybe TargetMachine
retrieveTargetMachine tmid =
  do let machines = map (\t -> (convertID $ tmID t, t))
                        [ Mips32.theTM
                        , Hexagon5.theTM
                        , X86Minus64.theTM
                        ]
     tm <- lookup (convertID tmid) machines
     return tm
  where convertID tid = toTargetMachineID $ toLower $ fromTargetMachineID tid
