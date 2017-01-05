{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeArrayIndexMaplists
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadTargetMachine )
import Language.InstrSel.ConstraintModels
  ( HighLevelModel (..)
  , HighLevelMachineParams (..)
  )
import Language.InstrSel.ConstraintModels.ArrayIndexMaplistsMaker
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> HighLevelModel -> IO [Output]

run MakeArrayIndexMaplists function model =
  do tm <- loadTargetMachine (hlMachineID $ hlMachineParams model)
     let mapset = mkArrayIndexMaplists function tm model
     return [toOutput $ toJson mapset]

run _ _ _ = reportErrorAndExit "MakeArrayIndexMaplists: unsupported action"
