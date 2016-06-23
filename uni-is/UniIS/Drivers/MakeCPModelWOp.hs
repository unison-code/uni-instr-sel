{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeCPModelWOp
  ( run )
where

import UniIS.Drivers.Base

import Language.InstrSel.ConstraintModels
  ( HighLevelModelNoOp )
import Language.InstrSel.ConstraintModels.ModelHandler
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..) )
import Language.InstrSel.Utils.JSON

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> HighLevelModelNoOp -> PatternMatchset -> IO [Output]

run MakeHighLevelCPModelWOp model matchset =
  do let new_model = mkHighLevelModelWOp model (pmMatches matchset)
     return [toOutput $ toJson new_model]

run _ _ _ = reportErrorAndExit "MakeCPModel: unsupported action"
