--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakeCPModelWOp
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph and pattern matchset as input, and constructs a CP
-- model instance.
--
--------------------------------------------------------------------------------

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
