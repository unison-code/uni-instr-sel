--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.TransformCPModel
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a CP model model.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.TransformCPModel
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadFromJson )
import Language.InstrSel.ConstraintModels
import Language.InstrSel.ConstraintModels.ModelHandler
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run
  :: TransformAction
  -> String
     -- ^ The content of the (low-level or high-level) CP model file.
  -> ArrayIndexMaplists
  -> IO [Output]

run LowerHighLevelCPModel str ai_maps =
  do model <- loadFromJson str
     let new_model = lowerHighLevelModel model ai_maps
     return [toOutputWithoutID $ toJson new_model]

run _ _ _ = reportErrorAndExit "TransformCPModel: unsupported action"
