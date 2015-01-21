--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.TransformCPModel
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a CP model model.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.TransformCPModel
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Drivers.DispatcherTools
  ( loadFromJson )
import Language.InstSel.ConstraintModels
import Language.InstSel.ConstraintModels.ModelHandler
import Language.InstSel.Utils.JSON
  ( toJson )

import Language.InstSel.Utils.IO
  ( reportError )



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

run _ _ _ = reportError "TransformCPModel: unsupported action"
