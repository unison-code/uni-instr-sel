{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.TransformCPModel
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadFromJson )
import Language.InstrSel.ConstraintModels
import Language.InstrSel.ConstraintModels.ModelHandler
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run
  :: TransformAction
  -> BS.ByteString
     -- ^ The content of the (low-level or high-level) CP model file.
  -> ArrayIndexMaplists
  -> IO [Output]

run LowerHighLevelCPModel str ai_maps =
  do model <- loadFromJson str
     let new_model = lowerHighLevelModel model ai_maps
     return [toOutput $ toJson new_model]

run _ _ _ = reportErrorAndExit "TransformCPModel: unsupported action"
