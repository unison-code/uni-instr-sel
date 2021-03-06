{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>
  Roberto Castaneda Lozano <rcas@sics.se>

-}

module UniIS.Drivers.TransformCPSolution
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadFromJson )
import Language.InstrSel.ConstraintModels
import Language.InstrSel.ConstraintModels.SolutionHandler
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run
  :: TransformAction
  -> BS.ByteString
     -- ^ The content of the (low-level or high-level) CP solution file.
  -> BS.ByteString
     -- ^ The content of the high-level CP model file.
  -> ArrayIndexMaplists
  -> IO [Output]

run RaiseLowLevelCPSolution str mstr ai_maps =
  do sol <- loadFromJson str
     model <- loadFromJson mstr
     let new_sol = raiseLowLevelSolution sol model ai_maps
     return [toOutput $ toJson new_sol]

run _ _ _ _ = reportErrorAndExit "TransformCPSolution: unsupported action"
