--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.TransformFunctionGraph
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a function graph.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.TransformFunctionGraph
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
import Language.InstrSel.Functions.Transformations
import Language.InstrSel.Utils.JSON
  ( toJson )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: TransformAction -> Function -> IO [Output]

run CopyExtendFunctionGraph f =
  do let new_f = copyExtend f
     return [toOutput $ toJson new_f]

run BranchExtendFunctionGraph f =
  do let new_f = branchExtend f
     return [toOutput $ toJson new_f]

run CombineConstantsInFunctionGraph f =
  do let new_f = combineConstants f
     return [toOutput $ toJson new_f]

run AlternativeExtendFunctionGraph f =
  do let new_f = alternativeExtend f
     return [toOutput $ toJson new_f]

run _ _ = reportErrorAndExit "TransformFunctionGraph: unsupported action"
