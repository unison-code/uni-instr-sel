--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.TransformFunctionGraph
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Driver for executing transformation commands on a function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.TransformFunctionGraph
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions
import Language.InstSel.Functions.Transformations
import Language.InstSel.Utils.JSON
  ( toJson )

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: TransformAction -> Function -> IO [Output]

run CopyExtendFunctionGraph f =
  do let new_f = copyExtend f
     return [toOutputWithoutID $ toJson new_f]

run BranchExtendFunctionGraph f =
  do let new_f = branchExtend f
     return [toOutputWithoutID $ toJson new_f]

run _ _ = reportError "TransformFunctionGraph: unsupported action"
