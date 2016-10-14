{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.TransformFunctionGraph
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
import Language.InstrSel.Functions.Transformations
import Language.InstrSel.TargetMachines
  ( TargetMachine )
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )
import Language.InstrSel.Utils.JSON
  ( toJson )
import Language.InstrSel.Utils.Natural



-------------
-- Functions
-------------

run :: TransformAction
    -> Function
    -> Maybe TargetMachine
    -> Maybe Natural
    -> IO [Output]

run FixPhisInFunctionGraph f _ _ =
  do let new_f = fixPhis f
     return [toOutput $ toJson new_f]

run LowerPointersInFunctionGraph f (Just tm) _ =
  do let new_f = lowerPointers tm f
     return [toOutput $ toJson new_f]

run CopyExtendFunctionGraph f _ _ =
  do let new_f = copyExtend f
     return [toOutput $ toJson new_f]

run BranchExtendFunctionGraph f _ _ =
  do let new_f = branchExtend f
     return [toOutput $ toJson new_f]

run CombineConstantsInFunctionGraph f _ _ =
  do let new_f = combineConstants f
     return [toOutput $ toJson new_f]

run AlternativeExtendFunctionGraph f _ (Just limit) =
  do let new_f = alternativeExtend limit f
     return [toOutput $ toJson new_f]

run _ _ _ _ = reportErrorAndExit "TransformFunctionGraph: unsupported action"
