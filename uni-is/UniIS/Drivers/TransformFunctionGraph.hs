{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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



-------------
-- Functions
-------------

run :: TransformAction
    -> Function
    -> Maybe TargetMachine
    -> IO [Output]

run RemoveDeadCodeInFunctionGraph f _ =
  do let new_f = removeDeadCode f
     return [toOutput $ toJson new_f]

run EnforcePhiNodeInvariantsInFunctionGraph f _ =
  do let new_f = enforcePhiNodeInvariants f
     return [toOutput $ toJson new_f]

run RemovePhiNodeRedundanciesInFunctionGraph f _ =
  do let new_f = removePhiNodeRedundancies f
     return [toOutput $ toJson new_f]

run LowerPointersInFunctionGraph f (Just tm) =
  do let new_f = lowerPointers tm f
     return [toOutput $ toJson new_f]

run CopyExtendFunctionGraph f _ =
  do let new_f = copyExtend f
     return [toOutput $ toJson new_f]

run BranchExtendFunctionGraph f _ =
  do let new_f = branchExtend f
     return [toOutput $ toJson new_f]

run CombineConstantsInFunctionGraph f _ =
  do let new_f = combineConstants f
     return [toOutput $ toJson new_f]

run _ _ _ = reportErrorAndExit "TransformFunctionGraph: unsupported action"
