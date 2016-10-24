{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.TransformPatternMatchset
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
import Language.InstrSel.TargetMachines
  ( TargetMachine )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset )
import Language.InstrSel.TargetMachines.Transformations
  ( alternativeExtend )
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
    -> TargetMachine
    -> PatternMatchset
    -> Natural
    -> IO [Output]

run AlternativeExtendPatternMatchset f t p limit =
  do let new_p = alternativeExtend f t limit p
     return [toOutput $ toJson new_p]

run _ _ _ _ _ =
  reportErrorAndExit "TransformPatternMatchset: unsupported action"
