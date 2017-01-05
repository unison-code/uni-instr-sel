{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
  ( PatternMatchset (..) )
import Language.InstrSel.TargetMachines.Transformations
  ( alternativeExtend )
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )
import Language.InstrSel.Utils.JSON
  ( toJson )
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.Time

import Control.DeepSeq
  ( deepseq )

import Data.Maybe
  ( isJust
  , fromJust
  )



-------------
-- Functions
-------------

run :: TransformAction
    -> Function
    -> TargetMachine
    -> PatternMatchset
    -> Natural
    -> IO [Output]

run AlternativeExtendPatternMatchset f t pmset0 limit =
  do start <- getTime
     let pmset1 = alternativeExtend f t limit pmset0
     end <- pmset1 `deepseq` getTime
     let time = start `secondsBetween` end
         old_time = pmTime pmset0
         new_time = if isJust old_time
                    then (fromJust old_time) + time
                    else time
         pmset2 = pmset1 { pmTime = Just new_time }
     return [toOutput $ toJson pmset2]

run _ _ _ _ _ =
  reportErrorAndExit "TransformPatternMatchset: unsupported action"
