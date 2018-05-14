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

module UniIS.Drivers.MakePatternMatchset
  ( run )
where

import UniIS.Drivers
import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.TargetMachines
  ( TargetMachine )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..), mkPatternMatchset )
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )
import Language.InstrSel.Utils.Time

import Control.DeepSeq
  ( deepseq )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run :: MakeAction -> Function -> TargetMachine -> IO [Output]

run MakePatternMatchset function target =
  do start <- getTime
     let pmset0 = mkPatternMatchset function target
     -- In order to use 'deepseq', all data types used within @pmset0@ must be
     -- an instance of the 'Control.DeepSeq.NFData' type class
     end <- pmset0 `deepseq` getTime
     let time = start `secondsBetween` end
         pmset1 = pmset0 { pmTime = Just time }
     return [toOutput $ toJson pmset1]

run _ _ _ = reportErrorAndExit "MakePatternMatchset: unsupported action"
