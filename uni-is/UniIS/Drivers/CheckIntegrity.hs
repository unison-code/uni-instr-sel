{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.CheckIntegrity
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , errorExitCode
  )



-------------
-- Functions
-------------

run
  :: CheckAction
  -> Maybe Function
  -> Maybe InstrPattern
  -> IO [Output]

run CheckFunctionIntegrity (Just fun) _ =
  -- TODO: implement
  undefined

run CheckPatternIntegrity _ (Just pat) =
  -- TODO: implement
  undefined

run _ _ _ = reportErrorAndExit "CheckIntegrity: unsupported action"
