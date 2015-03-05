--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.MakeAssemblyCode
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a high-level CP model instance and solution as input, and emits the
-- corresponding assembly code.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.MakeAssemblyCode
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Drivers.DispatcherTools
  ( loadTargetMachine )

import Language.InstrSel.ConstraintModels
  ( HighLevelModel (..)
  , HighLevelMachineParams (..)
  , HighLevelSolution
  )
import Language.InstrSel.TargetMachines.CodeEmission

import Language.InstrSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: MakeAction -> HighLevelModel -> HighLevelSolution -> IO [Output]

run MakeAssemblyCode model sol =
  do target <- loadTargetMachine $ hlMachineID $ hlMachineParams model
     let code = generateCode target model sol
         code_str = concat $ map (\c -> showCode c ++ "\n") code
     return [toOutputWithoutID code_str]

run _ _ _ = reportError "MakeArrayIndexMaplists: unsupported action"

-- | Flattens the assembly code into a string.
showCode :: AssemblyCode -> String
showCode (AsmBasicBlockLabel str) = str ++ ":"
showCode (AsmInstruction str) = "  " ++ str
