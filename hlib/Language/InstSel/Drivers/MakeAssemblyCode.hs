--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.MakeAssemblyCode
-- Copyright   : (c) Gabriel Hjort Blindell 2014
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

module Language.InstSel.Drivers.MakeAssemblyCode
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Drivers.DispatcherTools
  ( loadTargetMachine )

import Language.InstSel.ConstraintModels
  ( HighLevelModel (..)
  , HighLevelMachineParams (..)
  , HighLevelSolution
  )
import Language.InstSel.TargetMachines.CodeEmission

import Language.InstSel.Utils.IO
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
