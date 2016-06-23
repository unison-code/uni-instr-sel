{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeAssemblyCode
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Drivers.DispatcherTools
  ( loadTargetMachine )

import Language.InstrSel.ConstraintModels
  ( HighLevelModelWOp (..)
  , HighLevelMachineParams (..)
  , HighLevelSolution
  )
import Language.InstrSel.TargetMachines.CodeEmission

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> HighLevelModelWOp -> HighLevelSolution -> IO [Output]

run MakeAssemblyCode model sol =
  do target <- loadTargetMachine $ hlMachineID $ hlWOpMachineParams model
     let code = generateCode target model sol
         code_str = concat $ map (\c -> showCode c ++ "\n") code
     return [toOutput code_str]

run _ _ _ = reportErrorAndExit "MakeArrayIndexMaplists: unsupported action"

-- | Flattens the assembly code into a string.
showCode :: AssemblyCode -> String
showCode (AsmBlock str) = str ++ ":"
showCode (AsmInstruction str) = "  " ++ str
