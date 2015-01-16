--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.AssemblyEmitter
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a CP model solution and post parameters, and emits the corresponding
-- assembly code.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.AssemblyEmitter
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.CPModel
  ( mkCPSolutionData )
import Language.InstSel.TargetMachines.CodeEmission

import Data.List
  ( intercalate )


-------------
-- Functions
-------------

run
  :: String
     -- ^ The solution in JSON format.
  -> String
     -- ^ The post parameters in JSON format.
  -> IO [Output]
     -- ^ The produced output.
run s_str p_str =
  do s_data <- parseJson s_str
     p_data <- parseJson p_str
     let cp_data = mkCPSolutionData p_data s_data
         code = generateCode cp_data
         code_str = intercalate "\n" (map showCode code)
     return [toOutputWithoutID code_str]

showCode :: AssemblyCode -> String
showCode (AsmBasicBlockLabel str) = str ++ ":"
showCode (AsmInstruction str) = "  " ++ str
