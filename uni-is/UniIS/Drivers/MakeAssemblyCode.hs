{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
  ( HighLevelModel (..)
  , HighLevelMachineParams (..)
  , HighLevelSolution
  )
import Language.InstrSel.TargetMachines.CodeEmission

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.List
  ( intercalate )



-------------
-- Functions
-------------

run :: MakeAction -> HighLevelModel -> HighLevelSolution -> IO [Output]

run MakeAssemblyCode model sol =
  do target <- loadTargetMachine $ hlMachineID $ hlMachineParams model
     let code = generateCode target model sol
         code_str = concat $ map (\c -> showCode c ++ "\n") code
     return [toOutput code_str]

run _ _ _ = reportErrorAndExit "MakeArrayIndexMaplists: unsupported action"

-- | Flattens the assembly code into a string.
showCode :: AssemblyCode -> String
showCode b@(AsmBlock {}) =
  let freq_int = (fromIntegral $ asmExecFreq b) :: Integer
  in alignString 3 (show freq_int) ++ "  " ++ asmString b ++ ":"
showCode i@(AsmInstruction {}) =
  alignString 3 (asmLatency i) ++ "  " ++
  "  " ++
  "[" ++ intercalate ", " (asmOutput i) ++ "] <- " ++
  "\"" ++ asmString i ++ "\"" ++
  " <- [" ++ intercalate ", " (asmInput i) ++ "]"

-- | Prints a string with right-justified alignment.
alignString
  :: Int
     -- ^ Desired width of string.
  -> String
     -- ^ String to align
  -> String
alignString w s =
  let pad = take (w - length s) $ repeat ' '
  in pad ++ s
