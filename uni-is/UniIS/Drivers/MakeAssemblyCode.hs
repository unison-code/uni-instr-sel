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

import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.ByteStringBuilder
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> HighLevelModel -> HighLevelSolution -> IO [Output]

run MakeAssemblyCode model sol =
  do target <- loadTargetMachine $ hlMachineID $ hlMachineParams model
     let code = generateCode target model sol
         code_str = mconcat $ map (\c -> buildCode c <> stringUtf8 "\n") code
     return [toOutput $ toLazyByteString code_str]

run _ _ _ = reportErrorAndExit "MakeArrayIndexMaplists: unsupported action"

-- | Flattens the assembly code into a string.
buildCode :: AssemblyCode -> Builder
buildCode b@(AsmBlock {}) =
  let freq_int = (fromIntegral $ asmExecFreq b) :: Integer
  in alignString 3 (BS.pack $ show freq_int) <>
     stringUtf8 "  " <>
     stringUtf8 (asmString b) <>
     stringUtf8 ":"
buildCode i@(AsmInstruction {}) =
  alignString 3 (BS.pack $ asmLatency i) <>
  stringUtf8 "  " <>
  stringUtf8 "  " <>
  stringUtf8 "[" <>
  lazyByteString (BS.intercalate (BS.pack ", ") (map BS.pack $ asmOutput i)) <>
  stringUtf8 "] <- " <>
  stringUtf8 "\"" <>
  stringUtf8 (asmString i) <>
  stringUtf8 "\"" <>
  stringUtf8 " <- [" <>
  lazyByteString (BS.intercalate (BS.pack ", ") (map BS.pack $ asmInput i)) <>
  stringUtf8 "]"

-- | Prints a string with right-justified alignment.
alignString
  :: Int
     -- ^ Desired width of string.
  -> BS.ByteString
     -- ^ String to align
  -> Builder
alignString w s =
  let pad = BS.replicate (fromIntegral w - BS.length s) ' '
  in lazyByteString pad <> lazyByteString s
