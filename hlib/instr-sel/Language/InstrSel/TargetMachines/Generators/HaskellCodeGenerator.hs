{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator where

import Language.InstrSel.TargetMachines.Base
  ( Instruction
  , TargetMachine (..)
  , fromTargetMachineID
  , toSafeTargetMachineID
  )
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.ByteStringBuilder
import Language.InstrSel.Utils
  ( chunksOf )

import Data.List
  ( intercalate )
import qualified Data.Map as M

import Language.Haskell.Exts



-------------
-- Functions
-------------

-- | Takes a 'TargetMachine' and generates corresponding Haskell source code.
-- The source code is then wrapped inside a module with name equal to its
-- 'Language.InstrSel.TargetMachines.IDs.TargetMachineID'. The source code is
-- also paired with a 'FilePath' with the name of the generated module. If the
-- target machine contains many instructions, then it will be split into
-- multiple submodules.
generateTargetMachineModule
  :: String
     -- ^ Parent module to wherein the generated module(s) will reside.
  -> Int
     -- ^ Maximum number of instructions per submodule. Must be greater than 0.
  -> Bool
     -- ^ Whether to pretty-print the code of the module.
  -> TargetMachine
  -> [(FilePath, BS.ByteString)]
generateTargetMachineModule mparent max_instrs pretty_print tm =
  let module_name = fromTargetMachineID $
                    toSafeTargetMachineID $
                    fromTargetMachineID (tmID tm)
      instr_groups = if max_instrs > 1
                     then chunksOf max_instrs $
                          map snd $
                          M.toList $
                          tmInstructions tm
                     else error $ "max_instrs must be greater than 0 (was "
                                  ++ show max_instrs ++ ")"
      sub_module_names = zipWith (\_ i -> module_name ++ "sub" ++ (show i))
                                 instr_groups
                                 ([0..] :: [Int])
      sub_modules = zipWith ( \is mname -> generateSubModule mparent
                                                             mname
                                                             pretty_print
                                                             is
                            )
                            instr_groups
                            sub_module_names
      top_module = generateTopModule mparent
                                     module_name
                                     sub_module_names
                                     pretty_print
                                     tm
  in (module_name ++ ".hs", top_module) :
     (zip (map (++ ".hs") sub_module_names) sub_modules)

-- | Generates Haskell code for top-level module.
generateTopModule
  :: String
     -- ^ Parent module to wherein the generated module(s) will reside.
  -> String
     -- ^ Name of the top module.
  -> [String]
     -- ^ Name of all submodules to be used by the top-level module.
  -> Bool
     -- ^ Whether to pretty-print the code of the module.
  -> TargetMachine
  -> BS.ByteString
generateTopModule mparent module_name sub_module_names pretty_print tm =
  let module_path = mparent ++ "." ++ module_name
      haskell_code =
        stringUtf8 "module " <> stringUtf8 module_path <>
        stringUtf8 "\n\
                   \  ( theTM )\n\
                   \where\n\
                   \\n\
                   \import Language.InstrSel.TargetMachines\n\
                   \import Language.InstrSel.Utils\n\
                   \import Data.Map\n\
                   \  ( fromList )\n\
                   \\n" <>
        stringUtf8 ( concat $
                     map ( \m -> "import qualified " ++ mparent ++ "." ++ m ++
                                 " as " ++ m ++ "\n"
                         ) $
                     sub_module_names
                   ) <>
        stringUtf8 "\n\
                   \theTM :: TargetMachine\n\
                   \theTM =\n\
                   \  TargetMachine\n\
                   \  { tmID = " <> stringUtf8 (show $ tmID tm) <>
        stringUtf8 "\n\
                   \  , tmInstructions =\n\
                   \      fromList $\n\
                   \      map (\\i -> (instrID i, i)) $\
                   \      concat [" <>
        stringUtf8 ( intercalate ", " $
                     map (++ ".theInstructions") $
                     sub_module_names
                   ) <> stringUtf8 "]" <>
        stringUtf8 "\n\
                   \  , tmLocations = " <>
        stringUtf8 (show $ tmLocations tm) <>
        stringUtf8 "\n\
                   \  , tmPointerSize = " <>
        stringUtf8 (show $ tmPointerSize tm) <>
        stringUtf8 "\n\
                   \  , tmNullPointerValue = " <>
        stringUtf8 (show $ tmNullPointerValue tm) <>
        stringUtf8 "\n\
                   \  , tmPointerSymbolRange = " <>
        stringUtf8 (show $ tmPointerSymbolRange tm) <>
        stringUtf8 "\n\
                   \  }"
  in toLazyByteString $
     lazyByteString (generateBoilerPlateCode module_path) <>
     ( if pretty_print
       then lazyByteString $
            prettyPrintModuleCode $
            toLazyByteString haskell_code
       else haskell_code
     )

-- | Generates Haskell code for a submodule containing the given list of
-- instructions.
generateSubModule
  :: String
     -- ^ Parent module to wherein the generated submodule will reside.
  -> String
     -- ^ Name of the submodule.
  -> Bool
     -- ^ Whether to pretty-print the code of the module.
  -> [Instruction]
  -> BS.ByteString
generateSubModule mparent module_name pretty_print instructions =
  let renameFuncs str = BS.replace (BS.pack "mkGraph") (BS.pack "I.mkGraph") str
      module_path = mparent ++ "." ++ module_name
      haskell_code =
        stringUtf8 "{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n\
                   \\n\
                   \module " <> stringUtf8 module_path <>
        stringUtf8 "\n\
                   \  ( theInstructions )\n\
                   \where\n\n\
                   \import Language.InstrSel.Constraints\n\
                   \import Language.InstrSel.DataTypes\n\
                   \import Language.InstrSel.Graphs\n\
                   \import Language.InstrSel.Functions.IDs\n\
                   \import qualified Data.Graph.Inductive as I\n\
                   \import Language.InstrSel.OpStructures\n\
                   \import Language.InstrSel.OpTypes\n\
                   \import Language.InstrSel.TargetMachines\n\
                   \import Language.InstrSel.Utils\n\
                   \import Prelude\n\
                   \  hiding\n\
                   \  ( LT, GT )\n\
                   \import Data.Map\n\
                   \  ( fromList )\n\n\
                   \\n\
                   \theInstructions :: [Instruction]\n\
                   \theInstructions = " <>
        lazyByteString ( renameFuncs $
                         BS.pack $
                         show $
                         instructions
                       )
  in toLazyByteString $
     lazyByteString (generateBoilerPlateCode module_path) <>
     ( if pretty_print
       then lazyByteString $
            prettyPrintModuleCode $
            toLazyByteString haskell_code
       else haskell_code
     )

-- | Generates boiler platep code for a given module.
generateBoilerPlateCode
  :: String
     -- ^ The module path.
  -> BS.ByteString
generateBoilerPlateCode mpath =
  toLazyByteString $
  stringUtf8 (replicate 80 '-') <>
  stringUtf8 "\n\
             \-- |\n\
             \-- Module      : " <>
  stringUtf8 mpath <>
  stringUtf8 "\n\
             \-- Stability   : experimental\n\
             \-- Portability : portable\n\
             \--\n\
             \-- THIS MODULE HAS BEEN AUTOGENERATED!\n\
             \--\n" <>
  stringUtf8 (replicate 80 '-') <>
  stringUtf8 "\n\n"

-- | Pretty-prints given Haskell code for a module.
prettyPrintModuleCode :: BS.ByteString -> BS.ByteString
prettyPrintModuleCode code =
  let parse_res = parseFileContents $ BS.unpack code
      prettify m = prettyPrintStyleMode (style { lineLength = 80 })
                                        defaultMode m
  in case parse_res
     of (ParseOk m) -> BS.pack $ prettify m
        (ParseFailed line msg) -> error $ "prettyPrintModule: " ++
                                          "parsing failed at " ++
                                          show line ++ ": " ++ msg
