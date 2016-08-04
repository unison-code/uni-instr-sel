{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniTarGen.Drivers.GenerateTargetMachine
  ( run )
where

import UniTarGen.Drivers.Base
import Language.InstrSel.TargetMachines
  ( TargetMachine (tmID) )
import Language.InstrSel.TargetMachines.IDs
  ( fromTargetMachineID )
import Language.InstrSel.TargetMachines.Generators.LLVM.Base
  ( MachineDescription (..)
  , Instruction (..)
  , InstrPattern (..)
  , InstrSemantics (..)
  )
import Language.InstrSel.TargetMachines.Transformations
import Language.InstrSel.TargetMachines.Generators.LLVM.Generator
import Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator
import Language.InstrSel.Utils.Base
  ( isLeft
  , isRight
  , fromLeft
  , fromRight
  )
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO
  ( reportError
  , reportErrorAndExit
  , readFileContent
  , mapMaybeM
  )

import LLVM.General
import qualified LLVM.General.AST as AST
  ( Module )
import LLVM.General.Context
import Control.Monad.Except
  ( runExceptT )
import Control.Exception
  ( SomeException
  , try
  , evaluate
  )

import Data.Maybe
  ( isNothing
  , fromJust
  , mapMaybe
  )



-------------
-- Functions
-------------

run :: Options -> IO [Output]
run opts =
  do str <- loadMachDescFile opts
     let m_str = fromJson str
     when (isLeft m_str) $
       reportErrorAndExit $ fromLeft m_str
     let m = fromRight m_str
     parsed_m <- parseSemanticsInMD m
     tm <- generateTM parsed_m
     let ce_tm = copyExtend tm
         code = generateModule ce_tm
     return [toOutputWithID ((fromTargetMachineID $ tmID ce_tm) ++ ".hs") code]

-- | Loads the content of the machine description file specified on the command
-- line. Reports error if no file is specified.
loadMachDescFile :: Options -> IO String
loadMachDescFile opts =
  do let f = machDescFile opts
     when (isNothing f) $
       reportErrorAndExit "No machine description provided."
     readFileContent $ fromJust f

-- | Parses the semantic strings in the 'MachineDescription' into LLVM IR
-- modules.
parseSemanticsInMD
  :: MachineDescription
  -> IO MachineDescription
parseSemanticsInMD m =
  do new_is <- mapMaybeM processInstr $ mdInstructions m
     return $ m { mdInstructions = new_is }
  where processInstr i =
          do res <- mapM processPattern $ instrPatterns i
             new_p <-
               mapMaybeM
                 ( \r -> if isRight r
                         then return $ Just $ fromRight r
                         else do reportError $
                                   "--- ERROR found in pattern semantics of "
                                   ++ "instruction with emit string\n"
                                   ++ "--- '"
                                   ++ (instrEmitString i) ++ "':\n"
                                   ++ (fromLeft r) ++ "\n"
                                   ++ "Skipping to next pattern.\n"
                                 return Nothing
                 )
                 res
             return $ if length new_p > 0 || (length $ instrPatterns i) == 0
                      then Just $ i { instrPatterns = new_p }
                      else Nothing
        processPattern p =
          do res <- processSem $ instrSemantics p
             if isRight res
             then return $ Right $ p { instrSemantics = fromRight res }
             else return $ Left $ fromLeft res
        processSem (InstrSemantics (Left str)) =
          do res <- parseSemantics str
             if isRight res
             then return $ Right $ InstrSemantics $ Right $ fromRight res
             else return $ Left $ fromLeft res
        processSem s@(InstrSemantics (Right {})) = return $ Right s

parseSemantics :: String -> IO (Either String AST.Module)
parseSemantics str =
  do res <-
       withContext
         ( \context ->
             runExceptT $ withModuleFromLLVMAssembly context str moduleAST
         )
     if isRight res
     then return $ Right $ fromRight res
     else let err = fromLeft res
          in if isLeft err
             then return $ Left $ fromLeft err
             else return $ Left $ show $ fromRight err

-- | Generates a 'TargetMachine' from a given 'MachineDescription'. In order to
-- ignore instructions that yield errors when generating the 'TargetMachine', we
-- generate the 'TargetMachine' one instruction at a time and then regenerate
-- the 'TargetMachine' using only the instructions that succeeded.
generateTM :: MachineDescription -> IO TargetMachine
generateTM md =
  do let all_instrs = mdInstructions md
         uni_instr_mds = map (\i -> md { mdInstructions = [i] }) all_instrs
     uni_instr_tms <-
       mapM ( \m -> try (evaluate $ generateTargetMachine m)
                      :: IO (Either SomeException TargetMachine)
            )
            uni_instr_mds
     let tm_pairs = zip uni_instr_tms all_instrs
         okay_instrs =
           mapMaybe (\(r, i) -> if isRight r then Just i else Nothing) tm_pairs
         bad_instrs_with_err =
           mapMaybe ( \(r, i) -> if isLeft r
                                 then Just (i, fromLeft r)
                                 else Nothing
                    )
                    tm_pairs
     mapM_ ( \(i, err) -> reportError $
                            "--- ERROR found in pattern semantics of "
                            ++ "instruction with emit string:\n"
                            ++ "--- "
                            ++ (instrEmitString i) ++ "':\n"
                            ++ (show err) ++ "\n"
                            ++ "Skipping to next instruction.\n"
           )
           bad_instrs_with_err
     return $ generateTargetMachine $ md { mdInstructions = okay_instrs }
