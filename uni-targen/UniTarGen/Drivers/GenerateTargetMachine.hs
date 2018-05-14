{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
  ( TargetMachine )
import Language.InstrSel.TargetMachines.Generators.LLVM.Base
  ( MachineDescription (..)
  , Instruction (..)
  , InstrPattern (..)
  , InstrSemantics (..)
  )
import Language.InstrSel.TargetMachines.Transformations
import Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator
import Language.InstrSel.TargetMachines.Generators.InstructionSynthesizer
import Language.InstrSel.TargetMachines.Generators.LLVM.Generator
import Language.InstrSel.Utils.Base
  ( isLeft
  , isRight
  , fromLeft
  , fromRight
  )
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO
  ( reportError
  , reportErrorAndExit
  , readFileContent
  , foldM
  )
import Language.InstrSel.Utils.String
  ( trim )

import LLVM.General
import qualified LLVM.General.AST as AST
  ( Module )
import LLVM.General.Context
import Control.Monad.Except
  ( runExceptT )

import Data.Maybe
  ( isJust
  , isNothing
  , fromJust
  , mapMaybe
  )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run :: Options -> IO [Output]
run opts =
  do str <- loadMachDescFile opts
     pmodule <- getParentModule opts
     max_instrs <- getMaxInstructionsPerSubModule opts
     pretty_print <- getPrettyPrintPred opts
     let m_str = fromJson str
     when (isLeft m_str) $
       reportErrorAndExit $ fromLeft m_str
     let m = fromRight m_str
     (err_id, parsed_m) <- parseSemanticsInMD 1 m
     (_, tm0) <- generateTM err_id parsed_m
     let tm1 = lowerPointers tm0
         tm2 = copyExtend tm1
         tm3 = combineConstants tm2
         file_code_ps = generateTargetMachineModule pmodule
                                                    max_instrs
                                                    pretty_print
                                                    tm3
     return $ map (\(file, code) -> toOutputWithID file code) file_code_ps

-- | Loads the content of the machine description file specified on the command
-- line. Reports error if no file is specified.
loadMachDescFile :: Options -> IO BS.ByteString
loadMachDescFile opts =
  do let f = machDescFile opts
     when (isNothing f) $
       reportErrorAndExit "No machine description provided."
     readFileContent $ fromJust f

-- | Returns the parent module specified on the command line. Returns error if
-- no module is specified.
getParentModule :: Options -> IO String
getParentModule opts =
  do let p = parentModule opts
     when (isNothing p) $
       reportErrorAndExit "No parent module provided."
     return $ fromJust p

-- | Returns the option whether to pretty-print the output specified on the
-- command line.
getPrettyPrintPred :: Options -> IO Bool
getPrettyPrintPred opts =
  do let p = prettyPrint opts
     return $ if isJust p then fromJust p else False

-- | Returns the maximum number of instructions per target-machine submodule
-- specified on the command line. If no such value is specified, a default value
-- is returned.
--
-- @see 'defaultMaxInstrPerSubModule'
getMaxInstructionsPerSubModule :: Options -> IO Int
getMaxInstructionsPerSubModule opts =
  do let maybe_i = maxInstructionsPerSubModule opts
     if isJust maybe_i
     then do let i = fromJust maybe_i
             when (i < 1) $
               reportErrorAndExit "Maximum number of instructions per \
                                  \target-machine submodule must be greater \
                                  \than 0."
             return i
     else return defaultMaxInstructionsPerSubModule

-- | Parses the semantic strings in the 'MachineDescription' into LLVM IR
-- modules.
parseSemanticsInMD
  :: Int
     -- ^ Next error ID.
  -> MachineDescription
  -> IO (Int, MachineDescription)
parseSemanticsInMD err_id m =
  do (next_err_id, new_is) <-
       foldM ( \(e, is) i ->
                 do (e', i') <- processInstr e i
                    return $ if isJust i'
                             then (e', is ++ [fromJust i'])
                             else (e', is)
             )
             (err_id, [])
             (mdInstructions m)
     return $ (next_err_id, m { mdInstructions = new_is })
  where processInstr e i =
          do res <- mapM processPattern $ instrPatterns i
             err_p <- foldM ( \(e', rs) r ->
                              if isRight r
                              then return (e', rs ++ [fromRight r])
                              else do reportError $
                                        "--- ERROR #" ++ show e' ++ " found " ++
                                        "in pattern semantics of " ++
                                        "instruction with emit string\n" ++
                                        "--- '" ++
                                        instrEmitString i ++ "':\n" ++
                                        trim (fromLeft r) ++ "\n" ++
                                        "Skipping to next pattern.\n"
                                      return (e' + 1, rs)
                            )
                            (e, [])
                            res
             let new_p = snd err_p
                 next_err_id = fst err_p
             return $ if length new_p > 0 || (length $ instrPatterns i) == 0
                      then (next_err_id, Just $ i { instrPatterns = new_p })
                      else (next_err_id, Nothing)
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
     else return $ Left $ fromLeft res

-- | Generates a 'TargetMachine' from a given 'MachineDescription'. In order to
-- ignore instructions that yield errors when generating the 'TargetMachine', we
-- generate the 'TargetMachine' one instruction at a time and then regenerate
-- the 'TargetMachine' using only the instructions that succeeded.
generateTM
  :: Int
     -- ^ Next error ID.
  -> MachineDescription
  -> IO (Int, TargetMachine)
generateTM err_id md =
  do let all_instrs = mdInstructions md
         uni_instr_mds = map (\i -> md { mdInstructions = [i] }) all_instrs
         uni_instr_tms = map generateTargetMachine uni_instr_mds
     let tm_pairs = zip uni_instr_tms all_instrs
         okay_instrs =
           mapMaybe (\(r, i) -> if isRight r then Just i else Nothing) tm_pairs
         bad_instrs_with_err =
           mapMaybe ( \(r, i) -> if isLeft r
                                 then Just (i, fromLeft r)
                                 else Nothing
                    ) $
           tm_pairs
     next_err_id <-
       foldM ( \e (i, err) -> do reportError $
                                    "--- ERROR #" ++ show e ++ " found in " ++
                                    "pattern semantics of " ++
                                    "instruction with emit string:\n" ++
                                    "--- " ++ (instrEmitString i) ++ "':\n" ++
                                    err ++ "\n" ++
                                    "Skipping to next instruction.\n"
                                 return $ e + 1
             )
             err_id
             bad_instrs_with_err
     let tm0 = fromRight $
               generateTargetMachine $
               md { mdInstructions = okay_instrs }
     tm1 <- do let res = addDualTargetBranchInstructions tm0
               when (isLeft res) $
                 reportError $ "--- WARNING: Failed to synthesize " ++
                               "dual-target branch instructions:\n" ++
                               fromLeft res
               return $ if isRight res then fromRight res else tm0
     tm2 <- do let res = addEntryTargetBranchInstructions tm1
               when (isLeft res) $
                 reportError $ "--- WARNING: Failed to synthesize " ++
                               "entry-target branch instructions:\n" ++
                               fromLeft res
               return $ if isRight res then fromRight res else tm1
     return $ (next_err_id, tm2)
