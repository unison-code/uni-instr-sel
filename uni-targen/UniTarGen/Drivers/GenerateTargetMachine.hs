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
import Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator
import Language.InstrSel.TargetMachines.Generators.InstructionSynthesizer
import Language.InstrSel.TargetMachines.Generators.LLVM.Generator
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

run :: Options -> IO [Output]
run opts =
  do str <- loadMachDescFile opts
     let m_str = fromJson str
     when (isLeft m_str) $
       reportErrorAndExit $ fromLeft m_str
     let m = fromRight m_str
     (err_id, parsed_m) <- parseSemanticsInMD 1 m
     (_, tm0) <- generateTM err_id parsed_m
     let file = fromTargetMachineID (tmID tm0) ++ ".hs"
         tm1 = lowerPointers tm0
         tm2 = copyExtend tm1
         tm3 = combineConstants tm2
         code = generateModule tm3
     return [toOutputWithID file code]

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
     tm1 <- do let res = addEntryTargetBranchInstructions tm0
               when (isLeft res) $
                 reportError $ "--- WARNING: Failed to synthesize " ++
                               "entry-target branch instructions:\n" ++
                               fromLeft res
               return $ if isRight res then fromRight res else tm0
     tm2 <- do let res = addDualTargetBranchInstructions tm1
               when (isLeft res) $
                 reportError $ "--- WARNING: Failed to synthesize " ++
                               "dual-target branch instructions:\n" ++
                               fromLeft res
               return $ if isRight res then fromRight res else tm1
     return $ (next_err_id, tm2)
