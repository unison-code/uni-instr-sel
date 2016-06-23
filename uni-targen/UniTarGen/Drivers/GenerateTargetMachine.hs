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

import Data.Maybe
  ( isNothing
  , fromJust
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
     let tm = copyExtend $ generateTargetMachine parsed_m
         code = generateModule tm
     return [toOutputWithID ((fromTargetMachineID $ tmID tm) ++ ".hs") code]

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
          do res <- mapM processSem $ instrSemantics i
             new_sem <- mapMaybeM
                          ( \r -> if isRight r
                                  then return $ Just $ fromRight r
                                  else do reportError $
                                            "--- ERROR found in semantics "
                                            ++ "of instruction with emit string"
                                            ++ " '" ++ (instrEmitString i)
                                            ++ "':\n" ++ (fromLeft r) ++ "\n"
                                            ++ "Skipping to next semantics.\n"
                                          return Nothing
                          ) res
             return $ if length new_sem > 0 || (length $ instrSemantics i) == 0
                      then Just $ i { instrSemantics = new_sem }
                      else Nothing
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
