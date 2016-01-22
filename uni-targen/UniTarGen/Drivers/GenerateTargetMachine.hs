--------------------------------------------------------------------------------
-- |
-- Module      : UniTarGen.Drivers.GenerateTargetMachine
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a machine description and generates a corresponding target machine. The
-- result is intended to be written to file.
--
--------------------------------------------------------------------------------

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
import Language.InstrSel.TargetMachines.Generators.LLVM.Generator
import Language.InstrSel.TargetMachines.Generators.HaskellCodeGenerator
import Language.InstrSel.Utils.Base
  ( isLeft
  , fromLeft
  , fromRight
  )
import Language.InstrSel.Utils.JSON
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , readFileContent
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
     let tm = generateTargetMachine parsed_m
         code = generateHaskellCode tm
     return [toOutput ((fromTargetMachineID $ tmID tm) ++ ".hs") code]

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
parseSemanticsInMD :: MachineDescription -> IO MachineDescription
parseSemanticsInMD m =
  do new_is <- mapM processInstr $ mdInstructions m
     return $ m { mdInstructions = new_is }
  where processInstr i =
          do new_sem <- mapM processSem $ instrSemantics i
             return $ i { instrSemantics = new_sem }
        processSem (InstrSemantics (Left str)) =
          do ast <- parseSemantics str
             return $ InstrSemantics $ Right ast
        processSem s@(InstrSemantics (Right {})) = return s

parseSemantics :: String -> IO AST.Module
parseSemantics str =
  do res <-
       withContext
         ( \context ->
             runExceptT $ withModuleFromLLVMAssembly context str moduleAST
         )
     when (isLeft res)
        $ reportErrorAndExit $ show $ fromLeft res
     return $ fromRight res
