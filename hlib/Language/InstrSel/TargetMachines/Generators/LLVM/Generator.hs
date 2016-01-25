--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Generators.LLVM.Generator
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a LLVM-specific machine description and generates the corresponding
-- target machine.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Generators.LLVM.Generator where

import Language.InstrSel.OpStructures.Base
import Language.InstrSel.OpStructures.LLVM.OSMaker
import qualified Language.InstrSel.TargetMachines.Generators.LLVM.Base as LLVM
import qualified Language.InstrSel.TargetMachines.Base as TM
import Language.InstrSel.TargetMachines.IDs
import Language.InstrSel.Utils
  ( capitalize )

import qualified LLVM.General.AST as LLVM
  ( Module (..)
  , Definition (..)
  )
import qualified LLVM.General.AST.Global as LLVM
  ( Global (..) )
import qualified LLVM.General.AST.Name as LLVM

import Data.Maybe
  ( mapMaybe )



-------------
-- Functions
-------------

generateTargetMachine :: LLVM.MachineDescription -> TM.TargetMachine
generateTargetMachine m =
  let locs = mkLocations m
      instrs = mkInstructions m locs
  in TM.TargetMachine
       { TM.tmID = toTargetMachineID $ capitalize $ LLVM.mdID m
       , TM.tmInstructions = instrs
       , TM.tmLocations = locs
       }

mkLocations :: LLVM.MachineDescription -> [TM.Location]
mkLocations m =
  map processLoc $ zip ([0..] :: [Integer]) (LLVM.mdLocations m)
  where processLoc (loc_id, LLVM.RegLocation name value) =
          TM.Location { TM.locID = toLocationID loc_id
                      , TM.locName = TM.toLocationName name
                      , TM.locValue = value
                      }

mkInstructions :: LLVM.MachineDescription -> [TM.Location] -> [TM.Instruction]
mkInstructions m locs =
  map processInstr $ zip ([0..] :: [Integer]) (LLVM.mdInstructions m)
  where processInstr (i_id, i) =
          TM.Instruction { TM.instrID = TM.toInstructionID i_id
                         , TM.instrPatterns = mkInstrPatterns i
                         , TM.instrProps = mkInstrProps i
                         }

mkInstrPatterns :: LLVM.Instruction -> [TM.InstrPattern]
mkInstrPatterns i =
  map processSemantics $ zip ([0..] :: [Integer]) (LLVM.instrSemantics i)
  where processSemantics (p_num, p) =
          let p_id = TM.toPatternID p_num
              os = mkOpStructure p
              -- TODO: add missing constraints
              tmpl = mkAsmStrTemplate os (LLVM.instrAssemblyString i)
          in TM.InstrPattern { TM.patID = p_id
                             , TM.patOS = os
                             , TM.patADDUC = True
                             , TM.patAsmStrTemplate = tmpl
                             }

mkOpStructure :: LLVM.InstrSemantics -> OpStructure
mkOpStructure (LLVM.InstrSemantics (Right m)) =
  let m_defs = LLVM.moduleDefinitions m
      getFunction d =
        case d of (LLVM.GlobalDefinition f@(LLVM.Function {})) -> Just f
                  _ -> Nothing
      isSemanticsFunction f =
        case (LLVM.name f) of (LLVM.Name name) -> name == "semantics"
                              _ -> False
      fs = mapMaybe getFunction m_defs
      sem_f = filter isSemanticsFunction fs
  in if length sem_f == 1
     then mkPatternOS $ head sem_f
     else if length sem_f == 0
          then error "mkOpStructure: no semantics function found"
          else error "mkOpStructure: multiple semantics function found"
mkOpStructure (LLVM.InstrSemantics (Left _)) =
  error "mkOpStructure: instruction semantics has not been parsed"

mkAsmStrTemplate :: OpStructure -> String -> TM.AssemblyStringTemplate
mkAsmStrTemplate os str =
  -- TODO: implement
  TM.ASSTemplate []

mkInstrProps :: LLVM.Instruction -> TM.InstrProperties
mkInstrProps i =
  TM.InstrProperties { TM.instrCodeSize = LLVM.instrSize i
                     , TM.instrLatency = LLVM.instrLatency i
                     , TM.instrIsNonCopy = -- TODO: fix
                                           True
                     , TM.instrIsNullCopy = False
                     }
