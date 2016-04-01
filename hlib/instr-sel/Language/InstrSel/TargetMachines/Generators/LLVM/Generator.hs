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

import Language.InstrSel.Constraints.ConstraintBuilder
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures.Base
import Language.InstrSel.OpStructures.LLVM.OSMaker
import qualified Language.InstrSel.TargetMachines.Base as TM
import Language.InstrSel.TargetMachines.Generators.GenericInstructions
import qualified Language.InstrSel.TargetMachines.Generators.LLVM.Base as LLVM
import Language.InstrSel.TargetMachines.IDs
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Utils
  ( capitalize
  , splitOn
  , splitStartingOn
  , isNumeric
  )

import qualified LLVM.General.AST as LLVM
  ( Module (..)
  , Definition (..)
  )
import qualified LLVM.General.AST.Global as LLVM
  ( Global (..) )
import qualified LLVM.General.AST.Name as LLVM

import Data.Maybe
  ( isJust
  , fromJust
  , mapMaybe
  )

import Data.List
  ( intersperse )



-------------
-- Functions
-------------

generateTargetMachine :: LLVM.MachineDescription -> TM.TargetMachine
generateTargetMachine m =
  let mkPhiInstrEmitTemplate arg_ids ret_id =
        ( TM.EmitStringTemplate
          $ [ [ TM.ESLocationOfValueNode ret_id
              , TM.ESVerbatim " = PHI "
              ]
              ++ ( concat
                   $ intersperse
                       [TM.ESVerbatim " "]
                       ( map ( \n ->
                               [ TM.ESVerbatim "("
                               , TM.ESLocationOfValueNode n
                               , TM.ESVerbatim ", "
                               , TM.ESBlockOfValueNode n
                               , TM.ESVerbatim ")"
                               ]
                             )
                             arg_ids
                       )
                 )
            ]
        )
      locs = mkLocations m
      instrs = mkInstructions m locs
      generic_instrs = mkPhiInstructions mkPhiInstrEmitTemplate
                       ++ [mkBrFallThroughInstruction]
                       ++ [mkDataDefInstruction]
                       ++ [mkTempNullCopyInstruction [1, 8, 16, 32]]
      all_instrs = instrs
                   ++
                   reassignInstrIDs (toInstructionID $ length instrs)
                                    generic_instrs
  in TM.TargetMachine
       { TM.tmID = toTargetMachineID $ capitalize $ LLVM.mdID m
       , TM.tmInstructions = all_instrs
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
          let instr_id = TM.toInstructionID i_id
              patterns = mkInstrPatterns locs i
              is_copy_instr = if length patterns == 1
                              then isCopyInstrPattern $ head patterns
                              else False
              props = mkInstrProps i is_copy_instr
          in TM.Instruction { TM.instrID = instr_id
                            , TM.instrPatterns = patterns
                            , TM.instrProps = props
                            }

isCopyInstrPattern :: TM.InstrPattern -> Bool
isCopyInstrPattern p =
  let g = osGraph $ TM.patOS p
      op_nodes = filter isNodeAnOperation $ getAllNodes g
  in length op_nodes == 1 && isCopyNode (head op_nodes)

mkInstrPatterns :: [TM.Location] -> LLVM.Instruction -> [TM.InstrPattern]
mkInstrPatterns locs i =
  map processSemantics $ zip ([0..] :: [Integer]) (LLVM.instrSemantics i)
  where processSemantics (p_num, p) =
          let p_id = TM.toPatternID p_num
              os = addOperandConstraints i locs $ mkOpStructure p
              tmpl = mkEmitString i os (LLVM.instrEmitString i)
          in TM.InstrPattern { TM.patID = p_id
                             , TM.patOS = os
                             , TM.patADDUC = True
                             , TM.patEmitString = tmpl
                             }

addOperandConstraints
  :: LLVM.Instruction
  -> [TM.Location]
  -> OpStructure
  -> OpStructure
addOperandConstraints i all_locs os =
  foldr f os (LLVM.instrOperands i)
  where f (LLVM.RegInstrOperand op_name reg_names) os' =
          let locs = map getIDOfLocWithName reg_names
              n = getValueNode os' op_name
          in addNewDataLocConstraints locs (getNodeID n) os'
        f (LLVM.ImmInstrOperand op_name range) os' =
          let n = getValueNode os' op_name
              old_dt = getDataTypeOfValueNode n
              -- It is assumed that the value node for an immediate is always a
              -- temporary at this point
              new_dt = D.IntConstType range (Just $ D.intTempNumBits old_dt)
              new_g = updateDataTypeOfValueNode new_dt n (osGraph os')
          in os' { osGraph = new_g }
        f (LLVM.AbsAddrInstrOperand op_name range) os' =
          addAddressConstraints os' (getValueOrBlockNode os' op_name) range
        f (LLVM.RelAddrInstrOperand op_name range) os' =
          addAddressConstraints os' (getValueOrBlockNode os' op_name) range
        getValueNode os' origin =
          let n = findValueNodesWithOrigin (osGraph os') origin
          in if length n == 1
             then head n
             else if length n > 0
                  then error $ "addOperandConstraints: multiple value nodes "
                               ++ "with origin '" ++ origin ++ "'"
                  else error $ "addOperandConstraints: no value node with "
                               ++ "origin '" ++ origin ++ "' in instruction '"
                               ++ LLVM.instrEmitString i ++ "'"
        getValueOrBlockNode os' str =
          let value_n = findValueNodesWithOrigin (osGraph os') str
              block_n = findBlockNodesWithName (osGraph os') $ toBlockName str
          in if length value_n > 0
             then if length value_n == 1
                  then head value_n
                  else error $ "addOperandConstraints: multiple value nodes "
                               ++ "with origin '" ++ str ++ "'"
             else if length block_n > 0
                  then if length block_n == 1
                       then head block_n
                       else error $ "addOperandConstraints: multiple block "
                                    ++ "nodes with name '" ++ str ++ "'"
                  else error $ "addOperandConstraints: no value or block node "
                               ++ "with origin or name '" ++ str ++ "'"
        getIDOfLocWithName name =
          let loc = filter (\l -> TM.locName l == TM.toLocationName name)
                           all_locs
          in if length loc == 1
             then TM.locID $ head loc
             else if length loc > 0
                  then error $ "addOperandConstraints: multiple locations with "
                               ++ "name '" ++ name ++ "'"
                  else error $ "addOperandConstraints: no location with name '"
                               ++ name ++ "'"
        addAddressConstraints os' n range =
          if isValueNode n
          then let old_dt = getDataTypeOfValueNode n
                   -- It is assumed that the value node for an immediate is
                   -- always a temporary at this point
                   new_dt = D.IntConstType range
                                           (Just $ D.intTempNumBits old_dt)
                   new_g = updateDataTypeOfValueNode new_dt n (osGraph os')
               in os' { osGraph = new_g }
          else -- TODO: implement
               os'

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

mkEmitString
  :: LLVM.Instruction
  -> OpStructure
  -> String
  -> TM.EmitStringTemplate
mkEmitString i os str =
  TM.EmitStringTemplate
  $ map (mergeVerbatims . map f . splitStartingOn "% ,()[]")
  $ splitOn "\n" str
  where
  f s = if head s == '%'
        then if not (isNumeric (tail s))
             then let g = osGraph os
                      value_n = findValueNodesWithOrigin g s
                      block_n = findBlockNodesWithName g $ toBlockName s
                  in if length value_n > 0
                     then if length value_n == 1
                          then let op = getInstrOperand i s
                               in if isJust op
                                  then case (fromJust op)
                                       of (LLVM.RegInstrOperand {}) ->
                                            TM.ESLocationOfValueNode
                                            $ getNodeID
                                            $ head value_n
                                          (LLVM.ImmInstrOperand {}) ->
                                            TM.ESIntConstOfValueNode
                                            $ getNodeID
                                            $ head value_n
                                          _ -> error $ "mkEmitString: unknown "
                                                       ++ "operand type"
                                  else error $ "mkEmitString: no operand with "
                                               ++ "name '" ++ s ++ "'"
                          else error $ "mkEmitString: multiple value nodes with"
                                       ++ " origin '" ++ s ++ "'"
                     else if length block_n > 0
                          then if length block_n == 1
                               then let op = getInstrOperand i s
                                    in if isJust op
                                       then case (fromJust op)
                                            of (LLVM.AbsAddrInstrOperand {}) ->
                                                 TM.ESNameOfBlockNode
                                                 $ getNodeID
                                                 $ head block_n
                                               (LLVM.RelAddrInstrOperand {}) ->
                                                 TM.ESNameOfBlockNode
                                                 $ getNodeID
                                                 $ head block_n
                                               _ -> error
                                                    $ "mkEmitString: unknown "
                                                    ++ "operand type"
                                       else error $ "mkEmitString: no operand "
                                                    ++ "with name '" ++ s ++ "'"
                               else error $ "mkEmitString: multiple block nodes"
                                            ++ " with name '" ++ s ++ "'"
                          else error $ "mkEmitString: no value or blocks nodes "
                                       ++ "with origin or name '" ++ s ++ "'"
             else let int = read $ tail s
                  in TM.ESTemporary int
        else TM.ESVerbatim s
  mergeVerbatims [] = []
  mergeVerbatims [s] = [s]
  mergeVerbatims (TM.ESVerbatim s1:TM.ESVerbatim s2:ss) =
    mergeVerbatims (TM.ESVerbatim (s1 ++ s2):ss)
  mergeVerbatims (s:ss) = (s:mergeVerbatims ss)

mkInstrProps :: LLVM.Instruction -> Bool -> TM.InstrProperties
mkInstrProps i is_copy =
  TM.InstrProperties { TM.instrCodeSize = LLVM.instrSize i
                     , TM.instrLatency = LLVM.instrLatency i
                     , TM.instrIsNonCopy = not is_copy
                     , TM.instrIsNullCopy = False
                     }

-- | Gets the operand with a given name of a given instruction. If no such
-- operand is found, 'Nothing' is returned.
getInstrOperand :: LLVM.Instruction -> String -> Maybe LLVM.InstrOperand
getInstrOperand i name =
  let op = filter (\o -> LLVM.opName o == name) $ LLVM.instrOperands i
  in if length op > 0
     then Just $ head op
     else Nothing
