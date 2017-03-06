{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Generators.InstructionSynthesizer
  ( addDualTargetBranchInstructions
  , addEntryTargetBranchInstructions
  , mkDualTargetBranchInstructions
  , mkEntryTargetBranchInstructions
  )
where

import Language.InstrSel.Constraints
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.TargetMachines.Generators.GenericInstructions
  ( reassignInstrIDs )
import Language.InstrSel.TargetMachines.Generators.PatternAnalysis
  ( arePatternsUncondBranch
  , arePatternsCondBranchWithFallthrough
  )

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | From a given target machine, constructs a new target machine that contains
-- synthesized branch instructions that can jump to both the true and the false
-- branch. These are synthesized using the conditional and unconditional branch
-- instructions already in the target machine. This assumes that all
-- unconditional branch instructions are equally suitable.
addDualTargetBranchInstructions :: TargetMachine -> Either String TargetMachine
addDualTargetBranchInstructions tm =
  do let is = getAllInstructions tm
     dual_br_is <- mkDualTargetBranchInstructions is
     let new_is = reassignInstrIDs 0 (is ++ dual_br_is)
     return $ replaceAllInstructions new_is tm

-- | From a given list of instructions, constructs new branch instructions that
-- can jump to both the true and the false branch. These are synthesized using
-- the branch instructions already in the list of instructions.
mkDualTargetBranchInstructions
  :: [Instruction]
     -- ^ List of existing instructions.
  -> Either String [Instruction]
     -- ^ The synthesized instructions if successful, otherwise an error
     -- message.
mkDualTargetBranchInstructions is =
  do let br_instrs = filter (arePatternsUncondBranch . instrPatterns) is
     br_instr <- if length br_instrs > 0
                 then Right $ head br_instrs
                 else Left "No unconditional branch instruction found"
     let cbr_instrs = filter ( arePatternsCondBranchWithFallthrough .
                               instrPatterns
                             ) $
                      is
     dual_cbr_instrs <- mapM (convertFallthroughToBranch br_instr) cbr_instrs
     return dual_cbr_instrs

convertFallthroughToBranch
  :: Instruction
     -- ^ An unconditional branch instruction.
  -> Instruction
     -- ^ The conditional branch instruction with fall-through to convert.
  -> Either String Instruction
     -- ^ The new instruction if successful, otherwise an error message.
convertFallthroughToBranch br_instr i =
  do let appendBrToEmitStr ft_b_node str =
           do p <- let ps = instrPatterns br_instr
                   in if length ps > 0
                      then Right $ head ps
                      else Left $ "Unconditional branch instruction does " ++
                                  "not have any patterns"
              let error_head = "In instruction with emit string:\n" ++
                               pShow (patEmitString p) ++ ":\n"
                  os = patOS p
                  g = osGraph os
              br_b_node <- let ns = filter ( \n ->
                                             isBlockNode n &&
                                             not (hasAnySuccessors g n)
                                           ) $
                                    getAllNodes g
                           in if length ns == 1
                              then Right $ getNodeID $ head ns
                              else if length ns == 0
                                   then Left $ error_head ++
                                               "Has no block node without " ++
                                               "successors"
                                   else Left $ error_head ++ "Has multiple " ++
                                               "block node without successors"
              let br_emit_str = patEmitString p
                  new_br_emit_str = updateNodeInEmitStrTemplate ft_b_node
                                                                br_b_node
                                                                br_emit_str
              return $ str `concatEmitStrings` new_br_emit_str
         mkNewInstrPattern p =
           do let error_head = "In instruction with emit string:\n" ++
                               pShow (patEmitString p) ++ ":\n"
                  os = patOS p
                  cs = osConstraints os
              ft_c <- let c = filter isFallThroughConstraint cs
                      in if length c == 1
                         then Right $ head c
                         else if length c == 0
                              then Left $ error_head ++
                                          "No fall-through constraint found"
                              else Left $ error_head ++ "Multiple " ++
                                          "fall-through constraints found"
              let getNode ( FallThroughFromMatchToBlockConstraint
                            ( BlockOfBlockNodeExpr
                              ( ANodeIDExpr n )
                            )
                          ) = Right n
                  getNode _ = Left $ error_head ++
                                     "Unexpected fall-through constraint " ++
                                     "structure"
              ft_b_node <- getNode ft_c
              let new_cs = filter (not . isFallThroughConstraint) cs
                  new_os = os { osConstraints = new_cs }
                  emit_str = patEmitString p
              new_emit_str <- appendBrToEmitStr ft_b_node emit_str
              return $ p { patOS = new_os
                         , patEmitString = new_emit_str
                         }
     new_ps <- mapM mkNewInstrPattern $ instrPatterns i
     let br_props = instrProps br_instr
         i_props = instrProps i
         new_props = i_props { instrCodeSize = instrCodeSize br_props +
                                               instrCodeSize i_props
                             , instrLatency = instrLatency br_props +
                                              instrLatency i_props
                             }
     return $ Instruction { instrID = 0
                          , instrPatterns = new_ps
                          , instrProps = new_props
                          }

-- | From a given target machine, constructs a new target machine that contains
-- synthesized branch instructions that can jump back to the current block.
-- These are synthesized using the conditional branch instructions already in
-- the target machine.
addEntryTargetBranchInstructions :: TargetMachine -> Either String TargetMachine
addEntryTargetBranchInstructions tm =
  do let is = getAllInstructions tm
     entry_br_is <- mkEntryTargetBranchInstructions is
     let new_is = reassignInstrIDs 0 (is ++ entry_br_is)
     return $ replaceAllInstructions new_is tm

-- | From a given list of instructions, constructs new conditional branch
-- instructions that can jump back to the current block. These are synthesized
-- using the conditional branch instructions already in the list of
-- instructions.
mkEntryTargetBranchInstructions
  :: [Instruction]
     -- ^ List of existing instructions.
  -> Either String [Instruction]
     -- ^ The synthesized instructions if successful, otherwise an error
     -- message.
mkEntryTargetBranchInstructions is =
  do let cbr_instrs = filter ( arePatternsCondBranchWithFallthrough .
                               instrPatterns
                             ) $
                      is
     entry_cbr_instrs <- mapM redirectTargetBlockToEntry cbr_instrs
     return entry_cbr_instrs

redirectTargetBlockToEntry
  :: Instruction
     -- ^ The conditional branch instruction with fall-through to convert.
  -> Either String Instruction
     -- ^ The new instruction if successful, otherwise an error message.
redirectTargetBlockToEntry i =
  do let mkNewInstrPatterns p =
           do let error_head = "In instruction with emit string:\n" ++
                               pShow (patEmitString p) ++ ":\n"
                  os = patOS p
                  g = osGraph os
                  cs = osConstraints os
                  ft_cs = filter isFallThroughConstraint cs
              let getNode ( FallThroughFromMatchToBlockConstraint
                            ( BlockOfBlockNodeExpr
                              ( ANodeIDExpr n )
                            )
                          ) = Right n
                  getNode _ = Left $ error_head ++
                                     "Unexpected fall-through constraint " ++
                                     "structure"
              ft_b_nodes <- mapM getNode ft_cs
              entry_b <- if isJust (entryBlockNode g)
                         then Right $ fromJust $ entryBlockNode g
                         else Left $ error_head ++ "Has no entry block node"
              let b_ns = [ n | n <- filter isBlockNode $ getAllNodes g
                             , getNodeID n `notElem` ft_b_nodes
                             , length (getCtrlFlowOutEdges g n) == 0
                         ]
                  new_ps = map ( \n ->
                                 let new_g = mergeNodes entry_b n g
                                     new_os = os { osGraph = new_g }
                                     new_emit_str = updateNodeInEmitStrTemplate
                                                      (getNodeID entry_b)
                                                      (getNodeID n)
                                                      (patEmitString p)
                                 in p { patOS = new_os
                                      , patEmitString = new_emit_str
                                      }
                               ) $
                           b_ns
              return new_ps
     new_pss <- mapM mkNewInstrPatterns $ instrPatterns i
     let new_ps = concat new_pss
     return $ Instruction { instrID = 0
                          , instrPatterns = new_ps
                          , instrProps = instrProps i
                          }
