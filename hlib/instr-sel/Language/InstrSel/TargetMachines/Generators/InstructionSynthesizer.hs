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
  ( isInstructionCondBranch
  , isInstructionCondBranchWithFallthrough
  , isInstructionUncondBranch
  )

import Data.Maybe
  ( catMaybes
  , fromJust
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
  do let br_instrs = filter isInstructionUncondBranch is
     br_instr <- if length br_instrs > 0
                 then Right $ head br_instrs
                 else Left "No unconditional branch instruction found"
     let cbr_instrs = filter isInstructionCondBranchWithFallthrough is
     dual_cbr_instrs <- mapM (convertFallthroughToBranch br_instr) cbr_instrs
     return dual_cbr_instrs

convertFallthroughToBranch
  :: Instruction
     -- ^ An unconditional branch instruction.
  -> Instruction
     -- ^ The conditional branch instruction with fall-through to convert.
  -> Either String Instruction
     -- ^ The new instruction if successful, otherwise an error message.
convertFallthroughToBranch br_instr cond_br_instr =
  do let mkEmitString ft_b_node =
           do let br_emit_str = instrEmitString br_instr
                  os = instrOS br_instr
                  g = osGraph os
              br_b_node <- let error_head = "In instruction with emit " ++
                                            "string:\n" ++ pShow br_emit_str ++
                                            ":\n"
                               ns = filter ( \n ->
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
              let new_br_emit_str = updateNodeInEmitStrTemplate ft_b_node
                                                                br_b_node
                                                                br_emit_str
              return $ (instrEmitString cond_br_instr)
                       `concatEmitStrings`
                       new_br_emit_str
     let error_head = "In instruction with emit string:\n" ++
                      pShow (instrEmitString cond_br_instr) ++ ":\n"
         os = instrOS cond_br_instr
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
                            "Unexpected fall-through constraint structure"
     ft_b_node <- getNode ft_c
     let new_cs = filter (not . isFallThroughConstraint) cs
         new_os = os { osConstraints = new_cs }
     new_emit_str <- mkEmitString ft_b_node
     let mkInstrProps =
           let br_props = instrProps br_instr
               cond_br_props = instrProps cond_br_instr
           in cond_br_props { instrCodeSize = instrCodeSize cond_br_props +
                                              instrCodeSize br_props
                            , instrLatency = instrLatency cond_br_props +
                                             instrLatency br_props
                            }
     return $ cond_br_instr { instrID = 0
                            , instrOS = new_os
                            , instrEmitString = new_emit_str
                            , instrProps = mkInstrProps
                            }

-- | From a given target machine, constructs a new target machine that contains
-- synthesized branch instructions that can jump back to the current block.
-- These are synthesized using the branch instructions already in the target
-- machine.
addEntryTargetBranchInstructions :: TargetMachine -> Either String TargetMachine
addEntryTargetBranchInstructions tm =
  do let is = getAllInstructions tm
     entry_br_is <- mkEntryTargetBranchInstructions is
     let new_is = reassignInstrIDs 0 (is ++ entry_br_is)
     return $ replaceAllInstructions new_is tm

-- | From a given list of instructions, constructs new branch instructions that
-- can jump back to the current block. These are synthesized using the branch
-- instructions already in the list of instructions.
mkEntryTargetBranchInstructions
  :: [Instruction]
     -- ^ List of existing instructions.
  -> Either String [Instruction]
     -- ^ The synthesized instructions if successful, otherwise an error
     -- message.
mkEntryTargetBranchInstructions is =
  do let br_instrs = filter ( \i -> isInstructionUncondBranch i ||
                                    isInstructionCondBranch i
                            ) $
                     is
     new_br_instrs <- mapM redirectTargetBlockToEntry br_instrs
     return $ concat new_br_instrs

redirectTargetBlockToEntry
  :: Instruction
     -- ^ The branch instruction from which to synthesize new instructions.
  -> Either String [Instruction]
     -- ^ The set of new instructions if successful, otherwise an error message.
redirectTargetBlockToEntry i =
  do let error_head = "In instruction with emit string:\n" ++
                      pShow (instrEmitString i) ++ ":\n"
         os = instrOS i
         g = osGraph os
         cs = osConstraints os
         ft_cs = filter isFallThroughConstraint cs
     let getNode ( FallThroughFromMatchToBlockConstraint
                   ( BlockOfBlockNodeExpr
                     ( ANodeIDExpr n )
                   )
                 ) = Right $ Just n
         getNode (FallThroughFromMatchToBlockConstraint _) =
           Left $ error_head ++ "Unexpected fall-through constraint structure"
         getNode _ = Right $ Nothing
     ft_b_nodes_in_cs <- mapM getNode ft_cs
     let ft_b_nodes = catMaybes ft_b_nodes_in_cs
     entry_b <- if isJust (entryBlockNode g)
                then Right $ fromJust $ entryBlockNode g
                else Left $ error_head ++ "Has no entry block node"
     let b_ns = [ n | n <- filter isBlockNode $ getAllNodes g
                    , getNodeID n `notElem` ft_b_nodes
                    , length (getCtrlFlowOutEdges g n) == 0
                ]
         mkNewInstruction n =
           let new_g = mergeNodes entry_b n g
               new_os = os { osGraph = new_g }
               new_emit_str = updateNodeInEmitStrTemplate (getNodeID entry_b)
                                                          (getNodeID n)
                                                          (instrEmitString i)
           in i { instrID = 0
                , instrOS = new_os
                , instrEmitString = new_emit_str
                }
     return $ map mkNewInstruction b_ns
