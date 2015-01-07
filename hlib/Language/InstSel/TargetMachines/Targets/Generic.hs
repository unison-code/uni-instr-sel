--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachines.Targets.Generic
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating generic instructions, and other useful help functions.
--
--------------------------------------------------------------------------------

module Language.InstSel.TargetMachines.Targets.Generic
  ( fixInstrIDs
  , fixRegIDs
  , mkGenericBrFallthroughInstructions
  , mkGenericPhiInstructions
  )
where

import Language.InstSel.Constraints
import Language.InstSel.Constraints.PCBuilder
import Language.InstSel.DataTypes
  ( DataType (..) )
import Language.InstSel.Graphs
import qualified Language.InstSel.OpStructures as OS
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachines.Base



-------------
-- Functions
-------------

-- | Creates a set of instructions for handling the generic cases where
-- 'PhiNode's appear. The instruction IDs of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkGenericPhiInstructions :: [Instruction]
mkGenericPhiInstructions =
  let mkDataNode = DataNode { dataType = AnyType, dataOrigin = Nothing }
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 PhiNode )
                , ( 1, NodeLabel 1 mkDataNode )
                , ( 2, NodeLabel 2 mkDataNode )
                , ( 3, NodeLabel 3 mkDataNode )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      cs = [ BoolExprConstraint $
               AndExpr
                 ( EqExpr
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 1
                     )
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 2
                     )
                 )
                 ( EqExpr
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 2
                     )
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 3
                     )
                 )
           ]
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = False
              , patAssemblyStr = ( AssemblyString
                                     [ ASVerbatim "phi "
                                     , ASRegisterOfDataNode 3
                                     , ASVerbatim " ("
                                     , ASRegisterOfDataNode 1
                                     , ASVerbatim ", "
                                     , ASBBLabelOfDataNode 1
                                     , ASVerbatim ") ("
                                     , ASRegisterOfDataNode 2
                                     , ASVerbatim ", "
                                     , ASBBLabelOfDataNode 2
                                     , ASVerbatim ")"
                                     ]
                                 )
              }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps =
             ( InstrProperties { instrCodeSize = 0, instrLatency = 0 } )
         }
     ]

-- | Creates a set of instructions for handling unconditional branching to the
-- immediately following basic block (that is, fallthroughs). The instruction
-- IDs of all instructions will be (incorrectly) set to 0, meaning they must be
-- reassigned afterwards.
mkGenericBrFallthroughInstructions :: [Instruction]
mkGenericBrFallthroughInstructions =
  let mkLabelNode = LabelNode $ BasicBlockLabel ""
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.CondBranch) )
                , ( 1, NodeLabel 1 mkLabelNode )
                , ( 2, NodeLabel 2 mkLabelNode )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                ]
            )
      bb_alloc_cs = mkBBAllocConstraints g
      fallthrough_cs = mkFallthroughConstraints 2
      cs = bb_alloc_cs ++ fallthrough_cs
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 0, instrLatency = 0 }
         }
     ]

-- | In order to not have to concern ourselves with instruction IDs being
-- unique, we let this function fix those for us afterwards. The function goes
-- over the list of instructions and reassigns the instruction IDs such that
-- each instruction gets a unique ID.
fixInstrIDs :: [Instruction] -> [Instruction]
fixInstrIDs insts =
  map ( \(new_iid, inst) -> inst { instrID = new_iid } ) (zip [0..] insts)

-- | Same as 'fixInstrIDs' but for register IDs.
fixRegIDs :: [Register] -> [Register]
fixRegIDs regs =
  map ( \(new_rid, r) -> r { regID = new_rid } ) (zip [0..] regs)
