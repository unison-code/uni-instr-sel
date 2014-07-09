{-
Copyright (c) 2013-2014, Gabriel Hjort Blindell <ghb@kth.se>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-
Tests the implementation of the VF2 algorithm.
-}

import qualified Language.InstructionSelection.DataTypes as D
import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Constraints.PCBuilder
import Language.InstructionSelection.CPModel.Json
import Language.InstructionSelection.CPModel.ParamMaker
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Patterns
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.ProgramModules
import Language.InstructionSelection.OpStructures
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.TargetMachine

main :: IO ()
main =
  do let func_g = mkGraph
                (map Node
                [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
                , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
                , (2, NodeLabel 2 (DataNode D.AnyType Nothing))
                , (3, NodeLabel 3 (DataNode D.AnyType Nothing))
                , (4, NodeLabel 4 (DataNode D.AnyType Nothing))
                , (5, NodeLabel 5 (DataNode D.AnyType Nothing))
                , (6, NodeLabel 6 (DataNode D.AnyType Nothing))
                , (7, NodeLabel 7 (LabelNode $ BBLabel "start"))
                , (8, NodeLabel 8 (LabelNode $ BBLabel "middle"))
                , (9, NodeLabel 9 (LabelNode $ BBLabel "end"))
                , (10, NodeLabel 10 (ControlNode O.CondBranch))
                , (11, NodeLabel 11 (ControlNode O.Ret))
                , (12, NodeLabel 12 (ComputationNode (O.UIntOp O.Add)))
                , (13, NodeLabel 13 (ComputationNode (O.UIntOp O.Add)))
                , (14, NodeLabel 14 PhiNode)
                , (15, NodeLabel 15 (ControlNode O.UncondBranch))
                ])
                (map Edge
                [ (0, 10, EdgeLabel 0 1)
                , (1, 12, EdgeLabel 0 0)
                , (2, 12, EdgeLabel 0 1)
                , (3, 13, EdgeLabel 0 0)
                , (4, 14, EdgeLabel 0 1)
                , (4, 13, EdgeLabel 1 1)
                , (5, 14, EdgeLabel 0 2)
                , (6, 11, EdgeLabel 0 1)
                , (7, 0, EdgeLabel 0 0)
                , (7, 1, EdgeLabel 1 0)
                , (7, 2, EdgeLabel 2 0)
                , (7, 3, EdgeLabel 3 0)
                , (7, 10, EdgeLabel 4 0)
                , (8, 15, EdgeLabel 0 0)
                , (9, 11, EdgeLabel 0 0)
                , (9, 14, EdgeLabel 1 0)
                , (10, 8, EdgeLabel 1 0)
                , (10, 9, EdgeLabel 0 0)
                , (12, 4, EdgeLabel 0 0)
                , (13, 5, EdgeLabel 0 0)
                , (14, 6, EdgeLabel 0 0)
                , (15, 9, EdgeLabel 0 1)
                ])
         func_cs = [ BoolExprConstraint $
                     DataNodeIsAnIntConstantExpr $
                     ANodeIDExpr 0
                   , BoolExprConstraint $
                     EqExpr
                     (
                       Int2NumExpr $
                       IntConstValueOfDataNodeExpr $
                       ANodeIDExpr 0
                     )
                     (
                       Int2NumExpr $
                       AnIntegerExpr 0
                     )
                   ]
         func = Function
                "test"
                (OpStructure func_g func_cs)
                []
                []

         init_def_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (LabelNode $ BBLabel "start"))
                , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
                ])
                (map Edge
                [ (0, 1, EdgeLabel 0 0)
                ])
         add_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (ComputationNode (O.UIntOp O.Add))
                                           )
                , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
                , (2, NodeLabel 2 (DataNode D.AnyType Nothing))
                , (3, NodeLabel 3 (DataNode D.AnyType Nothing))
                ])
                (map Edge
                [ (1, 0, EdgeLabel 0 0)
                , (2, 0, EdgeLabel 0 1)
                , (0, 3, EdgeLabel 0 0)
                ])
         bnz_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
                , (1, NodeLabel 1 (LabelNode $ BBLabel ""))
                , (2, NodeLabel 2 (LabelNode $ BBLabel ""))
                , (3, NodeLabel 3 (LabelNode $ BBLabel ""))
                , (4, NodeLabel 4 (ControlNode O.CondBranch))
                ])
                (map Edge
                [ (0, 4, EdgeLabel 0 1)
                , (1, 4, EdgeLabel 0 0)
                , (4, 2, EdgeLabel 0 0)
                , (4, 3, EdgeLabel 1 0)
                ])
         br_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (LabelNode $ BBLabel ""))
                , (1, NodeLabel 1 (LabelNode $ BBLabel ""))
                , (2, NodeLabel 2 (ControlNode O.UncondBranch))
                ])
                (map Edge
                [ (0, 2, EdgeLabel 0 0)
                , (2, 1, EdgeLabel 0 0)
                ])
         br_fallthrough_pattern = br_pattern
         ret_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
                , (1, NodeLabel 1 (LabelNode $ BBLabel ""))
                , (2, NodeLabel 2 (ControlNode O.Ret))
                ])
                (map Edge
                [ (0, 2, EdgeLabel 0 1)
                , (1, 2, EdgeLabel 0 0)
                ])
         phi_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
                , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
                , (2, NodeLabel 2 (DataNode D.AnyType Nothing))
                , (3, NodeLabel 3 (LabelNode $ BBLabel ""))
                , (4, NodeLabel 4 (LabelNode $ BBLabel ""))
                , (5, NodeLabel 5 (LabelNode $ BBLabel ""))
                , (6, NodeLabel 6 NullNode)
                , (7, NodeLabel 7 NullNode)
                , (8, NodeLabel 8 PhiNode)
                ])
                (map Edge
                [ (0, 8, EdgeLabel 0 1)
                , (1, 8, EdgeLabel 0 2)
                , (8, 2, EdgeLabel 0 0)
                , (3, 6, EdgeLabel 0 0)
                , (4, 7, EdgeLabel 0 0)
                , (6, 5, EdgeLabel 0 0)
                , (7, 5, EdgeLabel 0 1)
                , (5, 8, EdgeLabel 0 0)
                ])
         init_def_pattern_cs = mkBBAllocConstraints init_def_pattern
         add_pattern_cs = mkBBAllocConstraints add_pattern
         bnz_pattern_cs = mkBBAllocConstraints bnz_pattern
         br_pattern_cs = mkBBAllocConstraints br_pattern
         br_fallthrough_pattern_cs =
           mkBBAllocConstraints br_fallthrough_pattern
           ++
           [ BoolExprConstraint $
             EqExpr
             (
               DistanceBetweenInstanceAndLabelExpr
               (
                 ThisPatternInstanceExpr
               )
               (
                 LabelOfLabelNodeExpr $
                 ANodeIDExpr 1
               )
             )
             (
               Int2NumExpr $
               AnIntegerExpr 0
             )
           ]
         ret_pattern_cs =  mkBBAllocConstraints ret_pattern
         phi_pattern_cs =
           [ BoolExprConstraint $
             AndExpr
             (
               EqExpr
               (
                 Register2NumExpr $
                 RegisterAllocatedToDataNodeExpr $
                 ANodeIDExpr 0
               )
               (
                 Register2NumExpr $
                 RegisterAllocatedToDataNodeExpr $
                 ANodeIDExpr 1
               )
             )
             (
               EqExpr
               (
                 Register2NumExpr $
                 RegisterAllocatedToDataNodeExpr $
                 ANodeIDExpr 1
               )
               (
                 Register2NumExpr $
                 RegisterAllocatedToDataNodeExpr $
                 ANodeIDExpr 2
               )
             )
           , BoolExprConstraint $
             EqExpr
             (
               Label2NumExpr $
               LabelOfLabelNodeExpr $
               ANodeIDExpr 5
             )
             (
               Label2NumExpr $
               LabelAllocatedToPatternInstanceExpr $
               ThisPatternInstanceExpr
             )
           , BoolExprConstraint $
             AndExpr
             (
               InSetExpr
               (
                 Label2SetElemExpr $
                 LabelAllocatedToPatternInstanceExpr $
                 DefinerOfDataNodeExpr $
                 ANodeIDExpr 0
               )
               (
                 DomSetOfLabelExpr $
                 LabelOfLabelNodeExpr $
                 ANodeIDExpr 3
               )
             )
             (
               InSetExpr
               (
                 Label2SetElemExpr $
                 LabelAllocatedToPatternInstanceExpr $
                 DefinerOfDataNodeExpr $
                 ANodeIDExpr 1
               )
               (
                 DomSetOfLabelExpr $
                 LabelOfLabelNodeExpr $
                 ANodeIDExpr 4
               )
             )
           , BoolExprConstraint $
             NotExpr $
             (
               AndExpr
               (
                 InSetExpr
                 (
                   Label2SetElemExpr $
                   LabelAllocatedToPatternInstanceExpr $
                   DefinerOfDataNodeExpr $
                   ANodeIDExpr 0
                 )
                 (
                   IntersectSetExpr
                   (
                     DomSetOfLabelExpr $
                     LabelOfLabelNodeExpr $
                     ANodeIDExpr 3
                   )
                   (
                     DomSetOfLabelExpr $
                     LabelOfLabelNodeExpr $
                     ANodeIDExpr 4
                   )
                 )
               )
               (
                 InSetExpr
                 (
                   Label2SetElemExpr $
                   LabelAllocatedToPatternInstanceExpr $
                   DefinerOfDataNodeExpr $
                   ANodeIDExpr 1
                 )
                 (
                   IntersectSetExpr
                   (
                     DomSetOfLabelExpr $
                     LabelOfLabelNodeExpr $
                     ANodeIDExpr 3
                   )
                   (
                     DomSetOfLabelExpr $
                     LabelOfLabelNodeExpr $
                     ANodeIDExpr 4
                   )
                 )
               )
             )
           ]
         insts = [ Instruction
                   0
                   [ InstPattern
                     0
                     (OpStructure init_def_pattern init_def_pattern_cs)
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   1
                   [ InstPattern
                     0
                     (OpStructure add_pattern add_pattern_cs)
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   2
                   [ InstPattern
                     0
                     (OpStructure bnz_pattern bnz_pattern_cs)
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   1
                   [ InstPattern
                     0
                     (OpStructure br_pattern br_pattern_cs)
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   1
                   [ InstPattern
                     0
                     ( OpStructure
                       br_fallthrough_pattern
                       br_fallthrough_pattern_cs
                     )
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   1
                   [ InstPattern
                     0
                     (OpStructure ret_pattern ret_pattern_cs)
                     True
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 , Instruction
                   1
                   [ InstPattern
                     0
                     (OpStructure phi_pattern phi_pattern_cs)
                     False
                     []
                   ]
                   (InstProperties 1 1)
                   (AssemblyString [])

                 ]
         target = TargetMachine [ ("r0", 0)
                                , ("r1", 1)
                                , ("r2", 2)
                                ]
         params = mkParams func
                           insts
                           target
     putStrLn $ toJson params
--     mapM_ (\nn -> (putStrLn $ show $ map convertMappingNToID nn))
--           (match func phi_pattern)
