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

import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Constraints
import Language.InstSel.CPModel.Json
import Language.InstSel.CPModel.ParamMaker
import Language.InstSel.Graphs
import Language.InstSel.ProgramModules
import Language.InstSel.OpStructures
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachine.Targets.Test

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
                , (7, NodeLabel 7 (LabelNode $ BBLabelID "start"))
                , (8, NodeLabel 8 (LabelNode $ BBLabelID "middle"))
                , (9, NodeLabel 9 (LabelNode $ BBLabelID "end"))
                , (10, NodeLabel 10 (ControlNode O.CondBranch))
                , (11, NodeLabel 11 (ControlNode O.Ret))
                , (12, NodeLabel 12 (ComputationNode (O.UIntOp O.Add)))
                , (13, NodeLabel 13 (ComputationNode (O.UIntOp O.Add)))
                , (14, NodeLabel 14 PhiNode)
                , (15, NodeLabel 15 (ControlNode O.Branch))
                ])
                (map Edge
                [ (0,  10, EdgeLabel DataFlowEdge 0 1)
                , (1,  12, EdgeLabel DataFlowEdge 0 0)
                , (2,  12, EdgeLabel DataFlowEdge 0 1)
                , (3,  13, EdgeLabel DataFlowEdge 0 0)
                , (4,  14, EdgeLabel DataFlowEdge 0 1)
                , (4,  13, EdgeLabel DataFlowEdge 1 1)
                , (5,  14, EdgeLabel DataFlowEdge 0 2)
                , (6,  11, EdgeLabel DataFlowEdge 0 1)
                , (7,   0, EdgeLabel DataFlowEdge 0 0)
                , (7,   1, EdgeLabel DataFlowEdge 1 0)
                , (7,   2, EdgeLabel DataFlowEdge 2 0)
                , (7,   3, EdgeLabel DataFlowEdge 3 0)
                , (7,  10, EdgeLabel ControlFlowEdge 4 0)
                , (8,  15, EdgeLabel ControlFlowEdge 0 0)
                , (9,  11, EdgeLabel ControlFlowEdge 0 0)
                , (10,  8, EdgeLabel ControlFlowEdge 1 0)
                , (10,  9, EdgeLabel ControlFlowEdge 0 0)
                , (12,  4, EdgeLabel DataFlowEdge 0 0)
                , (13,  5, EdgeLabel DataFlowEdge 0 0)
                , (14,  6, EdgeLabel DataFlowEdge 0 0)
                , (15,  9, EdgeLabel ControlFlowEdge 0 1)
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
         params = mkParams func tmTest
     putStrLn $ toJson params
--     mapM_ (\nn -> (putStrLn $ show $ map convertMappingNToID nn))
--           (match func phi_pattern)
