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
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Graphs.VFTwo
import Language.InstructionSelection.OpStructures
import Language.InstructionSelection.Patterns
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.CPModel
import Language.InstructionSelection.CPModel.JsonDumper
import Language.InstructionSelection.CPModel.ParamMaker
import Language.InstructionSelection.PrettyPrint
import Language.InstructionSelection.Utils (toNatural)
import Data.List ( mapAccumL
                 , zip5
                 )

main :: IO ()
main =
  do let func = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (DataNode D.UnknownType) ""))
                , (1, NodeLabel 1 (NodeInfo (DataNode D.UnknownType) ""))
                , (2, NodeLabel 2 (NodeInfo (DataNode D.UnknownType) ""))
                , (3, NodeLabel 3 (NodeInfo (DataNode D.UnknownType) ""))
                , (4, NodeLabel 4 (NodeInfo (DataNode D.UnknownType) ""))
                , (5, NodeLabel 5 (NodeInfo (DataNode D.UnknownType) ""))
                , (6, NodeLabel 6 (NodeInfo (DataNode D.UnknownType) ""))
                , (7, NodeLabel 7 (NodeInfo (LabelNode $ BBLabel "start") ""))
                , (8, NodeLabel 8 (NodeInfo (LabelNode $ BBLabel "middle") ""))
                , (9, NodeLabel 9 (NodeInfo (LabelNode $ BBLabel "end") ""))
                , (10, NodeLabel 10 (NodeInfo (ControlNode O.CondBranch) ""))
                , (11, NodeLabel 11 (NodeInfo (ControlNode O.Ret) ""))
                , (12, NodeLabel 12 (NodeInfo (ComputationNode (O.UIntOp O.Add))
                                     ""))
                , (13, NodeLabel 13 (NodeInfo (ComputationNode (O.UIntOp O.Add))
                                     ""))
                , (14, NodeLabel 14 (NodeInfo PhiNode ""))
                , (15, NodeLabel 15 (NodeInfo (ControlNode O.UncondBranch)
                                              ""))
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
         init_def_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (LabelNode $ BBLabel "start") ""))
                , (1, NodeLabel 1 (NodeInfo (DataNode D.UnknownType) ""))
                ])
                (map Edge
                [ (0, 1, EdgeLabel 0 0)
                ])
         add_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (ComputationNode (O.UIntOp O.Add))
                                            ""))
                , (1, NodeLabel 1 (NodeInfo (DataNode D.UnknownType) ""))
                , (2, NodeLabel 2 (NodeInfo (DataNode D.UnknownType) ""))
                , (3, NodeLabel 3 (NodeInfo (DataNode D.UnknownType) ""))
                ])
                (map Edge
                [ (1, 0, EdgeLabel 0 0)
                , (2, 0, EdgeLabel 0 1)
                , (0, 3, EdgeLabel 0 0)
                ])
         bnz_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (DataNode D.UnknownType) ""))
                , (1, NodeLabel 1 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (2, NodeLabel 2 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (3, NodeLabel 3 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (4, NodeLabel 4 (NodeInfo (ControlNode O.CondBranch) ""))
                ])
                (map Edge
                [ (0, 4, EdgeLabel 0 1)
                , (1, 4, EdgeLabel 0 0)
                , (4, 2, EdgeLabel 0 0)
                , (4, 3, EdgeLabel 1 0)
                ])
         br_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (1, NodeLabel 1 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (2, NodeLabel 2 (NodeInfo (ControlNode O.UncondBranch) ""))
                ])
                (map Edge
                [ (0, 2, EdgeLabel 0 0)
                , (2, 1, EdgeLabel 0 0)
                ])
         ret_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (DataNode D.UnknownType) ""))
                , (1, NodeLabel 1 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (2, NodeLabel 2 (NodeInfo (ControlNode O.Ret) ""))
                ])
                (map Edge
                [ (0, 2, EdgeLabel 0 1)
                , (1, 2, EdgeLabel 0 0)
                ])
         phi_pattern = mkGraph
                (map Node
                [ (0, NodeLabel 0 (NodeInfo (DataNode D.UnknownType) ""))
                , (1, NodeLabel 1 (NodeInfo (DataNode D.UnknownType) ""))
                , (2, NodeLabel 2 (NodeInfo (DataNode D.UnknownType) ""))
                , (3, NodeLabel 3 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (4, NodeLabel 4 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (5, NodeLabel 5 (NodeInfo (LabelNode $ BBLabel "") ""))
                , (6, NodeLabel 6 (NodeInfo NullNode ""))
                , (7, NodeLabel 7 (NodeInfo NullNode ""))
                , (8, NodeLabel 8 (NodeInfo PhiNode ""))
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
         patterns = [ init_def_pattern
                    , add_pattern
                    , bnz_pattern
                    , br_pattern
                    , ret_pattern
                    , phi_pattern
                    ]
         constraints = [ []
                       , []
                       , []
                       , []
                       , []
                       , [ Constraint $
                           AndExpr
                           (
                             EqExpr
                             (
                               RegisterId2NumExpr $
                               RegisterIdAllocatedToDataNodeExpr $
                               ANodeIdExpr 0
                             )
                             (
                               RegisterId2NumExpr $
                               RegisterIdAllocatedToDataNodeExpr $
                               ANodeIdExpr 1
                             )
                           )
                           (
                             EqExpr
                             (
                               RegisterId2NumExpr $
                               RegisterIdAllocatedToDataNodeExpr $
                               ANodeIdExpr 1
                             )
                             (
                               RegisterId2NumExpr $
                               RegisterIdAllocatedToDataNodeExpr $
                               ANodeIdExpr 2
                             )
                           )
                         , Constraint $
                           EqExpr
                           (
                             LabelId2NumExpr $
                             LabelIdOfLabelNodeExpr $
                             ANodeIdExpr 5
                           )
                           (
                             LabelId2NumExpr $
                             LabelIdAllocatedToInstanceExpr $
                             ThisInstanceIdExpr
                           )
                         , Constraint $
                           AndExpr
                           (
                             InSetExpr
                             (
                               LabelId2SetElemExpr $
                               LabelIdAllocatedToInstanceExpr $
                               DefinerOfEntityNodeExpr $
                               ANodeIdExpr 0
                             )
                             (
                               DomSetOfLabelIdExpr $
                               LabelIdOfLabelNodeExpr $
                               ANodeIdExpr 3
                             )
                           )
                           (
                             InSetExpr
                             (
                               LabelId2SetElemExpr $
                               LabelIdAllocatedToInstanceExpr $
                               DefinerOfEntityNodeExpr $
                               ANodeIdExpr 1
                             )
                             (
                               DomSetOfLabelIdExpr $
                               LabelIdOfLabelNodeExpr $
                               ANodeIdExpr 4
                             )
                           )
                         , Constraint $
                           NotExpr $
                           (
                             AndExpr
                             (
                               InSetExpr
                               (
                                 LabelId2SetElemExpr $
                                 LabelIdAllocatedToInstanceExpr $
                                 DefinerOfEntityNodeExpr $
                                 ANodeIdExpr 0
                               )
                               (
                                 IntersectSetExpr
                                 (
                                   DomSetOfLabelIdExpr $
                                   LabelIdOfLabelNodeExpr $
                                   ANodeIdExpr 3
                                 )
                                 (
                                   DomSetOfLabelIdExpr $
                                   LabelIdOfLabelNodeExpr $
                                   ANodeIdExpr 4
                                 )
                               )
                             )
                             (
                               InSetExpr
                               (
                                 LabelId2SetElemExpr $
                                 LabelIdAllocatedToInstanceExpr $
                                 DefinerOfEntityNodeExpr $
                                 ANodeIdExpr 1
                               )
                               (
                                 IntersectSetExpr
                                 (
                                   DomSetOfLabelIdExpr $
                                   LabelIdOfLabelNodeExpr $
                                   ANodeIdExpr 3
                                 )
                                 (
                                   DomSetOfLabelIdExpr $
                                   LabelIdOfLabelNodeExpr $
                                   ANodeIdExpr 4
                                 )
                               )
                             )
                           )
                         ]
                       ]
         no_use_def_cs = [ False
                         , False
                         , False
                         , False
                         , False
                         , True
                         ]
         inst_props = [ InstProperties 1 1
                      , InstProperties 1 1
                      , InstProperties 1 1
                      , InstProperties 1 1
                      , InstProperties 1 1
                      , InstProperties 1 1
                      ]
         list_of_matchsets = map (match func) patterns
         (_, list_of_matchsets_with_ids) =
           mapAccumL (\curr sets -> ( curr + (toInstanceId $ length sets)
                                    , zip sets [curr..]
                                    )
                     )
                     0
                     list_of_matchsets
         pattern_data =
           map (\(pat, c, m, prop, b) -> (OpStructure pat c, m, prop, b))
               (zip5 patterns
                     constraints
                     list_of_matchsets_with_ids
                     inst_props
                     no_use_def_cs)
         params = mkParams (OpStructure func []) pattern_data
     putStrLn $ toJson params
--     mapM_ (\nn -> (putStrLn $ show $ map convertMappingNToId nn))
--           (match func phi_pattern)
