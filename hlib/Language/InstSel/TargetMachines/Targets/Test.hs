--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachines.Targets.Test
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- A target machine for testing purposes.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstSel.TargetMachines.Targets.Test
  ( tmTest )
where

import Language.InstSel.Constraints
import Language.InstSel.Constraints.PCBuilder
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.Targets.Generic



-------------
-- Functions
-------------

tmTest :: TargetMachine
tmTest =
  let init_def_pattern =
        mkGraph
        ( map
            Node
            [ ( 0, NodeLabel 0 (LabelNode $ BasicBlockLabel "start") )
            , ( 1, NodeLabel 1 (DataNode D.AnyType Nothing) )
            ]
        )
        ( map
            Edge
            [ ( 0, 1, EdgeLabel DataFlowEdge 0 0 )
            ]
        )
      add_pattern =
        mkGraph
        ( map
            Node
            [ ( 0, NodeLabel 0 ( ComputationNode
                                   (O.CompArithOp $ O.UIntOp O.Add)
                               )
              )
            , ( 1, NodeLabel 1 (DataNode D.AnyType Nothing) )
            , ( 2, NodeLabel 2 (DataNode D.AnyType Nothing) )
            , ( 3, NodeLabel 3 (DataNode D.AnyType Nothing) )
            ]
        )
        ( map
           Edge
           [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
           , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
           , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
           ]
        )
      bnz_pattern =
        mkGraph
         ( map
             Node
             [ ( 0, NodeLabel 0 (DataNode D.AnyType Nothing) )
             , ( 1, NodeLabel 1 (LabelNode $ BasicBlockLabel "") )
             , ( 2, NodeLabel 2 (LabelNode $ BasicBlockLabel "") )
             , ( 3, NodeLabel 3 (LabelNode $ BasicBlockLabel "") )
             , ( 4, NodeLabel 4 (ControlNode O.CondBranch) )
             ]
         )
         ( map
             Edge
             [ ( 0, 4, EdgeLabel DataFlowEdge 0 1 )
             , ( 1, 4, EdgeLabel ControlFlowEdge 0 0 )
             , ( 4, 2, EdgeLabel ControlFlowEdge 0 0 )
             , ( 4, 3, EdgeLabel ControlFlowEdge 1 0 )
             ]
         )
      br_pattern =
        mkGraph
          ( map
              Node
              [ ( 0, NodeLabel 0 (LabelNode $ BasicBlockLabel "") )
              , ( 1, NodeLabel 1 (LabelNode $ BasicBlockLabel "") )
              , ( 2, NodeLabel 2 (ControlNode O.Branch) )
              ]
          )
          ( map
              Edge
              [ ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
              , ( 2, 1, EdgeLabel ControlFlowEdge 0 0 )
              ]
          )
      br_fallthrough_pattern = br_pattern
      ret_pattern =
        mkGraph
          ( map
              Node
              [ ( 0, NodeLabel 0 (DataNode D.AnyType Nothing) )
              , ( 1, NodeLabel 1 (LabelNode $ BasicBlockLabel "") )
              , ( 2, NodeLabel 2 (ControlNode O.Ret) )
              ]
          )
          ( map
              Edge
              [ ( 0, 2, EdgeLabel DataFlowEdge 0 1 )
              , ( 1, 2, EdgeLabel ControlFlowEdge 0 0 )
              ]
          )
      init_def_pattern_os =
        addBBAllocConstraints $ OpStructure init_def_pattern []
      add_pattern_os =
        addBBAllocConstraints $ OpStructure add_pattern []
      bnz_pattern_os =
        addBBAllocConstraints $ OpStructure  bnz_pattern []
      br_pattern_os =
        addBBAllocConstraints $ OpStructure br_pattern []
      br_fallthrough_pattern_os =
        addBBAllocConstraints $
          OpStructure
            br_fallthrough_pattern
            [ BoolExprConstraint $
                EqExpr
                  ( DistanceBetweenMatchAndLabelExpr
                      ThisMatchExpr
                      ( LabelOfLabelNodeExpr $
                        ANodeIDExpr 1
                      )
                  )
                  ( Int2NumExpr $
                      AnIntegerExpr 0
                  )
            ]
      ret_pattern_os =
        addBBAllocConstraints $ OpStructure ret_pattern []
      insts = [ Instruction
                  0
                  [ InstrPattern
                      0
                      init_def_pattern_os
                      []
                      True
                      (AssemblyString [])
                  ]
                  (InstrProperties 1 1)
              , Instruction
                  1
                  [ InstrPattern
                      0
                      add_pattern_os
                      [3]
                      True
                      ( AssemblyString
                          [ ASVerbatim "add "
                          , ASRegisterOfDataNode 3
                          , ASVerbatim ", "
                          , ASRegisterOfDataNode 1
                          , ASVerbatim ", "
                          , ASRegisterOfDataNode 2
                          ]
                      )
                  ]
                  (InstrProperties 1 1)
              , Instruction
                  2
                  [ InstrPattern
                      0
                      bnz_pattern_os
                      []
                      True
                      ( AssemblyString
                          [ ASVerbatim "bnz "
                          , ASRegisterOfDataNode 0
                          , ASVerbatim ", "
                          , ASBBLabelOfLabelNode 2
                          , ASVerbatim ", "
                          , ASBBLabelOfLabelNode 3
                          ]
                      )
                  ]
                  (InstrProperties 1 1)
              , Instruction
                  3
                  [ InstrPattern
                      0
                      br_pattern_os
                      []
                      True
                      ( AssemblyString
                          [ ASVerbatim "br "
                          , ASBBLabelOfLabelNode 1
                          ]
                      )
                  ]
                  (InstrProperties 1 1)
              , Instruction
                  4
                  [ InstrPattern
                      0
                      br_fallthrough_pattern_os
                      []
                      True
                      (AssemblyString [])
                  ]
                  (InstrProperties 1 1)
              , Instruction
                  5
                  [ InstrPattern
                      0
                      ret_pattern_os
                      []
                      True
                      ( AssemblyString
                        [ ASVerbatim "ret "
                        , ASRegisterOfDataNode 0
                        ]
                      )
                  ]
                  (InstrProperties 1 1)
              ]
  in TargetMachine
       (toTargetMachineID "test")
       (fixInstrIDs $ insts ++ mkGenericPhiInstructions)
       [ Register { regID = 0, regName = RegisterName "r0" }
       , Register { regID = 1, regName = RegisterName "r1" }
       , Register { regID = 2, regName = RegisterName "r2" }
       ]
