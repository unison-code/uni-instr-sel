--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachine.Targets.Test
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

module Language.InstSel.TargetMachine.Targets.Test
  ( tmTest )
where

import Language.InstSel.Constraints
import Language.InstSel.Constraints.PCBuilder
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.ProgramModules.IDs
  ( BasicBlockLabel (..) )
import Language.InstSel.TargetMachine



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
      phi_pattern =
        mkGraph
          ( map
              Node
              [ ( 0, NodeLabel 0 (DataNode D.AnyType Nothing) )
              , ( 1, NodeLabel 1 (DataNode D.AnyType Nothing) )
              , ( 2, NodeLabel 2 (DataNode D.AnyType Nothing) )
              , ( 3, NodeLabel 3 PhiNode )
              ]
          )
          ( map
              Edge
              [ ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
              , ( 1, 3, EdgeLabel DataFlowEdge 0 1 )
              , ( 3, 2, EdgeLabel DataFlowEdge 0 0 )
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
      phi_pattern_os =
        OpStructure
          phi_pattern
          [ BoolExprConstraint $
              AndExpr
                ( EqExpr
                    ( Register2NumExpr $
                        RegisterAllocatedToDataNodeExpr $
                          ANodeIDExpr 0
                    )
                    ( Register2NumExpr $
                        RegisterAllocatedToDataNodeExpr $
                          ANodeIDExpr 1
                    )
                )
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
          ]
      insts = [ Instruction
                  0
                  [ InstPattern
                      0
                      init_def_pattern_os
                      []
                      True
                      []
                  ]
                  (InstProperties 1 1)
                  (AssemblyString [])
              , Instruction
                  1
                  [ InstPattern
                      0
                      add_pattern_os
                      [3]
                      True
                      [3, 1, 2]
                  ]
                  (InstProperties 1 1)
                  ( AssemblyString
                      [ AssemblyVerbatim "add "
                      , AssemblyRegister 0
                      , AssemblyVerbatim ", "
                      , AssemblyRegister 1
                      , AssemblyVerbatim ", "
                      , AssemblyRegister 2
                      ]
                  )
              , Instruction
                  2
                  [ InstPattern
                      0
                      bnz_pattern_os
                      []
                      True
                      [0, 2, 3]
                  ]
                  (InstProperties 1 1)
                  ( AssemblyString
                      [ AssemblyVerbatim "bnz "
                      , AssemblyRegister 0
                      , AssemblyVerbatim ", "
                      , AssemblyBBLabel 1
                      , AssemblyVerbatim ", "
                      , AssemblyBBLabel 2
                      ]
                  )
              , Instruction
                  3
                  [ InstPattern
                      0
                      br_pattern_os
                      []
                      True
                      [1]
                  ]
                  (InstProperties 1 1)
                  ( AssemblyString
                      [ AssemblyVerbatim "br "
                      , AssemblyBBLabel 0
                      ]
                  )
              , Instruction
                  4
                  [ InstPattern
                      0
                      br_fallthrough_pattern_os
                      []
                      True
                      []
                  ]
                  (InstProperties 1 1)
                  (AssemblyString [])
              , Instruction
                  5
                  [ InstPattern
                      0
                      ret_pattern_os
                      []
                      True
                      [0]
                  ]
                  (InstProperties 1 1)
                  ( AssemblyString
                    [ AssemblyVerbatim "ret "
                    , AssemblyRegister 0
                    ]
                  )
              , Instruction
                  6
                  [ InstPattern
                      0
                      phi_pattern_os
                      [2]
                      False
                      [2, 0, 1]
                  ]
                  (InstProperties 1 1)
                  ( AssemblyString
                      [ AssemblyVerbatim "phi "
                      , AssemblyRegister 0
                      , AssemblyVerbatim " ("
                      , AssemblyRegister 1
                      , AssemblyVerbatim ", ?"
                      , AssemblyVerbatim ") ("
                      , AssemblyRegister 2
                      , AssemblyVerbatim ", ?"
                      , AssemblyVerbatim ")"
                      ]
                  )
              ]
  in TargetMachine
       (toTargetMachineID "test")
       insts
       [ Register { regID = 0, regName = RegisterName "r0" }
       , Register { regID = 1, regName = RegisterName "r1" }
       , Register { regID = 2, regName = RegisterName "r2" }
       ]
