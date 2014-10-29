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
          [ (0, NodeLabel 0 (LabelNode $ BBLabelID "start"))
          , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
          ]
        )
        ( map Edge
          [ (0, 1, EdgeLabel 0 0)
          ]
        )
      add_pattern =
        mkGraph
        ( map
          Node
          [ (0, NodeLabel 0 (ComputationNode (O.UIntOp O.Add)))
          , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
          , (2, NodeLabel 2 (DataNode D.AnyType Nothing))
          , (3, NodeLabel 3 (DataNode D.AnyType Nothing))
          ]
        )
        ( map
          Edge
          [ (1, 0, EdgeLabel 0 0)
          , (2, 0, EdgeLabel 0 1)
          , (0, 3, EdgeLabel 0 0)
          ]
        )
      bnz_pattern =
        mkGraph
        ( map
          Node
          [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
          , (1, NodeLabel 1 (LabelNode $ BBLabelID ""))
          , (2, NodeLabel 2 (LabelNode $ BBLabelID ""))
          , (3, NodeLabel 3 (LabelNode $ BBLabelID ""))
          , (4, NodeLabel 4 (ControlNode O.IfBranch))
          ]
        )
        ( map
          Edge
          [ (0, 4, EdgeLabel 0 1)
          , (1, 4, EdgeLabel 0 0)
          , (4, 2, EdgeLabel 0 0)
          , (4, 3, EdgeLabel 1 0)
          ]
        )
      br_pattern =
        mkGraph
        ( map
          Node
          [ (0, NodeLabel 0 (LabelNode $ BBLabelID ""))
          , (1, NodeLabel 1 (LabelNode $ BBLabelID ""))
          , (2, NodeLabel 2 (ControlNode O.Branch))
          ]
        )
        ( map
          Edge
          [ (0, 2, EdgeLabel 0 0)
          , (2, 1, EdgeLabel 0 0)
          ]
        )
      br_fallthrough_pattern = br_pattern
      ret_pattern =
        mkGraph
        ( map
          Node
          [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
          , (1, NodeLabel 1 (LabelNode $ BBLabelID ""))
          , (2, NodeLabel 2 (ControlNode O.Ret))
          ]
        )
        ( map
          Edge
          [ (0, 2, EdgeLabel 0 1)
          , (1, 2, EdgeLabel 0 0)
          ]
        )
      phi_pattern =
        mkGraph
        ( map
          Node
          [ (0, NodeLabel 0 (DataNode D.AnyType Nothing))
          , (1, NodeLabel 1 (DataNode D.AnyType Nothing))
          , (2, NodeLabel 2 (DataNode D.AnyType Nothing))
          , (3, NodeLabel 3 (LabelNode $ BBLabelID ""))
          , (4, NodeLabel 4 (LabelNode $ BBLabelID ""))
          , (5, NodeLabel 5 (LabelNode $ BBLabelID ""))
          , (6, NodeLabel 6 NullNode)
          , (7, NodeLabel 7 NullNode)
          , (8, NodeLabel 8 PhiNode)
          ]
        )
        ( map
          Edge
          [ (0, 8, EdgeLabel 0 1)
          , (1, 8, EdgeLabel 0 2)
          , (8, 2, EdgeLabel 0 0)
          , (3, 6, EdgeLabel 0 0)
          , (4, 7, EdgeLabel 0 0)
          , (6, 5, EdgeLabel 0 0)
          , (7, 5, EdgeLabel 0 1)
          , (5, 8, EdgeLabel 0 0)
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
          ( DistanceBetweenInstanceAndLabelExpr
            ThisPatternInstanceExpr
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
        , BoolExprConstraint $
          EqExpr
          ( Label2NumExpr $
            LabelOfLabelNodeExpr $
            ANodeIDExpr 5
          )
          ( Label2NumExpr $
            LabelAllocatedToPatternInstanceExpr $
            ThisPatternInstanceExpr
          )
        , BoolExprConstraint $
          AndExpr
          ( InSetExpr
            ( Label2SetElemExpr $
              LabelAllocatedToPatternInstanceExpr $
              DefinerOfDataNodeExpr $
              ANodeIDExpr 0
            )
            ( DomSetOfLabelExpr $
              LabelOfLabelNodeExpr $
              ANodeIDExpr 3
            )
          )
          ( InSetExpr
            ( Label2SetElemExpr $
              LabelAllocatedToPatternInstanceExpr $
              DefinerOfDataNodeExpr $
              ANodeIDExpr 1
            )
            ( DomSetOfLabelExpr $
              LabelOfLabelNodeExpr $
              ANodeIDExpr 4
            )
          )
        , BoolExprConstraint $
          NotExpr $
          ( AndExpr
            ( InSetExpr
              ( Label2SetElemExpr $
                LabelAllocatedToPatternInstanceExpr $
                DefinerOfDataNodeExpr $
                ANodeIDExpr 0
              )
              ( IntersectSetExpr
                ( DomSetOfLabelExpr $
                  LabelOfLabelNodeExpr $
                  ANodeIDExpr 3
                )
                ( DomSetOfLabelExpr $
                  LabelOfLabelNodeExpr $
                  ANodeIDExpr 4
                )
              )
            )
            ( InSetExpr
              ( Label2SetElemExpr $
                LabelAllocatedToPatternInstanceExpr $
                DefinerOfDataNodeExpr $
                ANodeIDExpr 1
              )
              ( IntersectSetExpr
                ( DomSetOfLabelExpr $
                  LabelOfLabelNodeExpr $
                  ANodeIDExpr 3
                )
                ( DomSetOfLabelExpr $
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
                  [2, 0, 4, 1, 5]
                ]
                (InstProperties 1 1)
                ( AssemblyString
                  [ AssemblyVerbatim "phi "
                  , AssemblyRegister 0
                  , AssemblyVerbatim " ("
                  , AssemblyRegister 1
                  , AssemblyVerbatim ", "
                  , AssemblyBBLabel 2
                  , AssemblyVerbatim ") ("
                  , AssemblyRegister 3
                  , AssemblyVerbatim ", "
                  , AssemblyBBLabel 4
                  , AssemblyVerbatim ")"
                  ]
                )

              ]
  in TargetMachine
     (toTargetMachineID "test")
     insts
     [ (0, RegisterName "r0")
     , (1, RegisterName "r1")
     , (2, RegisterName "r2")
     ]
