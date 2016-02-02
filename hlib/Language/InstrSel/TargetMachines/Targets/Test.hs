--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Targets.Test
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
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

module Language.InstrSel.TargetMachines.Targets.Test
  ( tmTest )
where

import Language.InstrSel.Constraints.ConstraintBuilder
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.Targets.Generic



-------------
-- Functions
-------------

tmTest :: TargetMachine
tmTest =
  let init_def_pattern =
        mkGraph
        ( map
            Node
            [ ( 0, NodeLabel 0 (BlockNode $ BlockName "start") )
            , ( 1, NodeLabel 1 (ValueNode D.AnyType Nothing) )
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
            , ( 1, NodeLabel 1 (ValueNode D.AnyType Nothing) )
            , ( 2, NodeLabel 2 (ValueNode D.AnyType Nothing) )
            , ( 3, NodeLabel 3 (ValueNode D.AnyType Nothing) )
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
             [ ( 0, NodeLabel 0 (ValueNode D.AnyType Nothing) )
             , ( 1, NodeLabel 1 (BlockNode $ BlockName "") )
             , ( 2, NodeLabel 2 (BlockNode $ BlockName "") )
             , ( 3, NodeLabel 3 (BlockNode $ BlockName "") )
             , ( 4, NodeLabel 4 (ControlNode O.CondBr) )
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
              [ ( 0, NodeLabel 0 (BlockNode $ BlockName "") )
              , ( 1, NodeLabel 1 (BlockNode $ BlockName "") )
              , ( 2, NodeLabel 2 (ControlNode O.Br) )
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
              [ ( 0, NodeLabel 0 (ValueNode D.AnyType Nothing) )
              , ( 1, NodeLabel 1 (BlockNode $ BlockName "") )
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
        addMatchPlacementConstraints
        $ OpStructure init_def_pattern (Just 0) []
      add_pattern_os =
        addMatchPlacementConstraints
        $ OpStructure add_pattern Nothing []
      bnz_pattern_os =
        addMatchPlacementConstraints
        $ OpStructure  bnz_pattern (Just 1) []
      br_pattern_os =
        addMatchPlacementConstraints $ OpStructure br_pattern (Just 0) []
      br_fallthrough_pattern_os =
        addMatchPlacementConstraints
        $ addFallThroughConstraints 1
        $ OpStructure br_fallthrough_pattern
                      (Just 0)
                      []
      ret_pattern_os =
        addMatchPlacementConstraints $ OpStructure ret_pattern (Just 1) []
      insts = [ Instruction
                  0
                  [ InstrPattern
                      0
                      init_def_pattern_os
                      True
                      (ASSTemplate [])
                  ]
                  (InstrProperties 1 1 True False)
              , Instruction
                  1
                  [ InstrPattern
                      0
                      add_pattern_os
                      True
                      ( ASSTemplate
                          [ ASVerbatim "add "
                          , ASLocationOfValueNode 3
                          , ASVerbatim ", "
                          , ASLocationOfValueNode 1
                          , ASVerbatim ", "
                          , ASLocationOfValueNode 2
                          ]
                      )
                  ]
                  (InstrProperties 1 1 True False)
              , Instruction
                  2
                  [ InstrPattern
                      0
                      bnz_pattern_os
                      True
                      ( ASSTemplate
                          [ ASVerbatim "bnz "
                          , ASLocationOfValueNode 0
                          , ASVerbatim ", "
                          , ASNameOfBlockNode 2
                          , ASVerbatim ", "
                          , ASNameOfBlockNode 3
                          ]
                      )
                  ]
                  (InstrProperties 1 1 True False)
              , Instruction
                  3
                  [ InstrPattern
                      0
                      br_pattern_os
                      True
                      ( ASSTemplate
                          [ ASVerbatim "br "
                          , ASNameOfBlockNode 1
                          ]
                      )
                  ]
                  (InstrProperties 1 1 True False)
              , Instruction
                  4
                  [ InstrPattern
                      0
                      br_fallthrough_pattern_os
                      True
                      (ASSTemplate [])
                  ]
                  (InstrProperties 1 1 True False)
              , Instruction
                  5
                  [ InstrPattern
                      0
                      ret_pattern_os
                      True
                      ( ASSTemplate
                        [ ASVerbatim "ret "
                        , ASLocationOfValueNode 0
                        ]
                      )
                  ]
                  (InstrProperties 1 1 True False)
              ]
  in TargetMachine
       (toTargetMachineID "test")
       (fixInstrIDs $ insts ++ mkGenericPhiInstructions)
       [ Location { locID = 0
                  , locName = LocationName "r0"
                  , locValue = Nothing
                  }
       , Location { locID = 1
                  , locName = LocationName "r1"
                  , locValue = Nothing
                  }
       , Location { locID = 2
                  , locName = LocationName "r2"
                  , locValue = Nothing
                  }
       ]
