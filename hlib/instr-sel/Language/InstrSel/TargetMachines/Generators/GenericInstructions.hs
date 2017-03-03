{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Generators.GenericInstructions
  ( mkBrFallThroughInstruction
  , mkPhiInstruction
  , mkDataDefInstruction
  , mkTempNullCopyInstruction
  , mkInactiveInstruction
  , reassignInstrIDs
  )
where

import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.DataTypes
  ( DataType (..) )
import Language.InstrSel.Functions
  ( mkEmptyBlockName )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.OpTypes
  ( ControlOp (Br) )
import Language.InstrSel.TargetMachines.Base

import Data.List
  ( intersperse )



-------------
-- Functions
-------------

-- | Creates a value node without specified origin.
mkValueNode :: DataType -> NodeType
mkValueNode dt = ValueNode { typeOfValue = dt, originOfValue = Nothing }

-- | Creates a value node type with no value or origin.
mkGenericValueNodeType :: NodeType
mkGenericValueNodeType = ValueNode { typeOfValue = AnyType
                                   , originOfValue = Nothing
                                   }

-- | Creates a generic block node type.
mkGenericBlockNodeType :: NodeType
mkGenericBlockNodeType = BlockNode mkEmptyBlockName

-- | Creates an instruction for handling the generic cases where
-- 'PhiNode's appear. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkPhiInstruction :: Instruction
mkPhiInstruction =
  let mkPat n =
        let g = mkGraph ( map Node $
                          ( [ ( 0, NodeLabel 0 PhiNode )
                            , ( 1, NodeLabel 1 mkGenericValueNodeType )
                            ]
                            ++
                            map ( \n' ->
                                  ( fromIntegral n'
                                  , NodeLabel (toNodeID n')
                                              mkGenericValueNodeType
                                  )
                                )
                                [2..n+1]
                            ++
                            map ( \n' ->
                                  ( fromIntegral n'
                                  , NodeLabel (toNodeID n')
                                              mkGenericBlockNodeType
                                  )
                                )
                                [n+2..2*n+2]
                          )
                        )
                        ( map Edge $
                          ( [ ( 0, 1, EdgeLabel DataFlowEdge 0 0 ) ]
                            ++
                            map ( \n' ->
                                  ( fromIntegral n'
                                  , 0
                                  , EdgeLabel DataFlowEdge 0 ((toEdgeNr n')-2)
                                  )
                                )
                                [2..n+1]
                            ++
                            [ ( fromIntegral n + 2, 1, EdgeLabel DefEdge 0 0 ) ]
                            ++
                            map ( \n' ->
                                    let int = fromIntegral n'
                                    in ( int
                                       , int + fromIntegral n + 1
                                       , EdgeLabel DefEdge 0 0
                                       )
                                )
                                [2..n+1]
                          )
                        )
                        Nothing
            emit_str = EmitStringTemplate $
                       [ [ ESVerbatim "PHI "
                         ]
                         ++
                         ( concat $
                           intersperse [ESVerbatim " "] $
                           map ( \nid -> [ ESVerbatim "("
                                         , ESLocationOfValueNode nid
                                         , ESVerbatim ", "
                                         , ESBlockOfValueNode nid
                                         , ESVerbatim ")"
                                         ]
                               ) $
                           [2..n+1]
                         )
                       ]
        in InstrPattern
             { patID = (toPatternID $ n-2)
             , patOS = OpStructure g [] [] []
             , patInputData = [2..n+1]
             , patOutputData = [1]
             , patEmitString = emit_str
             }
  in Instruction { instrID = 0
                 , instrPatterns = map mkPat [2..10]
                 , instrProps = InstrProperties { instrCodeSize = 0
                                                , instrLatency = 0
                                                , instrIsCopy = False
                                                , instrIsInactive = False
                                                , instrIsNull = False
                                                , instrIsPhi = True
                                                , instrIsSimd = False
                                                }
                 }

-- | Creates an instruction for handling unconditional branching to the
-- immediately following block (that is, fallthroughs). Note that the
-- 'InstructionID's of all instructions will be (incorrectly) set to 0, meaning
-- they must be reassigned afterwards.
mkBrFallThroughInstruction :: Instruction
mkBrFallThroughInstruction =
  let entry = ( 1, NodeLabel 1 mkGenericBlockNodeType )
      g = mkGraph ( map Node $
                    [ ( 0, NodeLabel 0 (ControlNode Br) )
                    , entry
                    , ( 2, NodeLabel 2 mkGenericBlockNodeType )
                    ]
                  )
                  ( map Edge $
                    [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                    , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                    ]
                  )
                  (Just $ Node entry)
      cs = mkFallThroughConstraints 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OpStructure g [] [] cs
          , patInputData = []
          , patOutputData = []
          , patEmitString = EmitStringTemplate []
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = False
                                      , instrIsInactive = False
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Creates an instruction for handling definition of data that represent
-- constants and function arguments.
mkDataDefInstruction :: Instruction
mkDataDefInstruction =
  let entry = ( 0, NodeLabel 0 mkGenericBlockNodeType )
      mkPatternGraph datum flow_type =
        mkGraph ( map Node $
                  [ entry
                  , ( 1, NodeLabel 1 datum )
                  ]
                )
                ( map Edge $
                  [ ( 0, 1, EdgeLabel flow_type 0 0 ) ]
                )
                (Just $ Node entry)
      g1 = mkPatternGraph mkGenericValueNodeType DataFlowEdge
      g2 = mkPatternGraph StateNode StateFlowEdge
      mkInstrPattern pid g cs =
        InstrPattern
          { patID = pid
          , patOS = OpStructure g [] [] cs
          , patInputData = []
          , patOutputData = [1]
          , patEmitString = EmitStringTemplate []
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [ mkInstrPattern 0 g1 []
                         , mkInstrPattern 1 g2 []
                         ]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = False
                                      , instrIsInactive = False
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Creates an instruction for handling null-copy operations regarding
-- temporaries. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkTempNullCopyInstruction :: Instruction
mkTempNullCopyInstruction =
  let g = mkGraph ( map Node $
                    [ ( 0, NodeLabel 0 CopyNode )
                    , ( 1, NodeLabel 1 $ mkValueNode IntTempTypeAnyWidth)
                    , ( 2, NodeLabel 2 $ mkValueNode IntTempTypeAnyWidth)
                    ]
                  )
                  ( map Edge $
                    [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                    , ( 0, 2, EdgeLabel DataFlowEdge 0 0 )
                    ]
                  )
                  Nothing
      pat = InstrPattern
             { patID = 0
             , patOS = OpStructure g [] [(1, 2)] []
             , patInputData = [1]
             , patOutputData = [2]
             , patEmitString = EmitStringTemplate []
             }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = True
                                      , instrIsInactive = False
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Creates an instruction to be selected for operations that are
-- inactive. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkInactiveInstruction :: Instruction
mkInactiveInstruction =
  let g = mkGraph ( map Node $
                    [ ( 0, NodeLabel 0 CopyNode )
                    , ( 1, NodeLabel 1 mkGenericValueNodeType )
                    , ( 2, NodeLabel 2 mkGenericValueNodeType )
                    ]
                  )
                  ( map Edge $
                    [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                    , ( 0, 2, EdgeLabel DataFlowEdge 0 0 )
                    ]
                  )
                  Nothing
      pat = InstrPattern
              { patID = 0
              , patOS = OpStructure g [] [] []
              , patInputData = [1]
              , patOutputData = [2]
              , patEmitString = EmitStringTemplate []
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = True
                                      , instrIsInactive = True
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Reassigns the 'InstructionID's of the given instructions, starting from a
-- given 'InstructionID' and then incrementing it for each instruction.
reassignInstrIDs
  :: InstructionID
     -- ^ The ID from which to start the assignment.
  -> [Instruction]
  -> [Instruction]
reassignInstrIDs next_id insts =
  map (\(new_iid, inst) -> inst { instrID = toInstructionID new_iid }) $
  zip [(fromInstructionID next_id)..] insts
