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
  , mkPhiInstructions
  , mkDataDefInstructions
  , mkTempNullCopyInstruction
  , mkKillInstruction
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

-- | Creates instructions for handling the generic cases where 'PhiNode's
-- appear. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkPhiInstructions :: [Instruction]
mkPhiInstructions =
  let mkInstr n =
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
        in Instruction
             { instrID = 0
             , instrOS = OpStructure g [] [] []
             , instrInputData = [2..n+1]
             , instrOutputData = [1]
             , instrEmitString = emit_str
             , instrProps = InstrProperties { instrCodeSize = 0
                                            , instrLatency = 0
                                            , instrIsCopy = False
                                            , instrIsKill = False
                                            , instrIsNull = False
                                            , instrIsPhi = True
                                            , instrIsSimd = False
                                            }
             }
  in map mkInstr [2..10]

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
  in Instruction
       { instrID = 0
       , instrOS = OpStructure g [] [] cs
       , instrInputData = []
       , instrOutputData = []
       , instrEmitString = EmitStringTemplate []
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = False
                                      , instrIsKill = False
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Creates instructions for handling definition of data that represent
-- constants and function arguments. Note that the 'InstructionID's of all
-- instructions will be (incorrectly) set to 0, meaning they must be reassigned
-- afterwards.
mkDataDefInstructions :: [Instruction]
mkDataDefInstructions =
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
      mkInstr g cs =
        Instruction
          { instrID = 0
          , instrOS = OpStructure g [] [] cs
          , instrInputData = []
          , instrOutputData = [1]
          , instrEmitString = EmitStringTemplate []
          , instrProps = InstrProperties { instrCodeSize = 0
                                         , instrLatency = 0
                                         , instrIsCopy = False
                                         , instrIsKill = False
                                         , instrIsNull = True
                                         , instrIsPhi = False
                                         , instrIsSimd = False
                                         }
          }
  in [ mkInstr g1 []
     , mkInstr g2 []
     ]

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
  in Instruction
       { instrID = 0
       , instrOS = OpStructure g [] [(1, 2)] []
       , instrInputData = [1]
       , instrOutputData = [2]
       , instrEmitString = EmitStringTemplate []
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = True
                                      , instrIsKill = False
                                      , instrIsNull = True
                                      , instrIsPhi = False
                                      , instrIsSimd = False
                                      }
       }

-- | Creates an instruction to be selected to define data that are
-- inactive. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkKillInstruction :: Instruction
mkKillInstruction =
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
  in Instruction
       { instrID = 0
       , instrOS = OpStructure g [] [] []
       , instrInputData = [1]
       , instrOutputData = [2]
       , instrEmitString = EmitStringTemplate []
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsCopy = True
                                      , instrIsKill = True
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
