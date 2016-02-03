--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstrSel.TargetMachines.Generators.GenericInstructions
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating generic instructions that must appear in all target
-- machines.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Generators.GenericInstructions
  ( mkBrFallThroughInstruction
  , mkPhiInstructions
  , mkDataDefInstruction
  , mkTempNullCopyInstruction
  , reassignInstrIDs
  )
where

import Language.InstrSel.Constraints
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
import Language.InstrSel.Utils
  ( Natural )



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

-- | Creates an 'IntTempType' with a given number of bits.
mkIntTempType :: Natural -> DataType
mkIntTempType n = IntTempType { intNumBits = n }

-- | Creates a set of instructions for handling the generic cases where
-- 'PhiNode's appear. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkPhiInstructions
  :: (    [NodeID]
       -> NodeID
       -> AssemblyStringTemplate
     )
     -- ^ Function for creating the assembly string for the generic phi
     -- instructions. The first argument is a list of 'NodeID's for the value
     -- nodes which serve as input to the phi operation, and the second argument
     -- is the 'NodeID's of the value node representing the output from the phi
     -- operation.
  -> [Instruction]
mkPhiInstructions mkAss =
  let mkPat n =
        let g = mkGraph
                  ( map
                      Node
                      ( [ ( 0, NodeLabel 0 PhiNode )
                        , ( 1, NodeLabel 1 mkGenericValueNodeType )
                        ]
                        ++
                        map ( \n' ->
                              ( fromIntegral n'
                              , NodeLabel (toNodeID n') mkGenericValueNodeType
                              )
                            )
                            [2..n+1]
                      )
                  )
                  ( map
                      Edge
                      ( [ ( 0, 1, EdgeLabel DataFlowEdge 0 0 ) ]
                        ++
                        map ( \n' ->
                              ( fromIntegral n'
                              , 0
                              , EdgeLabel DataFlowEdge 0 ((toEdgeNr n')-2)
                              )
                            )
                            [2..n+1]
                      )
                  )
            cs = mkSameDataLocConstraints [1..n+1]
        in InstrPattern
             { patID = (toPatternID $ n-2)
             , patOS = OpStructure g Nothing cs
             , patADDUC = False
             , patAsmStrTemplate = mkAss (map toNodeID [2..n+1]) 1
             }
  in [ Instruction
         { instrID = 0
         , instrPatterns = map mkPat [2..10]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Creates an instruction for handling unconditional branching to the
-- immediately following block (that is, fallthroughs). Note that the
-- 'InstructionID's of all instructions will be (incorrectly) set to 0, meaning
-- they must be reassigned afterwards.
mkBrFallThroughInstruction :: Instruction
mkBrFallThroughInstruction =
  let g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode Br) )
                , ( 1, NodeLabel 1 mkGenericBlockNodeType )
                , ( 2, NodeLabel 2 mkGenericBlockNodeType )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                ]
            )
      bb_alloc_cs = mkMatchPlacementConstraints g
      fallthrough_cs = mkFallThroughConstraints 2
      cs = bb_alloc_cs ++ fallthrough_cs
      pat =
        InstrPattern
          { patID = 0
          , patOS = OpStructure g (Just 1) cs
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates an instruction for handling definition of data that represent
-- constants and function arguments.
mkDataDefInstruction :: Instruction
mkDataDefInstruction =
  let mkPatternGraph datum =
        mkGraph ( map Node
                      [ ( 0, NodeLabel 0 mkGenericBlockNodeType )
                      , ( 1, NodeLabel 1 datum )
                      ]
                )
                ( map Edge
                      [ ( 0, 1, EdgeLabel DataFlowEdge 0 0 ) ]
                )
      g1 = mkPatternGraph mkGenericValueNodeType
      g2 = mkPatternGraph StateNode
      cs1 = mkMatchPlacementConstraints g1
      cs2 = mkMatchPlacementConstraints g2
      mkInstrPattern pid g cs =
        InstrPattern
          { patID = pid
          , patOS = OpStructure g (Just 0) cs
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [ mkInstrPattern 0 g1 cs1
                         , mkInstrPattern 1 g2 cs2
                         ]
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates an instruction for handling null-copy operations regarding
-- temporaries. Note that the 'InstructionID's of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkTempNullCopyInstruction
  :: [Natural]
     -- ^ List of temporary bit widths for which null-copies are allowed.
  -> Instruction
mkTempNullCopyInstruction bits =
  let g w = mkGraph
              ( map
                  Node
                  [ ( 0, NodeLabel 0 CopyNode )
                  , ( 1, NodeLabel 1 $ mkValueNode $ mkIntTempType w)
                  , ( 2, NodeLabel 2 $ mkValueNode $ mkIntTempType w)
                  ]
              )
              ( map
                  Edge
                  [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                  , ( 0, 2, EdgeLabel DataFlowEdge 0 0 )
                  ]
              )
      cs = [ BoolExprConstraint $
               EqExpr
                 ( Location2NumExpr $
                     LocationOfValueNodeExpr $
                       ANodeIDExpr 1
                 )
                 ( Location2NumExpr $
                     LocationOfValueNodeExpr $
                       ANodeIDExpr 2
                 )
           ]
      pat (pid, w) = InstrPattern
                       { patID = pid
                       , patOS = OpStructure (g w) Nothing cs
                       , patADDUC = True
                       , patAsmStrTemplate = ASSTemplate []
                       }
  in Instruction
       { instrID = 0
       , instrPatterns = map pat $ zip [0..] bits
       , instrProps = InstrProperties { instrCodeSize = 0
                                      , instrLatency = 0
                                      , instrIsNonCopy = False
                                      , instrIsNullCopy = True
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
  map (\(new_iid, inst) -> inst { instrID = toInstructionID new_iid })
      (zip [(fromInstructionID next_id)..] insts)
