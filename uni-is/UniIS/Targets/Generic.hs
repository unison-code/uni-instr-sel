{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

Contributing authors:
  Roberto Castaneda Lozano <rcas@sics.se>

-}

module UniIS.Targets.Generic
  ( fixInstrIDs
  , fixLocIDs
  , mkValueNode
  , mkIntTempType
  , mkIntConstType
  , mkGenericBrFallthroughInstructions
  , mkGenericPhiInstructions
  , mkGenericDataDefInstructions
  , mkGenericCopyInstructions
  )
where

import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.DataTypes
  ( DataType (..) )
import Language.InstrSel.Functions
  ( mkEmptyBlockName )
import Language.InstrSel.Graphs
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.Utils
  ( Natural
  , Range (..)
  )

import Data.List
  ( intersperse )



-------------
-- Functions
-------------

-- | Creates a value node without specified origin.
mkValueNode :: DataType -> NodeType
mkValueNode dt = ValueNode { typeOfValue = dt, originOfValue = Nothing }

-- | Creates a generic value node type.
mkGenericValueNodeType :: NodeType
mkGenericValueNodeType = ValueNode { typeOfValue = AnyType
                                   , originOfValue = Nothing
                                   }

-- | Creates an 'IntTempType' with a given number of bits.
mkIntTempType :: Natural -> DataType
mkIntTempType n = IntTempType { intTempNumBits = n  }

-- | Creates an 'IntConstType' with a given range and number of bits.
mkIntConstType :: Range Integer -> Natural -> DataType
mkIntConstType r n = IntConstType { intConstValue = r
                                  , intConstNumBits = Just n
                                  }

-- | Creates a generic block node type.
mkGenericBlockNodeType :: NodeType
mkGenericBlockNodeType = BlockNode mkEmptyBlockName

-- | Creates a set of instructions for handling the generic cases where
-- 'PhiNode's appear. The instruction IDs of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkGenericPhiInstructions :: [Instruction]
mkGenericPhiInstructions =
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
             , patOS = OS.OpStructure g Nothing cs
             , patADDUC = False
             , patEmitString =
                 ( EmitStringTemplate
                   $ [ [ ESLocationOfValueNode 1
                       , ESVerbatim " = PHI "
                       ]
                       ++
                       ( concat
                         $ intersperse
                             [ESVerbatim " "]
                             ( map ( \n' ->
                                     [ ESVerbatim "("
                                     , ESLocationOfValueNode (toNodeID n')
                                     , ESVerbatim ", "
                                     , ESBlockOfValueNode (toNodeID n')
                                     , ESVerbatim ")"
                                     ]
                                   )
                                   [2..n+1]
                             )
                       )
                     ]
                 )
             }
  in [ Instruction
         { instrID = 0
         , instrPatterns = map mkPat [2..10]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        }
         }
     ]

-- | Creates a set of instructions for handling unconditional branching to the
-- immediately following block (that is, fallthroughs). The instruction IDs of
-- all instructions will be (incorrectly) set to 0, meaning they must be
-- reassigned afterwards.
mkGenericBrFallThroughInstructions :: [Instruction]
mkGenericBrFallThroughInstructions =
  let g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Br) )
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
      cs = mkFallThroughConstraints 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 1) cs
          , patADDUC = True
          , patEmitString = EmitStringTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        }
         }
     ]

-- | Creates a set of instructions for handling definition of data that
-- represent constants and function arguments.
mkGenericDataDefInstructions :: [Instruction]
mkGenericDataDefInstructions =
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
      mkInstrPattern pid g cs =
        InstrPattern
          { patID = pid
          , patOS = OS.OpStructure g (Just 0) cs
          , patADDUC = True
          , patEmitString = EmitStringTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [ mkInstrPattern 0 g1 []
                           , mkInstrPattern 1 g2 []
                           ]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        }
         }
     ]

-- | Creates a set of instructions for handling null-copy operations.
mkGenericCopyInstructions :: [Instruction]
mkGenericCopyInstructions =
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
      pat w =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g w) Nothing cs
          , patADDUC = True
          , patEmitString = EmitStringTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat 1, pat 8, pat 16, pat 32]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        }
         }
     ]

-- | In order to not have to concern ourselves with instruction IDs being
-- unique, we let this function fix those for us afterwards. The function goes
-- over the list of instructions and reassigns the instruction IDs such that
-- each instruction gets a unique ID.
fixInstrIDs :: [Instruction] -> [Instruction]
fixInstrIDs insts =
  map ( \(new_iid, inst) -> inst { instrID = new_iid } ) (zip [0..] insts)

-- | Same as 'fixInstrIDs' but for location IDs.
fixLocIDs :: [Location] -> [Location]
fixLocIDs regs =
  map ( \(new_rid, r) -> r { locID = new_rid } ) (zip [0..] regs)
