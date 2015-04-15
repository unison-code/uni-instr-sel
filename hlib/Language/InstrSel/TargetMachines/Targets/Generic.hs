--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstrSel.TargetMachines.Targets.Generic
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2015
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating generic instructions, and other useful help functions.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Targets.Generic
  ( fixInstrIDs
  , fixLocIDs
  , mkValueNode
  , mkIntTempType
  , mkIntConstType
  , mkGenericBrFallthroughInstructions
  , mkGenericPhiInstructions
  , mkGenericEntityDefInstructions
  , mkGenericCopyInstructions
  )
where

import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.DataTypes
  ( DataType (..) )
import Language.InstrSel.Functions
  ( mkEmptyBBLabel )
import Language.InstrSel.Graphs
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.Utils
  ( Natural
  , Range (..)
  )



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

-- | Creates a generic label node type.
mkGenericBlockNodeType :: NodeType
mkGenericBlockNodeType = BlockNode mkEmptyBBLabel

-- | Creates a set of instructions for handling the generic cases where
-- 'PhiNode's appear. The instruction IDs of all instructions will be
-- (incorrectly) set to 0, meaning they must be reassigned afterwards.
mkGenericPhiInstructions :: [Instruction]
mkGenericPhiInstructions =
  let g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 PhiNode )
                , ( 1, NodeLabel 1 mkGenericValueNodeType )
                , ( 2, NodeLabel 2 mkGenericValueNodeType )
                , ( 3, NodeLabel 3 mkGenericValueNodeType )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      cs = [ BoolExprConstraint $
               AndExpr
                 ( EqExpr
                     ( Location2NumExpr $
                         LocationOfValueNodeExpr $
                           ANodeIDExpr 1
                     )
                     ( Location2NumExpr $
                         LocationOfValueNodeExpr $
                           ANodeIDExpr 2
                     )
                 )
                 ( EqExpr
                     ( Location2NumExpr $
                         LocationOfValueNodeExpr $
                           ANodeIDExpr 2
                     )
                     ( Location2NumExpr $
                         LocationOfValueNodeExpr $
                           ANodeIDExpr 3
                     )
                 )
           ]
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = False
              , patAsmStrTemplate = ( ASSTemplate
                                        [ ASLocationOfDataNode 3
                                        , ASVerbatim " = PHI "
                                        , ASLocationOfDataNode 1
                                        , ASVerbatim ", "
                                        , ASBlockOfDataNode 1
                                        , ASVerbatim ", "
                                        , ASLocationOfDataNode 2
                                        , ASVerbatim ", "
                                        , ASBlockOfDataNode 2
                                        ]
                                    )
              }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
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
      bb_alloc_cs = mkMatchPlacementConstraints g
      fallthrough_cs = mkFallThroughConstraints 2
      cs = bb_alloc_cs ++ fallthrough_cs
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 1) cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
                                        }
         }
     ]

-- | Creates a set of instructions for handling definition of entities that
-- represent constants and function arguments.
mkGenericEntityDefInstructions :: [Instruction]
mkGenericEntityDefInstructions =
  let g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 mkGenericBlockNodeType )
                , ( 1, NodeLabel 1 mkGenericValueNodeType )
                ]
            )
            ( map
                Edge
                [ ( 0, 1, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      cs = mkMatchPlacementConstraints g
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 0) cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
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
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat 1, pat 8, pat 16, pat 32]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = False
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
