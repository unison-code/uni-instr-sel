--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Targets.Mips32
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- The ISA of 32-bit MIPS.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstrSel.TargetMachines.Targets.Mips32
  ( theTM
  , tmFancyMips32
  )
where

import Language.InstrSel.Constraints
  ( NodeExpr (..) )
import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.Constraints.ConstraintReconstructor
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Functions
  ( mkEmptyBlockName )
import Language.InstrSel.Graphs
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.Targets.Generic
import Language.InstrSel.Utils
  ( Natural
  , Range (..)
  )

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

regPrefix :: String
regPrefix = "$"

updateEmitStrTemplate :: EmitStringTemplate -> Instruction -> Instruction
updateEmitStrTemplate t i @ Instruction { instrPatterns = pats } =
    i {instrPatterns = map (\pat -> pat { patEmitString = t }) pats }

updateLatency :: Integer -> Instruction -> Instruction
updateLatency l i =
    let p = instrProps i
    in i {instrProps = p { instrLatency = l } }

-- | The name of the zero register.
getZeroRegName :: String
getZeroRegName = "%ZERO"

-- | Creates the zero register. Note that there are no guarantees that the
-- location IDs will be correctly set!
mkZeroRegister :: Location
mkZeroRegister = Location { locID = 0
                          , locName = LocationName getZeroRegName
                          , locValue = Just 0
                          }

-- | Creates the list of general-purpose registers, excluding the zero register,
-- but there are no guarantees that the location IDs will be correctly set!
mkGPRegistersWithoutZero :: [Location]
mkGPRegistersWithoutZero =
  map ( \i -> Location { locID = 0
                       , locName = LocationName $ regPrefix ++ show i
                       , locValue = Nothing
                       }
      )
      ([1..31] :: [Integer]) -- Cast needed to prevent compilation warning

-- | Creates the list of 'HI' register (which is actually a memory
-- location). Note that there are no guarantees that the location IDs will be
-- correctly set!
mkHIRegister :: Location
mkHIRegister = Location { locID = 0
                        , locName = LocationName "HI"
                        , locValue = Nothing
                        }

-- | Creates the list of 'LO' register (which is actually a memory
-- location). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkLORegister :: Location
mkLORegister = Location { locID = 0
                        , locName = LocationName "LO"
                        , locValue = Nothing
                        }

-- | Retrieves the register with a given location name. It is assumed that there
-- will exist exactly one such register.
getRegisterByName :: LocationName -> Location
getRegisterByName rname =
  let regs = getAllLocations
      found = filter (\r -> rname == locName r) regs
  in head found

-- | Retrieves all locations, where the location IDs have been set such that
-- every location is given a unique ID.
getAllLocations :: [Location]
getAllLocations = fixLocIDs $ [mkZeroRegister]
                              ++
                              mkGPRegistersWithoutZero
                              ++
                              [ mkHIRegister
                              , mkLORegister
                              ]

-- | Retrieves all general-purpose registers, includeing the zero register,
-- where the location IDs have been set such that every location is given a
-- unique ID.
getGPRegistersInclZero :: [Location]
getGPRegistersInclZero = getGPRegistersWithoutZero ++ [getZeroRegister]

-- | Same as 'getGPRegistersInclZero' but without the zero register.
getGPRegistersWithoutZero :: [Location]
getGPRegistersWithoutZero = map getRegisterByName
                                (map locName mkGPRegistersWithoutZero)

-- | Retrieves the zero register, which always contains the value 0.  The
-- location ID will be correctly set.
getZeroRegister :: Location
getZeroRegister = getRegisterByName $ LocationName getZeroRegName

-- | Retrieves the general-purpose register that serves as the return register.
-- The location ID will be correctly set.
getRetRegister :: Location
getRetRegister = getRegisterByName $ LocationName $ regPrefix ++ "31"

-- | Retrieves the 'HI' register. The location ID will be correctly set.
getHIRegister :: Location
getHIRegister = getRegisterByName $ locName mkHIRegister

-- | Retrieves the 'LO' register. The location ID will be correctly set.
getLORegister :: Location
getLORegister = getRegisterByName $ locName mkLORegister

-- | Creates a simple pattern that consists of a single computation node, which
-- takes two value nodes as input, and produces another value node as output.
mkSimpleCompPattern
  :: O.CompOp
     -- ^ The computation operation.
  -> Bool
     -- ^ Whether to swap the order of the input operands. This does not affect
     -- the node IDs.
  -> D.DataType
     -- ^ The data type of the first operand.
  -> D.DataType
     -- ^ The data type of the second operand.
  -> D.DataType
     -- ^ The data type of the result.
  -> Graph
mkSimpleCompPattern op swap_ops src1 src2 dst =
  let mkCompNode = ComputationNode { compOp = op }
  in mkGraph
       ( map Node
             [ ( 0, NodeLabel 0 mkCompNode )
             , ( 1, NodeLabel 1 (mkValueNode src1) )
             , ( 2, NodeLabel 2 (mkValueNode src2) )
             , ( 3, NodeLabel 3 (mkValueNode  dst) )
             ]
       )
       ( map Edge
             ( [ ( 0, 3, EdgeLabel DataFlowEdge 0 0 ) ]
               ++
               if not swap_ops
               then [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                    , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                    ]
               else [ ( 1, 0, EdgeLabel DataFlowEdge 0 1 )
                    , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                    ]
             )
       )

-- | Creates a simple pattern that consists of a single copy node, which takes a
-- value node as input, and produces another value node as output.
mkSimpleCopyPattern
  :: D.DataType
     -- ^ The data type of the operand.
  -> D.DataType
     -- ^ The data type of the result.
  -> Graph
mkSimpleCopyPattern src dst =
  mkGraph
       ( map Node
             [ ( 0, NodeLabel 0 CopyNode )
             , ( 1, NodeLabel 1 (mkValueNode src) )
             , ( 2, NodeLabel 2 (mkValueNode dst) )
             ]
       )
       ( map Edge
             [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
             , ( 0, 2, EdgeLabel DataFlowEdge 0 0 )
             ]
       )

-- | Creates an instance of the simple copy pattern. All data are assumed to be
-- 32 bits in size.
mkSimpleCopy32Pattern :: Graph
mkSimpleCopy32Pattern =
  mkSimpleCopyPattern (mkIntTempType 32) (mkIntTempType 32)

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.
mkGenericSimpleRegRegCompInst
  :: String
     -- ^ The emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> D.DataType
     -- ^ The data type of the first operand.
  -> D.DataType
     -- ^ The data type of the second operand.
  -> D.DataType
     -- ^ The data type of the result.
  -> [Location]
     -- ^ The location class of the first operand.
  -> [Location]
     -- ^ The location class of the second operand.
  -> [Location]
     -- ^ The location class of the destination.
  -> Instruction
mkGenericSimpleRegRegCompInst str op d1 d2 d3 r1 r2 r3 =
  let g = mkSimpleCompPattern op False d1 d2 d3
      cs = concatMap ( \(r, nid) ->
                       mkNewDataLocConstraints (map locID r) nid
                     )
                     (zip [r1, r2, r3] [1, 2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patADDUC = True
              , patEmitString = ESTSimple
                                      [ ESLocationOfValueNode 3
                                      , ESVerbatim $ " = "
                                      , ESVerbatim $ str ++ " "
                                      , ESLocationOfValueNode 1
                                      , ESVerbatim ", "
                                      , ESLocationOfValueNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  All values are assumed to be N bits in size.
mkSimpleNBitRegRegCompInst
  :: Natural
     -- ^ The width of the data.
  -> String
     -- ^ The emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Location]
     -- ^ The location class of the first operand.
  -> [Location]
     -- ^ The location class of the second operand.
  -> [Location]
     -- ^ The location class of the destination.
  -> Instruction
mkSimpleNBitRegRegCompInst n str op r1 r2 r3 =
  let dt = mkIntTempType n
  in mkGenericSimpleRegRegCompInst str op dt dt dt r1 r2 r3

mkSimple32BitRegRegCompInst :: String -> O.CompOp -> [Location] -> [Location] ->
                               [Location] -> Instruction
mkSimple32BitRegRegCompInst = mkSimpleNBitRegRegCompInst 32

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  The input operands are assumed to be 32 bits in size, and the result
-- is assumed to be 1 bit in size.
mkSimple32BitRegs1BitResultCompInst
  :: String
     -- ^ The emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Location]
     -- ^ The location class of the first operand.
  -> [Location]
     -- ^ The location class of the second operand.
  -> [Location]
     -- ^ The location class of the destination.
  -> Instruction
mkSimple32BitRegs1BitResultCompInst str op r1 r2 r3 =
  let dt32 = mkIntTempType 32
      dt1 = mkIntTempType 1
  in mkGenericSimpleRegRegCompInst str op dt32 dt32 dt1 r1 r2 r3

mkDataImmDataEmitStr :: String -> [EmitStringPart]
mkDataImmDataEmitStr str =
    [ ESLocationOfValueNode 3
    , ESVerbatim $ " = "
    , ESVerbatim $ str ++ " "
    , ESLocationOfValueNode 1
    , ESVerbatim ", "
    , ESIntConstOfValueNode 2
    ]

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output. The first input operand and result are assumed to reside in one of
-- the 32 general-purpose registers, and the second input operand is assumed to
-- be a N-bit immediate of a given range. If the computation is a commutative
-- operation, then two instruction patterns will be created.
mkSimpleNBitRegMBitImmCompInst
  :: [EmitStringPart]
     -- ^ The emit string parts of the instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Location]
     -- ^ The location class of the first operand.
  -> [Location]
     -- ^ The location class of the destination.
  -> Range Integer
     -- ^ The range of the immediate (which is the second operand).
  -> Natural
     -- ^ The number of bits of the first operand and the destination.
  -> Natural
     -- ^ The number of bits of the immediate.
  -> Instruction
mkSimpleNBitRegMBitImmCompInst str op r1 r3 imm n m =
  let dtN = mkIntTempType n
      dtM = mkIntConstType imm m
      ord_g = mkSimpleCompPattern op False dtN dtM dtN
      swapped_g = mkSimpleCompPattern op True dtN dtM dtN
      cs = concatMap ( \(r, nid) ->
                       mkNewDataLocConstraints (map locID r) nid
                     )
                     (zip [r1, r3] [1, 3])
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure ord_g Nothing cs
                 , patADDUC = True
                 , patEmitString = ESTSimple parts
                 }
             ]
             ++
             if O.isOpCommutative op
             then [ InstrPattern
                      { patID = 1
                      , patOS = OS.OpStructure swapped_g Nothing cs
                      , patADDUC = True
                      , patEmitString = ESTSimple parts
                      }
                  ]
             else []
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  The second input operand and result are assumed to reside in one of
-- the 32 general-purpose registers, and the first input operand is assumed to
-- be a N-bit immediate of a given range.
mkSimpleNBitRegMBitFirstImmCompInst
  :: [EmitStringPart]
     -- ^ The emit string parts of the instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Location]
     -- ^ The location class of the second operand.
  -> [Location]
     -- ^ The location class of the destination.
  -> Range Integer
     -- ^ The range of the immediate (which is the first operand).
  -> Natural
     -- ^ The number of bits of the second operand and the destination.
  -> Natural
     -- ^ The number of bits of the immediate.
  -> Instruction
mkSimpleNBitRegMBitFirstImmCompInst str op r2 r3 imm n m =
  let dtN = mkIntTempType n
      dtM = mkIntConstType imm m
      g = mkSimpleCompPattern op False dtM dtN dtN
      cs = concatMap ( \(r, nid) ->
                       mkNewDataLocConstraints (map locID r) nid
                     )
                     (zip [r2, r3] [2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patADDUC = True
              , patEmitString = ESTSimple parts
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Merges two nodes in a given instruction pattern, and updates the referred
-- node IDs accordingly.
combineNodesInInstrPattern
  :: InstrPattern
  -> NodeID
     -- ^ Node to merge with (will be kept).
  -> NodeID
     -- ^ Node to merge with (will be discarded).
  -> InstrPattern
combineNodesInInstrPattern ip keep_n disc_n =
  let checkAndReplace nid = if nid == disc_n then keep_n else nid
      -- Old data
      old_os = patOS ip
      old_g = OS.osGraph old_os
      old_entry = OS.osEntryBlockNode old_os
      old_cs = OS.osConstraints old_os
      old_template = patEmitString ip
      -- New data
      new_g = mergeNodes (head $ findNodesWithNodeID old_g keep_n)
                         (head $ findNodesWithNodeID old_g disc_n)
                         old_g
      replaceNodeInCs cs =
        let def_r = mkDefaultReconstructor
            mkNodeExpr _ (ANodeIDExpr nid) = ANodeIDExpr (checkAndReplace nid)
            mkNodeExpr r expr = (mkNodeExprF def_r) r expr
            new_r = def_r { mkNodeExprF = mkNodeExpr }
        in map (apply new_r) cs
      new_cs = replaceNodeInCs old_cs
      new_entry = if isJust old_entry
                  then Just (checkAndReplace $ fromJust old_entry)
                  else Nothing
      new_os = OS.OpStructure { OS.osGraph = new_g
                              , OS.osEntryBlockNode = new_entry
                              , OS.osConstraints = new_cs
                              }
      new_template = updateNodeInEmitStrTemplate keep_n disc_n old_template
  in ip { patOS = new_os
        , patEmitString = new_template
        }

-- | Makes two conditional branch instructions - an ordinary branch instruction
-- and its inverse branch instruction - that takes two registers as input. The
-- inverse is achieved by inverting the comparison, and swapping the branch
-- blocks, which means both instructions carry the same semantics.
mkRegRegCondBrInstrs
  :: Natural
     -- ^ The width of the comparison operands.
  -> String
     -- ^ The emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The comparison corresponding to this instruction.
  -> String
     -- ^ The inverse emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The inverse comparison corresponding to this instruction.
  -> Instruction
mkRegRegCondBrInstrs n ord_str ord_op inv_str inv_op =
  let mkBlockNode = BlockNode mkEmptyBlockName
      mkPatternGraph op =
        mkGraph ( map Node
                      [ ( 0, NodeLabel 0 (ControlNode O.CondBr) )
                      , ( 1, NodeLabel 1 mkBlockNode )
                      , ( 2, NodeLabel 2 mkBlockNode )
                      , ( 3, NodeLabel 3 mkBlockNode )
                      , ( 4, NodeLabel 4 (ComputationNode { compOp = op }) )
                      , ( 5, NodeLabel 5 (ValueNode (mkIntTempType n) Nothing) )
                      , ( 6, NodeLabel 6 (ValueNode (mkIntTempType n) Nothing) )
                      , ( 7, NodeLabel 7 (ValueNode (mkIntTempType 1) Nothing) )
                      ]
                )
                ( map Edge
                      [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                      , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                      , ( 0, 3, EdgeLabel ControlFlowEdge 1 0 )
                      , ( 7, 0, EdgeLabel DataFlowEdge 0 0 )
                      , ( 5, 4, EdgeLabel DataFlowEdge 0 0 )
                      , ( 6, 4, EdgeLabel DataFlowEdge 0 1 )
                      , ( 4, 7, EdgeLabel DataFlowEdge 0 0 )
                      ]
                )
      ord_g = mkPatternGraph ord_op
      inv_g = mkPatternGraph inv_op
      ord_cs = mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 3
      inv_cs = mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 2
      ord_pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure ord_g (Just 1) ord_cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESVerbatim $ ord_str ++ " "
                                  , ESLocationOfValueNode 5
                                  , ESVerbatim ", "
                                  , ESLocationOfValueNode 6
                                  , ESVerbatim ", "
                                  , ESNameOfBlockNode 2
                                  ]
          }
      inv_pat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure inv_g (Just 1) inv_cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESVerbatim $ inv_str ++ " "
                                  , ESLocationOfValueNode 5
                                  , ESVerbatim ", "
                                  , ESLocationOfValueNode 6
                                  , ESVerbatim ", "
                                  , ESNameOfBlockNode 3
                                  ]
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [ ord_pat
                         , inv_pat
                         , (combineNodesInInstrPattern ord_pat 1 2)
                             { patID = 2 }
                         , (combineNodesInInstrPattern inv_pat 1 3)
                             { patID = 3 }
                         ]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Makes a conditional branch instruction that takes a register and an
-- immediate as input.
mkRegImmCondBrInstr
  :: Natural
     -- ^ The width of the first comparison operand.
  -> Range Integer
     -- ^ The range of the integer immediate.
  -> String
     -- ^ The emit string corresponding to this instruction.
  -> O.CompOp
     -- ^ The comparison corresponding to this instruction.
  -> Instruction
mkRegImmCondBrInstr n imm_r str op =
  let mkBlockNode = BlockNode mkEmptyBlockName
      mkPatternGraph swap_ops =
        mkGraph ( map Node
                      [ ( 0, NodeLabel 0 (ControlNode O.CondBr) )
                      , ( 1, NodeLabel 1 mkBlockNode )
                      , ( 2, NodeLabel 2 mkBlockNode )
                      , ( 3, NodeLabel 3 mkBlockNode )
                      , ( 4, NodeLabel 4 ( ComputationNode
                                             { compOp = if not swap_ops
                                                        then op
                                                        else O.swapComparison op
                                             }
                                         )
                        )
                      , ( 5, NodeLabel 5 ( ValueNode (mkIntTempType n)
                                                     Nothing
                                         )
                        )
                      , ( 6, NodeLabel 6 ( ValueNode (mkIntConstType imm_r n)
                                                     Nothing
                                         )
                        )
                      , ( 7, NodeLabel 7 (ValueNode (mkIntTempType 1) Nothing) )
                      ]
                )
                ( map Edge
                      ( [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                        , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                        , ( 0, 3, EdgeLabel ControlFlowEdge 1 0 )
                        , ( 7, 0, EdgeLabel DataFlowEdge 0 0 )
                        , ( 4, 7, EdgeLabel DataFlowEdge 0 0 )
                        ]
                        ++
                        if not swap_ops
                        then [ ( 5, 4, EdgeLabel DataFlowEdge 0 0 )
                             , ( 6, 4, EdgeLabel DataFlowEdge 0 1 )
                             ]
                        else [ ( 5, 4, EdgeLabel DataFlowEdge 0 1 )
                             , ( 6, 4, EdgeLabel DataFlowEdge 0 0 )
                             ]
                      )
                )
      ord_g = mkPatternGraph False
      swapped_g = mkPatternGraph True
      ord_cs = mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 3
      swapped_cs = mkNoDataReuseConstraints 7
                   ++
                   mkFallThroughConstraints 3
      mkInstrPattern pid g cs =
        InstrPattern
          { patID = pid
          , patOS = OS.OpStructure g (Just 1) cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESVerbatim $ str ++ " "
                                  , ESLocationOfValueNode 5
                                  , ESVerbatim ", "
                                  , ESNameOfBlockNode 2
                                  ]
          }
      ord_pat = mkInstrPattern 0 ord_g ord_cs
      swapped_pat = mkInstrPattern 1 swapped_g swapped_cs
      pats = if O.isOpCommutative op
             then [ ord_pat
                  , (combineNodesInInstrPattern ord_pat 1 2)
                      { patID = 1 }
                  ]
             else [ ord_pat
                  , swapped_pat
                  , (combineNodesInInstrPattern ord_pat 1 2)
                      { patID = 2 }
                  , (combineNodesInInstrPattern swapped_pat 1 2)
                      { patID = 3 }
                  ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Makes a predicated branch instruction (compares equal to $0).
mkPredBrInstr :: Instruction
mkPredBrInstr =
  let mkBlockNode = BlockNode $ BlockName ""
      g = mkGraph
         ( map
             Node
             [ ( 0, NodeLabel 0 (ControlNode O.CondBr) )
             , ( 1, NodeLabel 1 mkBlockNode )
             , ( 2, NodeLabel 2 mkBlockNode )
             , ( 3, NodeLabel 3 mkBlockNode )
             , ( 4, NodeLabel 4 (ValueNode (mkIntTempType 1) Nothing) )
             ]
         )
         ( map
             Edge
             [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
             , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
             , ( 0, 3, EdgeLabel ControlFlowEdge 1 0 )
             , ( 4, 0, EdgeLabel DataFlowEdge 0 0 )
             ]
         )
      ord_cs = mkFallThroughConstraints 3
      inv_cs = mkFallThroughConstraints 2
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure g (Just 1) ord_cs
                 , patADDUC = True
                 , patEmitString = ESTSimple
                                       [ ESVerbatim $ "BEQ "
                                       , ESLocationOfValueNode 4
                                       , ESVerbatim ", "
                                       , ESNameOfBlockNode 2
                                       , ESVerbatim ", "
                                       , ESVerbatim getZeroRegName
                                       ]
                 }
             , InstrPattern
                 { patID = 1
                 , patOS = OS.OpStructure g (Just 1) inv_cs
                 , patADDUC = True
                 , patEmitString = ESTSimple
                                       [ ESVerbatim $ "BNE "
                                       , ESLocationOfValueNode 4
                                       , ESVerbatim ", "
                                       , ESNameOfBlockNode 3
                                       , ESVerbatim ", "
                                       , ESVerbatim getZeroRegName
                                       ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Makes the unconditional branch instructions.
mkBrInstrs :: [Instruction]
mkBrInstrs =
  let mkBlockNode = BlockNode $ BlockName ""
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Br) )
                , ( 1, NodeLabel 1 mkBlockNode )
                , ( 2, NodeLabel 2 mkBlockNode )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                ]
            )
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 1) []
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESVerbatim "B "
                                  , ESNameOfBlockNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [ pat
                           , (combineNodesInInstrPattern pat 1 2)
                               { patID = 1 }
                           ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes the return instructions.
mkRetInstrs :: [Instruction]
mkRetInstrs =
  let g n = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Ret) )
                , ( 1, NodeLabel 1 (BlockNode $ BlockName "") )
                , ( 2, NodeLabel 2 (ValueNode (mkIntTempType n) Nothing) )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      vg = mkGraph
           ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Ret) )
                , ( 1, NodeLabel 1 (BlockNode $ BlockName "") )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 ) ]
            )
      reg_cs = mkNewDataLocConstraints [locID getRetRegister] 2
      pat n str =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g n) (Just 1) (reg_cs)
          , patADDUC = True
          , patEmitString = str
          }
      vpat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure (vg) (Just 1) []
          , patADDUC = True
          , patEmitString = ESTSimple [ ESVerbatim "RetRA" ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat 32 (ESTSimple
                                    [ ESVerbatim "RetRA "
                                    , ESLocationOfValueNode 2
                                    ])
                           , vpat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
       ,
       Instruction
         { instrID = 0
           -- The 16-bits returns truncate the result value by shifting 16 bits
           -- to the left and 16 bits to the right "arithmetically".
         , instrPatterns = [pat 16 (ESTMulti
                                    [
                                     ESTSimple
                                     [ ESVerbatim "%temp1 = SLL "
                                     , ESLocationOfValueNode 2
                                     , ESVerbatim ", 16"
                                     ],
                                     ESTSimple [ ESVerbatim
                                                   "%temp2 = SRA %temp1, 16" ],
                                     ESTSimple [ ESVerbatim "RetRA %temp2" ]
                                    ])]
         , instrProps = InstrProperties { instrCodeSize = 12
                                        , instrLatency = 2
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes the 'mfhi' instruction.
mkMfhiInstrs :: [Instruction]
mkMfhiInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkNewDataLocConstraints [locID getHIRegister] 1
           ++
           mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESLocationOfValueNode 2
                                  , ESVerbatim " = PseudoMFHI "
                                  , ESLocationOfValueNode 1
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes the 'mflo' instruction.
mkMfloInstrs :: [Instruction]
mkMfloInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkNewDataLocConstraints [locID getLORegister] 1
           ++
           mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESLocationOfValueNode 2
                                  , ESVerbatim " = PseudoMFLO "
                                  , ESLocationOfValueNode 1
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes the 'move' instruction.
mkPseudoMoveInstrs :: [Instruction]
mkPseudoMoveInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkNewDataLocConstraints (map locID getGPRegistersInclZero) 1
           ++
           mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patADDUC = True
          , patEmitString = ESTSimple
                                  [ ESVerbatim "move "
                                  , ESLocationOfValueNode 1
                                  , ESVerbatim ", "
                                  , ESLocationOfValueNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = False
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Implements the load of immediates.
mkLoadImmInstr :: [Instruction]
mkLoadImmInstr =
  let g w r     = mkSimpleCopyPattern (mkIntConstType r 32) (mkIntTempType w)
      cs ls     = mkNewDataLocConstraints (map locID ls) 2
      pat w r ls a =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g w r) Nothing (cs ls)
          , patADDUC = True
          , patEmitString = ESTSimple a
          }
  in [ Instruction
       -- Zero immediate (free in mips32)
         { instrID = 0
         , instrPatterns = [ pat 16 (Range 0 0) [getZeroRegister] []
                           , pat 32 (Range 0 0) [getZeroRegister] []
                           ]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = False
                                        , instrIsNullCopy = False
                                        }
         }
     , Instruction
       -- 16-bits immediates (implemented with ADDiu)
         { instrID = 0
         , instrPatterns = [ pat 16
                                 (Range (-32768) 32767)
                                 getGPRegistersWithoutZero
                                 [ ESLocationOfValueNode 2
                                 , ESVerbatim " = "
                                 , ESVerbatim
                                   $ "ADDiu " ++ getZeroRegName ++ ", "
                                 , ESIntConstOfValueNode 1
                                 ]
                           , pat 32
                                 (Range (-32768) 32767)
                                 getGPRegistersWithoutZero
                                 [ ESLocationOfValueNode 2
                                 , ESVerbatim " = "
                                 , ESVerbatim
                                   $ "ADDiu " ++ getZeroRegName ++ ", "
                                 , ESIntConstOfValueNode 1
                                 ]
                           ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = False
                                        , instrIsNullCopy = False
                                        }
         }
     , Instruction
       -- Arbitrary immediates (implemented with lui + ori)
         { instrID = 0
         , instrPatterns = [
                             pat 16
                                 (Range (-2147483648) 2147483647)
                                 getGPRegistersWithoutZero
                                 [ ESLocationOfValueNode 2
                                 , ESVerbatim " = "
                                 , ESVerbatim "LUi+ORi "
                                 , ESIntConstOfValueNode 1
                                 ]
                           , pat 32
                                 (Range (-2147483648) 2147483647)
                                 getGPRegistersWithoutZero
                                 [ ESLocationOfValueNode 2
                                 , ESVerbatim " = "
                                 , ESVerbatim "LUi+ORi "
                                 , ESIntConstOfValueNode 1
                                 ]
                           ]
         , instrProps = InstrProperties { instrCodeSize = 8
                                        , instrLatency = 2
                                        , instrIsNonCopy = False
                                        , instrIsNullCopy = False
                                        }
         }
     , Instruction
       -- Immediate -2147483648 (implemented with lui only)
         { instrID = 0
         , instrPatterns = [ pat 32
                                 (Range (-2147483648) (-2147483648))
                                 getGPRegistersWithoutZero
                                 [ ESLocationOfValueNode 2
                                 , ESVerbatim " = "
                                 , ESVerbatim "LUi "
                                 , ESIntConstOfValueNode 1
                                 ]
                           ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = False
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes the various move instructions. Note that the instruction ID will be
-- (incorrectly) set to 0 for all instructions.
mkMoveInstrs :: [Instruction]
mkMoveInstrs =
  mkMfhiInstrs
  ++
  mkMfloInstrs
  ++
  mkPseudoMoveInstrs
  ++
  mkLoadImmInstr

-- | Makes type conversion instructions.
mkTypeConvInstrs :: [Instruction]
mkTypeConvInstrs =
  let mkCompNode op = ComputationNode { compOp = op }
      g t (n, m) = mkGraph
       ( map
           Node
           [ ( 0, NodeLabel 0 (mkCompNode $ O.CompTypeConvOp t) )
           , ( 1, NodeLabel 1 (mkValueNode (mkIntTempType n)) )
           , ( 2, NodeLabel 2 (mkValueNode (mkIntTempType m)) )
           ]
       )
       ( map
           Edge
           [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
           , ( 0, 2, EdgeLabel DataFlowEdge 0 0 )
           ]
       )
      cs  = mkNewDataLocConstraints (map locID getGPRegistersInclZero) 1
            ++
            mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat t (n, m) =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g t (n, m)) Nothing cs
          , patADDUC = True
          , patEmitString = ESTSimple []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [ pat O.SExt  (16, 32)
                           , pat O.Trunc (32, 16)
                           ]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]
     ++
     [ Instruction
         { instrID = 0
         , instrPatterns =
             [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure (g O.ZExt (8, 32)) Nothing cs
                 , patADDUC = True
                 , patEmitString = ESTSimple
                                         [ ESLocationOfValueNode 2
                                         , ESVerbatim $ " = ANDi "
                                         , ESLocationOfValueNode 1
                                         , ESVerbatim ", 255"
                                         ]
                 }
             ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                        }
         }
     ]

-- | Makes a "set if equal to" comparison. The actual implementation does a
-- 'result = (slt $ZERO, (XOR input imm))'. See
-- https://gcc.gnu.org/ml/gcc-patches/2012-05/msg00867.html .
mkEqComparison :: Instruction
mkEqComparison =
  let dt16 = mkIntTempType 16
      imm  = mkIntConstType (Range (-32768) 32767) 16
      dt1  = mkIntTempType 1
      g    = mkSimpleCompPattern (O.CompArithOp $ O.IntOp O.Eq)
                                 False
                                 dt16
                                 imm
                                 dt1
      cs   = mkNewDataLocConstraints (map locID getGPRegistersInclZero) 1
             ++
             mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 3
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patADDUC = True
              , patEmitString = ESTMulti
                                    [ ESTSimple
                                      [ ESVerbatim "%temp1 = "
                                      , ESVerbatim "XOR "
                                      -- TODO: looks lite ESTSimple must have
                                      -- a certain number of elements to work,
                                      -- otherwise we get
                                      -- "updateNodeIDsInEmitStrParts: Invalid
                                      -- arguments".
                                      , ESVerbatim ""
                                      , ESLocationOfValueNode 1
                                      , ESVerbatim ", "
                                      , ESIntConstOfValueNode 2
                                      ]
                                    , ESTSimple
                                      [ ESVerbatim $
                                        "%cmp = SLT " ++ getZeroRegName ++ ", %temp1" ]
                                    ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 2
                                      , instrIsNonCopy = True
                                        , instrIsNullCopy = False
                                      }
       }

-- | Makes a "set if less than to" comparison with an immediate.
mkSLTIComparison :: Instruction
mkSLTIComparison =
  let dt32 = mkIntTempType 32
      imm  = mkIntConstType (Range (-32768) 32767) 16
      dt1  = mkIntTempType 1
      g    = mkSimpleCompPattern (O.CompArithOp $ O.IntOp O.LT)
                                 False
                                 dt32
                                 imm
                                 dt1
      cs   = mkNewDataLocConstraints (map locID getGPRegistersInclZero) 1
             ++
             mkNewDataLocConstraints (map locID getGPRegistersWithoutZero) 3
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patADDUC = True
              , patEmitString = ESTSimple
                                      [ ESLocationOfValueNode 3
                                      , ESVerbatim " = SLTi "
                                      , ESLocationOfValueNode 1
                                      , ESVerbatim ", "
                                      , ESIntConstOfValueNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 0
                                                       -- Should be 1, but LLVM
                                                       -- uses 0 (TODO: fix)
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates a vectorized add3 instruction.
mkSimdAddInstruction :: Instruction
mkSimdAddInstruction =
  let mkAddGraph off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, (ComputationNode $ O.CompArithOp $ O.SIntOp O.Add))
                , (off + 1, (mkValueNode (mkIntTempType 32)))
                , (off + 2, (mkValueNode (mkIntTempType 32)))
                , (off + 3, (mkValueNode (mkIntTempType 32)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off + 2, off    , 0, 1)
              , (off    , off + 3, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      add2_g = combineGraphs (mkAddGraph 0) (mkAddGraph 4)
      add3_g = combineGraphs (mkAddGraph 8) add2_g
      add2_input = [1, 2, 5, 6]
      add3_input = add2_input ++ [9, 10]
      add2_output = [3, 7]
      add3_output = add2_output ++ [11]
      add2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          add2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          add2_output
      add3_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          add3_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          add3_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure add3_g Nothing add3_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 11
                                 , ESVerbatim " = ADD3 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 6
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 9
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 10
                                 , ESVerbatim ")"
                                 ]
                 }
             , InstrPattern
                 { patID = 1
                 , patOS = OS.OpStructure add2_g Nothing add2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = ADD3 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 6
                                 , ESVerbatim ") ()"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates vectorized andi2 and andi3 instructions.
mkSimdAndiInstruction :: Instruction
mkSimdAndiInstruction =
  let mkAndGraph off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, (ComputationNode $ O.CompArithOp $ O.IntOp O.And))
                , (off + 1, (mkValueNode (mkIntTempType 32)))
                , (off + 2, (mkValueNode (mkIntConstType (Range 0 65535) 16)))
                , (off + 3, (mkValueNode (mkIntTempType 32)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off + 2, off    , 0, 1)
              , (off    , off + 3, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      and2_g = combineGraphs (mkAndGraph 0) (mkAndGraph 4)
      and3_g = combineGraphs (mkAndGraph 8) and2_g
      and2_input = [1, 5]
      and3_input = and2_input ++ [9]
      and2_output = [3, 7]
      and3_output = and2_output ++ [11]
      and2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          and2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          and2_output
      and3_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          and3_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          and3_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure and3_g Nothing and3_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 11
                                 , ESVerbatim " = ANDi3 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 6
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 9
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 10
                                 , ESVerbatim ")"
                                 ]
                 }
             , InstrPattern
                 { patID = 1
                 , patOS = OS.OpStructure and2_g Nothing and2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = ANDi2 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 6
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates vectorized (sll, srl) instruction.
mkSimdSlrlInstruction :: Instruction
mkSimdSlrlInstruction =
  let mkSlrlGraph op off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, (ComputationNode $ O.CompArithOp $ O.IntOp op))
                , (off + 1, (mkValueNode (mkIntTempType 32)))
                , (off + 2, (mkValueNode (mkIntConstType (Range 0 32) 5)))
                , (off + 3, (mkValueNode (mkIntTempType 32)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off + 2, off    , 0, 1)
              , (off    , off + 3, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      slrl2_g = combineGraphs (mkSlrlGraph O.Shl 0) (mkSlrlGraph O.LShr 4)
      slrl2_input = [1, 5]
      slrl2_output = [3, 7]
      slrl2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          slrl2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          slrl2_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure slrl2_g Nothing slrl2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = SLRL2 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 6
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates a vectorized (sll, srl) instruction from single constant.
mkSimdSlrlConstantInstruction :: Instruction
mkSimdSlrlConstantInstruction =
  let slrl_g = mkGraph
               [
                Node (0, NodeLabel (toNodeID (0 :: Integer)) (ComputationNode $ O.CompArithOp $ O.IntOp O.Shl)),
                Node (1, NodeLabel (toNodeID (1 :: Integer)) (ComputationNode $ O.CompArithOp $ O.IntOp O.LShr)),
                Node (2, NodeLabel (toNodeID (2 :: Integer)) (mkValueNode (mkIntConstType (Range 0 32) 5))),
                Node (3, NodeLabel (toNodeID (3 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (4, NodeLabel (toNodeID (4 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (5, NodeLabel (toNodeID (5 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (6, NodeLabel (toNodeID (6 :: Integer)) (mkValueNode (mkIntTempType 32)))
               ]
               [
                Edge (3, 0, EdgeLabel DataFlowEdge 0 0),
                Edge (2, 0, EdgeLabel DataFlowEdge 0 1),
                Edge (4, 1, EdgeLabel DataFlowEdge 0 0),
                Edge (2, 1, EdgeLabel DataFlowEdge 0 1),
                Edge (0, 5, EdgeLabel DataFlowEdge 0 0),
                Edge (1, 6, EdgeLabel DataFlowEdge 0 0)
               ]
      slrl_input  = [3, 4]
      slrl_output = [5, 6]
      slrl_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          slrl_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          slrl_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure slrl_g Nothing slrl_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 6
                                 , ESVerbatim " = SLRL2 ("
                                 , ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 4
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates a vectorized sllv2 instruction.
mkSimdSllvInstruction :: Instruction
mkSimdSllvInstruction =
  let mkSllvGraph off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, (ComputationNode $ O.CompArithOp $ O.IntOp O.Shl))
                , (off + 1, (mkValueNode (mkIntTempType 32)))
                , (off + 2, (mkValueNode (mkIntTempType 32)))
                , (off + 3, (mkValueNode (mkIntTempType 32)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off + 2, off    , 0, 1)
              , (off    , off + 3, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      sllv2_g = combineGraphs (mkSllvGraph 0) (mkSllvGraph 4)
      sllv2_input = [1, 2, 5, 6]
      sllv2_output = [3, 7]
      sllv2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          sllv2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          sllv2_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure sllv2_g Nothing sllv2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = SLLV2 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 6
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates a vectorized nor instruction.
mkSimdNorInstruction :: Instruction
mkSimdNorInstruction =
  let xor2_g = mkGraph
               [
                Node (0, NodeLabel (toNodeID (0 :: Integer)) (ComputationNode $ O.CompArithOp $ O.IntOp O.XOr)),
                Node (1, NodeLabel (toNodeID (1 :: Integer)) (ComputationNode $ O.CompArithOp $ O.IntOp O.XOr)),
                Node (2, NodeLabel (toNodeID (2 :: Integer)) (mkValueNode (mkIntConstType (Range (-1) (-1)) 16))),
                Node (3, NodeLabel (toNodeID (3 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (4, NodeLabel (toNodeID (4 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (5, NodeLabel (toNodeID (5 :: Integer)) (mkValueNode (mkIntTempType 32))),
                Node (6, NodeLabel (toNodeID (6 :: Integer)) (mkValueNode (mkIntTempType 32)))
               ]
               [
                Edge (3, 0, EdgeLabel DataFlowEdge 0 0),
                Edge (2, 0, EdgeLabel DataFlowEdge 0 1),
                Edge (4, 1, EdgeLabel DataFlowEdge 0 0),
                Edge (2, 1, EdgeLabel DataFlowEdge 0 1),
                Edge (0, 5, EdgeLabel DataFlowEdge 0 0),
                Edge (1, 6, EdgeLabel DataFlowEdge 0 0)
               ]
      xor2_input  = [3, 4]
      xor2_output = [5, 6]
      xor2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          xor2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          xor2_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure xor2_g Nothing xor2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 6
                                 , ESVerbatim " = NOR2 ("
                                 , ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESVerbatim getZeroRegName
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 4
                                 , ESVerbatim ", "
                                 , ESVerbatim getZeroRegName
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates vectorized srl instruction.
mkSimdSrlInstruction :: Instruction
mkSimdSrlInstruction =
  let mkSrlGraph off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, (ComputationNode $ O.CompArithOp $ O.IntOp O.LShr))
                , (off + 1, (mkValueNode (mkIntTempType 32)))
                , (off + 2, (mkValueNode (mkIntConstType (Range 0 32) 5)))
                , (off + 3, (mkValueNode (mkIntTempType 32)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off + 2, off    , 0, 1)
              , (off    , off + 3, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      srl2_g = combineGraphs (mkSrlGraph 0) (mkSrlGraph 4)
      srl2_input = [1, 5]
      srl2_output = [3, 7]
      srl2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersInclZero))
                          srl2_input
                ++
                concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          srl2_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure srl2_g Nothing srl2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = SRL2 ("
                                 , ESLocationOfValueNode 1
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESLocationOfValueNode 5
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 6
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates vectorized addiu instruction to load 16-bits immediates.

mkSimdLoad16ImmInstruction :: Natural -> Instruction
mkSimdLoad16ImmInstruction w =
  let mkLiGraph off =
        mkGraph
          ( map (\(n, l) -> Node (n, NodeLabel (toNodeID n) l))
                [ (off + 0, CopyNode)
                , (off + 1, (mkValueNode (mkIntConstType (Range (-32768) 32767) 16)))
                , (off + 2, (mkValueNode (mkIntTempType w)))
                ]
          )
          ( map ( \(n1, n2, out_nr, in_nr) ->
                  Edge (n1, n2, EdgeLabel DataFlowEdge out_nr in_nr)
                )
              [ (off + 1, off    , 0, 0)
              , (off    , off + 2, 0, 0)
              ]
          )
      combineGraphs g1 g2 =
        mkGraph (getAllNodes g1 ++ getAllNodes g2)
                (getAllEdges g1 ++ getAllEdges g2)
      li2_g = combineGraphs (mkLiGraph 0) (mkLiGraph 3)
      li2_output = [2, 5]
      li2_cs = concatMap (mkNewDataLocConstraints (map locID getGPRegistersWithoutZero))
                          li2_output
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure li2_g Nothing li2_cs
                 , patADDUC = True
                 , patEmitString =
                     ESTSimple [ ESLocationOfValueNode 3
                                 , ESVerbatim ", "
                                 , ESLocationOfValueNode 7
                                 , ESVerbatim " = ADDiu2 ("
                                 , ESVerbatim getZeroRegName
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 2
                                 , ESVerbatim ") ("
                                 , ESVerbatim getZeroRegName
                                 , ESVerbatim ", "
                                 , ESIntConstOfValueNode 6
                                 , ESVerbatim ")"
                                 ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 8
                                      , instrLatency = 1
                                      , instrIsNonCopy = False
                                      , instrIsNullCopy = False
                                      }
       }

-- | Creates the list of MIPS instructions. Note that the instruction ID will be
-- (incorrectly) set to 0 for all instructions.
mkInstructions :: [Instruction]
mkInstructions =
  mkGenericPhiInstructions
  ++
  mkGenericBrFallThroughInstructions
  ++
  mkGenericDataDefInstructions
  ++
  mkGenericCopyInstructions
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersInclZero
              getGPRegistersWithoutZero
    )
    [ ("ADD" , O.SIntOp O.Add)
    , ("ADDu", O.UIntOp O.Add)
    , ("SUB" , O.SIntOp O.Sub)
    , ("SUBu", O.UIntOp O.Sub)
    , ("SLLV", O.IntOp O.Shl)
    , ("SRLV", O.IntOp O.LShr)
    , ("SRAV", O.IntOp O.AShr)
    ]
  ++
  [ let i =
            mkSimple32BitRegRegCompInst
              "MUL"
              (O.CompArithOp $ O.SIntOp O.Mul)
              getGPRegistersInclZero
              getGPRegistersInclZero
              getGPRegistersWithoutZero
    in updateLatency 17 i
  ]
  ++ [
      mkSimpleNBitRegRegCompInst
              1
              "AND"
              (O.CompArithOp $ O.IntOp O.And)
              getGPRegistersInclZero
              getGPRegistersInclZero
              getGPRegistersWithoutZero
     ]
  ++
  [ let i =
            mkSimple32BitRegRegCompInst
              "PseudoSDIV"
              (O.CompArithOp $ O.SIntOp O.Rem)
              getGPRegistersInclZero
              getGPRegistersInclZero
              [getHIRegister]
    in updateLatency 38 i
  ]
  ++
  [ let i =
            mkSimple32BitRegRegCompInst
              "PseudoSDIV"
              (O.CompArithOp $ O.SIntOp O.Div)
              getGPRegistersInclZero
              getGPRegistersInclZero
              [getLORegister]
    in updateLatency 38 i
  ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataEmitStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range (-32768) 32767)
              32
              16
    )
    [ ("ADDi", O.SIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataEmitStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 65535)
              32
              16
    )
    [ ("ADDiu", O.UIntOp O.Add),
      ("ANDi",  O.IntOp O.And),
      ("ORi",   O.IntOp O.Or)
    ]
  ++
  map
    ( \a -> mkSimple32BitRegs1BitResultCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersInclZero
              getGPRegistersWithoutZero
    )
    [ ("AND", O.IntOp O.And)
    , ("OR" , O.IntOp O.Or)
    , ("XOR", O.IntOp O.XOr)
    , ("SLT", O.IntOp O.LT)
    , ("SLTu", O.UIntOp O.GT)
    ]
  ++
  [ mkEqComparison ]
  ++
  concatMap
    ( \(s1, op1, s2, op2) -> [ mkRegRegCondBrInstrs 16
                                                    s1
                                                    (O.CompArithOp op1)
                                                    s2
                                                    (O.CompArithOp op2)
                             , mkRegRegCondBrInstrs 32
                                                    s1
                                                    (O.CompArithOp op1)
                                                    s2
                                                    (O.CompArithOp op2)
                             ]
    )
    [ ("BGT", O.SIntOp O.GT, "BLE", O.SIntOp O.LE)
    , ("BLT", O.SIntOp O.LT, "BGE", O.SIntOp O.GE)
    , ("BGE", O.SIntOp O.GE, "BLT", O.SIntOp O.LT)
    , ("BLE", O.SIntOp O.LE, "BGT", O.SIntOp O.GT)
    , ("BEQ", O.IntOp  O.Eq,  "BNE", O.IntOp  O.NEq)
    , ("BNE", O.IntOp  O.NEq, "BEQ", O.IntOp  O.Eq)
    ]
  ++
  concatMap
    ( \(r, s, op) -> [ mkRegImmCondBrInstr 16 r s (O.CompArithOp op)
                     , mkRegImmCondBrInstr 32 r s (O.CompArithOp op)
                     ]
    )
    [ ((Range    0   0 ), "BLEZ", O.SIntOp O.LE)
    , ((Range    1   1 ), "BLEZ", O.SIntOp O.LT)
      -- The BLEZ patterns subsumes cases for BGEZ
    , ((Range   0    0 ), "BLTZ", O.SIntOp O.LT)
    , ((Range (-1) (-1)), "BLTZ", O.SIntOp O.LE)
      -- The BLTZ patterns subsumes cases for BGTZ
    ]
  ++
    -- Implements a BGT as a SLTi + BEQ (LLVM gives SLTi latency 0)
  [ let i = mkRegImmCondBrInstr 32 (Range (-32768) 32766) "BEQLT"
            (O.CompArithOp $ O.SIntOp O.GT)
    in updateEmitStrTemplate
       (ESTMulti
        [ ESTSimple
          [ ESVerbatim $ "%temp1 = SLTi "
          , ESLocationOfValueNode 5
          , ESVerbatim ", "
          , ESIntConstOfValueNode 6
          ]
        , ESTSimple
          [ ESVerbatim $ "BEQ "
          , ESVerbatim "%temp1, "
          , ESVerbatim "%ZERO, "
          , ESNameOfBlockNode 2
          ]
        ]
       ) i
  ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataEmitStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 32)
              32
              5
    )
    [ ("SLL", O.IntOp O.Shl)
    , ("SRL", O.IntOp O.LShr)
    , ("SRA", O.IntOp O.AShr)
    ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataEmitStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 65535)
              8
              16
    )
    [ ("XORi", O.IntOp O.XOr) ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataEmitStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 65535)
              16
              16
    )
    [ ("SRA", O.IntOp O.AShr) ]
  ++
  map
    ( \n -> mkSimpleNBitRegMBitImmCompInst
              [ ESLocationOfValueNode 3
              , ESVerbatim $ " = "
              , ESVerbatim $ "NOR "
              , ESLocationOfValueNode 1
              , ESVerbatim ", "
              , ESVerbatim getZeroRegName
              ]
              (O.CompArithOp $ O.IntOp O.XOr)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range (-1) (-1))
              n
              16
    )
    [ 8, 32 ]
  ++
    [ mkSimpleNBitRegMBitFirstImmCompInst
              [ ESLocationOfValueNode 3
              , ESVerbatim " = "
              , ESVerbatim "SUBu "
              , ESVerbatim getZeroRegName
              , ESVerbatim ", "
              , ESLocationOfValueNode 2
              ]
              (O.CompArithOp $ O.UIntOp O.Sub)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 0)
              16
              16
    ]
  ++
  [mkPredBrInstr]
  ++
  mkBrInstrs
  ++
  mkRetInstrs
  ++
  mkMoveInstrs
  ++
  mkTypeConvInstrs
  ++
  [mkSLTIComparison]

-- | Creates the list of MIPS instructions, including the fancy onces. Note that
-- the instruction ID will be (incorrectly) set to 0 for all instructions.
mkInstructionsInclFancy :: [Instruction]
mkInstructionsInclFancy =
  mkInstructions
  ++
  [mkSimdAddInstruction]
  ++
  [mkSimdAndiInstruction]
  ++
  [mkSimdSlrlInstruction]
  ++
  [mkSimdSllvInstruction]
  ++
  [mkSimdNorInstruction]
  ++
  [mkSimdSrlInstruction]
  ++
  [mkSimdSlrlConstantInstruction]
  {-
    TODO: if we activate these, no solution is found for g721.g72x.reconstruct
    TODO: extend to 3- and 4-way SIMD
  ++
  [ mkSimdLoad16ImmInstruction 16
  , mkSimdLoad16ImmInstruction 32
  ]
  -}

-- | Constructs the target machine data for ordinary MIPS.
theTM :: TargetMachine
theTM = TargetMachine
             { tmID = toTargetMachineID "mips32"
             , tmInstructions = fixInstrIDs mkInstructions
             , tmLocations = getAllLocations
             }

-- | Constructs the target machine data for fancy MIPS.
tmFancyMips32 :: TargetMachine
tmFancyMips32 = TargetMachine
                  { tmID = toTargetMachineID "fmips32"
                  , tmInstructions = fixInstrIDs mkInstructionsInclFancy
                  , tmLocations = getAllLocations
                  }
