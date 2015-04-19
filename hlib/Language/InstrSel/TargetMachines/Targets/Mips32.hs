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
  ( tmMips32
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

-- | Creates all location classes, but there are no guarantees that the location
-- IDs will be correctly set!
mkRegClasses :: [Location]
mkRegClasses = mkGPRegisters ++ mkHILORegisters

regPrefix :: String
regPrefix = "$"

updateLatency :: Integer -> Instruction -> Instruction
updateLatency l i =
    let p = instrProps i
    in i {instrProps = p { instrLatency = l } }

-- | The name of the zero register.
getZeroRegName :: String
getZeroRegName = "%ZERO"

-- | Creates the list of general-purpose registers, but there are no guarantees
-- that the location IDs will be correctly set!
mkGPRegisters :: [Location]
mkGPRegisters =
  [ Location { locID = 0
             , locName = LocationName getZeroRegName
             , locIsAValue = True
             }
  ]
  ++
  map
    ( \i -> Location { locID = 0
                     , locName = LocationName $ regPrefix ++ show i
                     , locIsAValue = False
                     }
    )
    ([1..31] :: [Integer]) -- Cast needed to prevent compilation warning

-- | Creates the list of 'HILO' registers (which are actually memory
-- locations). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkHILORegisters :: [Location]
mkHILORegisters =
  [ Location { locID = 0
             , locName = LocationName "HILO"
             , locIsAValue = False
             }
  , mkHIRegister
  , mkLORegister
  ]

-- | Creates the list of 'HI' register (which is actually a memory
-- location). Note that there are no guarantees that the location IDs will be
-- correctly set!
mkHIRegister :: Location
mkHIRegister = Location { locID = 0
                        , locName = LocationName "HI"
                        , locIsAValue = False
                        }

-- | Creates the list of 'LO' register (which is actually a memory
-- location). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkLORegister :: Location
mkLORegister = Location { locID = 0
                        , locName = LocationName "LO"
                        , locIsAValue = False
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
getAllLocations = fixLocIDs $ mkRegClasses

-- | Retrieves all locations, including those used by fancy instructions, where
-- the location IDs have been set such that every location is given a unique ID.
getAllLocationsInclFancy :: [Location]
getAllLocationsInclFancy =
  -- TODO: implement
  undefined

-- | Retrieves all general-purpose registers, includeing the zero register,
-- where the location IDs have been set such that every location is given a
-- unique ID.
getGPRegistersInclZero :: [Location]
getGPRegistersInclZero = getGPRegistersWithoutZero ++ [getZeroRegister]

-- | Same as 'getGPRegistersInclZero' but without the zero register.
getGPRegistersWithoutZero :: [Location]
getGPRegistersWithoutZero =
  map
    (\i -> getRegisterByName $ LocationName $ regPrefix ++ show i)
    ([1..31] :: [Integer]) -- Cast needed to prevent compilation warning

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
getHIRegister = getRegisterByName $ LocationName "HI"

-- | Retrieves the 'LO' register. The location ID will be correctly set.
getLORegister :: Location
getLORegister = getRegisterByName $ LocationName "LO"

-- | Creates a simple pattern that consists of a single computation node, which
-- takes two value nodes as input, and produces another value node as output.
mkSimpleCompPattern
  :: O.CompOp
     -- ^ The computation operation.
  -> D.DataType
     -- ^ The data type of the first operand.
  -> D.DataType
     -- ^ The data type of the second operand.
  -> D.DataType
     -- ^ The data type of the result.
  -> Graph
mkSimpleCompPattern op src1 src2 dst =
  let mkCompNode = ComputationNode { compOp = op }
  in mkGraph
       ( map
           Node
           [ ( 0, NodeLabel 0 mkCompNode )
           , ( 1, NodeLabel 1 (mkValueNode src1) )
           , ( 2, NodeLabel 2 (mkValueNode src2) )
           , ( 3, NodeLabel 3 (mkValueNode  dst) )
           ]
       )
       ( map
           Edge
           [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
           , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
           , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
           ]
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
       ( map
           Node
           [ ( 0, NodeLabel 0 CopyNode )
           , ( 1, NodeLabel 1 (mkValueNode src) )
           , ( 2, NodeLabel 2 (mkValueNode dst) )
           ]
       )
       ( map
           Edge
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
     -- ^ The assembly string corresponding to this instruction.
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
  let g = mkSimpleCompPattern op d1 d2 d3
      cs = concatMap ( \(r, nid) ->
                       mkDataLocConstraints (map locID r) nid
                     )
                     (zip [r1, r2, r3] [1, 2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = ASSTemplate
                                      [ ASLocationOfDataNode 3
                                      , ASVerbatim $ " = "
                                      , ASVerbatim $ str ++ " "
                                      , ASLocationOfDataNode 1
                                      , ASVerbatim ", "
                                      , ASLocationOfDataNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  All values are assumed to be N bits in size.
mkSimpleNBitRegRegCompInst
  :: Natural
     -- ^ The width of the data.
  -> String
     -- ^ The assembly string corresponding to this instruction.
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
     -- ^ The assembly string corresponding to this instruction.
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

mkDataImmDataAsmStr :: String -> [AssemblyStringPart]
mkDataImmDataAsmStr str =
    [ ASLocationOfDataNode 3
    , ASVerbatim $ " = "
    , ASVerbatim $ str ++ " "
    , ASLocationOfDataNode 1
    , ASVerbatim ", "
    , ASImmIntValueOfDataNode 2
    ]

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  The first input operand and result are assumed to reside in one of
-- the 32 general-purpose registers, and the second input operand is assumed to
-- be a N-bit immediate of a given range.
mkSimpleNBitRegMBitImmCompInst
  :: [AssemblyStringPart]
     -- ^ The assembly string parts of the instruction.
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
      g = mkSimpleCompPattern op dtN dtM dtN
      cs = concatMap ( \(r, nid) ->
                       mkDataLocConstraints (map locID r) nid
                     )
                     (zip [r1, r3] [1, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = ASSTemplate str
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
                                      }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two value nodes as input, and produces another value node as
-- output.  The second input operand and result are assumed to reside in one of
-- the 32 general-purpose registers, and the first input operand is assumed to
-- be a N-bit immediate of a given range.
mkSimpleNBitRegMBitFirstImmCompInst
  :: [AssemblyStringPart]
     -- ^ The assembly string parts of the instruction.
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
      g = mkSimpleCompPattern op dtM dtN dtN
      cs = concatMap ( \(r, nid) ->
                       mkDataLocConstraints (map locID r) nid
                     )
                     (zip [r2, r3] [2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = ASSTemplate str
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
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
      old_output = patOutputValueNodes ip
      old_template = patAsmStrTemplate ip
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
      new_output = map checkAndReplace old_output
      new_template = updateNodeInAsmStrTemplate keep_n disc_n old_template
  in ip { patOS = new_os
        , patOutputValueNodes = new_output
        , patAsmStrTemplate = new_template
        }

-- | Makes two conditional branch instructions - an ordinary branch instruction
-- and its inverse branch instruction - that takes two registers as input. The
-- inverse is achieved by inverting the comparison, and swapping the branch
-- blocks, which means both instructions carry the same semantics.
mkRegRegCondBrInstrs
  :: Natural
     -- ^ The width of the comparison operands.
  -> String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The comparison corresponding to this instruction.
  -> String
     -- ^ The inverse assembly string corresponding to this instruction.
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
      ord_cs = mkMatchPlacementConstraints ord_g
               ++
               mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 3
      inv_cs = mkMatchPlacementConstraints inv_g
               ++
               mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 2
      ord_pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure ord_g (Just 1) ord_cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASVerbatim $ ord_str ++ " "
                                  , ASLocationOfDataNode 5
                                  , ASVerbatim ", "
                                  , ASLocationOfDataNode 6
                                  , ASVerbatim ", "
                                  , ASBlockOfLabelNode 2
                                  ]
          }
      inv_pat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure inv_g (Just 1) inv_cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASVerbatim $ inv_str ++ " "
                                  , ASLocationOfDataNode 5
                                  , ASVerbatim ", "
                                  , ASLocationOfDataNode 6
                                  , ASVerbatim ", "
                                  , ASBlockOfLabelNode 3
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
     -- ^ The assembly string corresponding to this instruction.
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
      ord_cs = mkMatchPlacementConstraints ord_g
               ++
               mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 3
      swapped_cs = mkMatchPlacementConstraints swapped_g
                   ++
                   mkNoDataReuseConstraints 7
                   ++
                   mkFallThroughConstraints 3
      mkInstrPattern pid g cs =
        InstrPattern
          { patID = pid
          , patOS = OS.OpStructure g (Just 1) cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASVerbatim $ str ++ " "
                                  , ASReferenceToValueNode 5
                                  , ASVerbatim ", "
                                  , ASNameOfBlockNode 2
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
      ord_cs = mkMatchPlacementConstraints g
               ++
               mkFallThroughConstraints 3
      inv_cs = mkMatchPlacementConstraints g
               ++
               mkFallThroughConstraints 2
      pats = [ InstrPattern
                 { patID = 0
                 , patOS = OS.OpStructure g (Just 1) ord_cs
                 , patOutputValueNodes = []
                 , patADDUC = True
                 , patAsmStrTemplate = ASSTemplate
                                       [ ASVerbatim $ "BEQ "
                                       , ASReferenceToValueNode 4
                                       , ASVerbatim ", "
                                       , ASNameOfBlockNode 2
                                       , ASVerbatim ", "
                                       , ASVerbatim getZeroRegName
                                       ]
                 }
             , InstrPattern
                 { patID = 1
                 , patOS = OS.OpStructure g (Just 1) inv_cs
                 , patOutputValueNodes = []
                 , patADDUC = True
                 , patAsmStrTemplate = ASSTemplate
                                       [ ASVerbatim $ "BNE "
                                       , ASReferenceToValueNode 4
                                       , ASVerbatim ", "
                                       , ASNameOfBlockNode 3
                                       , ASVerbatim ", "
                                       , ASVerbatim getZeroRegName
                                       ]
                 }
             ]
  in Instruction
       { instrID = 0
       , instrPatterns = pats
       , instrProps = InstrProperties { instrCodeSize = 4
                                      , instrLatency = 1
                                      , instrIsNonCopy = True
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
      cs = mkMatchPlacementConstraints g
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 1) cs
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASVerbatim "B "
                                  , ASBlockOfLabelNode 2
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
      bb_cs n = mkMatchPlacementConstraints (g n)
      reg_cs  = mkDataLocConstraints [locID getRetRegister] 2
      pat n str =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g n) (Just 1) (bb_cs n ++ reg_cs)
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = str
          }
      vpat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure (vg) (Just 1)
                    (mkMatchPlacementConstraints vg)
          , patOutputValueNodes = []
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate [ ASVerbatim "RetRA" ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat 32 (ASSTemplate
                                    [ ASVerbatim "RetRA "
                                    , ASLocationOfDataNode 2
                                    ])
                           , vpat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
                                        }
         }
       ,
       Instruction
         { instrID = 0
           -- The 16-bits returns truncate the result value by shifting 16 bits
           -- to the left and 16 bits to the right "arithmetically".
         , instrPatterns = [pat 16 (ASSMultiTemplate
                                    [
                                     ASSTemplate
                                     [ ASVerbatim "%temp1 = SLL "
                                     , ASLocationOfDataNode 2
                                     , ASVerbatim ", 16"
                                     ],
                                     ASSTemplate [ ASVerbatim
                                                   "%temp2 = SRA %temp1, 16" ],
                                     ASSTemplate [ ASVerbatim "RetRA %temp2" ]
                                    ])]
         , instrProps = InstrProperties { instrCodeSize = 12
                                        , instrLatency = 2
                                        , instrIsNonCopy = True
                                        }
         }
     ]

-- | Makes the 'mfhi' instruction.
mkMfhiInstrs :: [Instruction]
mkMfhiInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints [locID getHIRegister] 1
           ++
           mkDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputValueNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASLocationOfDataNode 2
                                  , ASVerbatim " = PseudoMFHI "
                                  , ASLocationOfDataNode 1
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        }
         }
     ]

-- | Makes the 'mflo' instruction.
mkMfloInstrs :: [Instruction]
mkMfloInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints [locID getLORegister] 1
           ++
           mkDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputValueNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASLocationOfDataNode 2
                                  , ASVerbatim " = PseudoMFLO "
                                  , ASLocationOfDataNode 1
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        }
         }
     ]

-- | Makes the 'move' instruction.
mkPseudoMoveInstrs :: [Instruction]
mkPseudoMoveInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints (map locID getGPRegistersInclZero) 1
           ++
           mkDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputValueNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate
                                  [ ASVerbatim "move "
                                  , ASLocationOfDataNode 1
                                  , ASVerbatim ", "
                                  , ASLocationOfDataNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = False
                                        }
         }
     ]

-- | Implements the load of immediates.
mkLoadImmInstr :: [Instruction]
mkLoadImmInstr =
  let g w r     = mkSimpleCopyPattern (mkIntConstType r 32) (mkIntTempType w)
      cs ls     = mkDataLocConstraints (map locID ls) 2
      pat w r ls a =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g w r) Nothing (cs ls)
          , patOutputValueNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate a
          }
      asm s = [ ASVerbatim $ s ++ " "
              , ASImmIntValueOfDataNode 1
              , ASVerbatim ", "
              , ASLocationOfDataNode 2
              ]
  in [ Instruction
       -- Zero immediate (free in mips32)
         { instrID = 0
         , instrPatterns = [ pat 16 (Range 0 0) [getZeroRegister] []
                           , pat 32 (Range 0 0) [getZeroRegister] []
                           ]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = False
                                        }
         }
     , Instruction
       -- 16-bits immediates (implemented with ADDiu)
         { instrID = 0
         , instrPatterns = [ pat 16
                                 (Range (-32768) 32767)
                                 getGPRegistersWithoutZero
                                 [ ASReferenceToValueNode 2
                                 , ASVerbatim " = "
                                 , ASVerbatim
                                   $ "ADDiu " ++ getZeroRegName ++ ", "
                                 , ASIntConstOfValueNode 1
                                 ]
                           , pat 32
                                 (Range (-32768) 32767)
                                 getGPRegistersWithoutZero
                                 (asm "load-half32")
                           ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = False
                                        }
         }
     , Instruction
       -- Arbitrary immediates (implemented with lui + ori)
         { instrID = 0
         , instrPatterns = [
                             pat 16
                                 (Range (-2147483648) 2147483647)
                                 getGPRegistersWithoutZero
                                 (asm "load-full16")
                           , pat 32
                                 (Range (-2147483648) 2147483647)
                                 getGPRegistersWithoutZero
                                 (asm "load-full32")
                           ]
         , instrProps = InstrProperties { instrCodeSize = 8
                                        , instrLatency = 2
                                        , instrIsNonCopy = False
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
      cs  = mkDataLocConstraints (map locID getGPRegistersInclZero) 1
            ++
            mkDataLocConstraints (map locID getGPRegistersWithoutZero) 2
      pat t (n, m) =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g t (n, m)) Nothing cs
          , patOutputValueNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = ASSTemplate []
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [ pat O.SExt  (16, 32)
                           , pat O.Trunc (32, 16)
                           ]
         , instrProps = InstrProperties { instrCodeSize = 0
                                        , instrLatency = 0
                                        , instrIsNonCopy = True
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
                 , patOutputValueNodes = [2]
                 , patADDUC = True
                 , patAsmStrTemplate = ASSTemplate
                                         [ ASReferenceToValueNode 2
                                         , ASVerbatim $ " = ANDi "
                                         , ASReferenceToValueNode 1
                                         , ASVerbatim ", 255"
                                         ]
                 }
             ]
         , instrProps = InstrProperties { instrCodeSize = 4
                                        , instrLatency = 1
                                        , instrIsNonCopy = True
                                        }
         }
     ]

-- | Makes a "set if equal to" comparison. The actual implementation does a
-- bitwise 'and' on the result of two 'slt' instructions.
mkEqComparison :: Instruction
mkEqComparison =
  let dt16 = mkIntTempType 16
      dt1  = mkIntTempType 1
      g    = mkSimpleCompPattern (O.CompArithOp $ O.IntOp O.Eq) dt16 dt16 dt1
      cs   = concatMap ( \(r, nid) ->
                          mkDataLocConstraints (map locID r) nid
                       )
             (zip (replicate 3 getGPRegistersWithoutZero) [1, 2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = ASSTemplate
                                      [ ASVerbatim $ "seq "
                                      , ASLocationOfDataNode 0
                                      , ASVerbatim ", "
                                      , ASLocationOfDataNode 1
                                      , ASVerbatim ", "
                                      , ASLocationOfDataNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 12
                                      , instrLatency = 3
                                      , instrIsNonCopy = True
                                      }
       }

-- | Makes a "set if less than to" comparison with an immediate.
mkSLTIComparison :: Instruction
mkSLTIComparison =
  let dt32 = mkIntTempType 32
      imm  = mkIntConstType (Range (-32768) 32767) 16
      dt1  = mkIntTempType 1
      g    = mkSimpleCompPattern (O.CompArithOp $ O.IntOp O.LT) dt32 imm dt1
      cs   = mkDataLocConstraints (map locID getGPRegistersInclZero) 1
             ++
             mkDataLocConstraints (map locID getGPRegistersWithoutZero) 3
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputValueNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = ASSTemplate
                                      [ ASReferenceToValueNode 3
                                      , ASVerbatim " = SLTi "
                                      , ASReferenceToValueNode 1
                                      , ASVerbatim ", "
                                      , ASIntConstOfValueNode 2
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
    , ("MUL",  O.SIntOp O.Mul)
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
              (mkDataImmDataAsmStr $ fst a)
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
              (mkDataImmDataAsmStr $ fst a)
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
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataAsmStr $ fst a)
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
              (mkDataImmDataAsmStr $ fst a)
              (O.CompArithOp $ snd a)
              getGPRegistersInclZero
              getGPRegistersWithoutZero
              (Range 0 65535)
              8
              16
    )
    [ ("xori", O.IntOp O.XOr) ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (mkDataImmDataAsmStr $ fst a)
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
              [ ASLocationOfDataNode 3
              , ASVerbatim $ " = "
              , ASVerbatim $ "NOR "
              , ASLocationOfDataNode 1
              , ASVerbatim ", "
              , ASVerbatim getZeroRegName
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
              [ ASLocationOfDataNode 3
              , ASVerbatim " = "
              , ASVerbatim "SUBu "
              , ASVerbatim getZeroRegName
              , ASVerbatim ", "
              , ASLocationOfDataNode 2
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
  -- TODO: implement
  undefined

-- | Constructs the target machine data for ordinary MIPS.
tmMips32 :: TargetMachine
tmMips32 = TargetMachine
             { tmID = toTargetMachineID "mips32"
             , tmInstructions = fixInstrIDs mkInstructions
             , tmLocations = getAllLocations
             }

-- | Constructs the target machine data for fancy MIPS.
tmFancyMips32 :: TargetMachine
tmFancyMips32 = TargetMachine
                  { tmID = toTargetMachineID "fmips32"
                  , tmInstructions = fixInstrIDs mkInstructionsInclFancy
                  , tmLocations = getAllLocationsInclFancy
                  }
