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
  ( tmMips32 )
where

import Language.InstrSel.Constraints.ConstraintBuilder
import qualified Language.InstrSel.DataTypes as D
import Language.InstrSel.Graphs
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpTypes as O
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.Targets.Generic
import Language.InstrSel.Utils
  ( Natural
  , Range (..)
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

-- | Creates the list of general-purpose registers, but there are no guarantees
-- that the location IDs will be correctly set!
mkGPRegisters :: [Location]
mkGPRegisters =
  map
    ( \i -> Location
              { locID = 0
              , locName = LocationName $ regPrefix ++ show i
              }
    )
    ([0..31] :: [Integer]) -- Cast needed to prevent compilation warning

-- | Creates the list of 'HILO' registers (which are actually memory
-- locations). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkHILORegisters :: [Location]
mkHILORegisters =
  [ Location { locID = 0, locName = LocationName "HILO" }
  , mkHIRegister
  , mkLORegister
  ]

-- | Creates the list of 'HI' register (which is actually a memory
-- location). Note that there are no guarantees that the location IDs will be
-- correctly set!
mkHIRegister :: Location
mkHIRegister = Location { locID = 0, locName = LocationName "HI" }

-- | Creates the list of 'LO' register (which is actually a memory
-- location). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkLORegister :: Location
mkLORegister = Location { locID = 0, locName = LocationName "LO" }

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

-- | Retrieves all general-purpose registers, where the location IDs have been
-- set such that every location is given a unique ID.
getGPRegisters :: [Location]
getGPRegisters =
  map
    (\i -> getRegisterByName $ LocationName $ regPrefix ++ show i)
    ([0..31] :: [Integer]) -- Cast needed to prevent compilation warning


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
-- takes two data nodes as input, and produces another data node as output.
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
           , ( 1, NodeLabel 1 (mkDataNode src1) )
           , ( 2, NodeLabel 2 (mkDataNode src2) )
           , ( 3, NodeLabel 3 (mkDataNode  dst) )
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
-- data node as input, and produces another data node as output.
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
           , ( 1, NodeLabel 1 (mkDataNode src) )
           , ( 2, NodeLabel 2 (mkDataNode dst) )
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
-- that takes two data nodes as input, and produces another data node as output.
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
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = AssemblyStringTemplate
                                      [ ASVerbatim $ str ++ " "
                                      , ASLocationOfDataNode 0
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 1
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- All data are assumed to be N bits in size.
mkSimpleNBitRegRegCompInst
  :: Natural
     -- ^ The width of the data
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
-- that takes two data nodes as input, and produces another data node as output.
-- The input operands are assumed to be 32 bits in size, and the result is
-- assumed to be 1 bit in size.
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

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- The first input operand and result are assumed to reside in one of the 32
-- general-purpose registers, and the second input operand is assumed to be a
-- N-bit immediate of a given range.
mkSimpleNBitRegMBitImmCompInst
  :: String
     -- ^ The assembly string corresponding to this instruction.
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
      dtM  = D.IntConstType { D.intConstValue = imm
                            , D.intConstNumBits = Just m
                            }
      g = mkSimpleCompPattern op dtN dtM dtN
      cs = concatMap ( \(r, nid) ->
                       mkDataLocConstraints (map locID r) nid
                     )
                     (zip [r1, r3] [1, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = AssemblyStringTemplate
                                      [ ASVerbatim $ str ++ " "
                                      , ASLocationOfDataNode 3
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 1
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- The second input operand and result are assumed to reside in one of the 32
-- general-purpose registers, and the first input operand is assumed to be a
-- N-bit immediate of a given range.
mkSimpleNBitRegMBitFirstImmCompInst
  :: String
     -- ^ The assembly string corresponding to this instruction.
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
      dtM  = D.IntConstType { D.intConstValue = imm
                            , D.intConstNumBits = Just m
                            }
      g = mkSimpleCompPattern op dtM dtN dtN
      cs = concatMap ( \(r, nid) ->
                       mkDataLocConstraints (map locID r) nid
                     )
                     (zip [r2, r3] [2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g Nothing cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAsmStrTemplate = AssemblyStringTemplate
                                      [ ASVerbatim $ str ++ " "
                                      , ASLocationOfDataNode 3
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 1
                                      , ASVerbatim ","
                                      , ASLocationOfDataNode 2
                                      ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
       }

-- | Creates a conditional branch pattern for a given comparison operator. The
-- first and second operands are the (32-bit) data nodes with IDs 5 and 6,
-- respectively, and the 'true' and 'false' labels are the label node with IDs 2
-- and 3, respectively. The returned value contains the graph and the ID of the
-- entry label node.
mkCondBrPattern :: O.CompOp -> (Graph, NodeID)
mkCondBrPattern op =
  let mkLabelNode = LabelNode $ BlockName ""
      mkCompNode = ComputationNode { compOp = op }
      mk32BitDataNode = DataNode (mkIntTempType 32) Nothing
  in ( mkGraph
         ( map
             Node
             [ ( 0, NodeLabel 0 (ControlNode O.CondBr) )
             , ( 1, NodeLabel 1 mkLabelNode )
             , ( 2, NodeLabel 2 mkLabelNode )
             , ( 3, NodeLabel 3 mkLabelNode )
             , ( 4, NodeLabel 4 mkCompNode )
             , ( 5, NodeLabel 5 mk32BitDataNode )
             , ( 6, NodeLabel 6 mk32BitDataNode )
             , ( 7, NodeLabel 7 (DataNode (mkIntTempType 1) Nothing) )
             ]
         )
         ( map
             Edge
             [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
             , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
             , ( 0, 3, EdgeLabel ControlFlowEdge 1 0 )
             , ( 7, 0, EdgeLabel DataFlowEdge 0 0 )
             , ( 5, 4, EdgeLabel DataFlowEdge 0 0 )
             , ( 6, 4, EdgeLabel DataFlowEdge 0 1 )
             , ( 4, 7, EdgeLabel DataFlowEdge 0 0 )
             ]
         )
     , 1
     )

-- | Makes two conditional branch instructions: an ordinary branch instruction,
-- and its inverse branch instruction. The inverse is achieved by inverting the
-- comparison, and swapping the branch labels, which means both instructions
-- carry the same semantics. Both are needed to handle cases where a comparison
-- needs to be inverted in order to achieve a valid block ordering.
mkCondBrInstrs
  :: String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The comparison corresponding to this instruction.
  -> String
     -- ^ The inverse assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The inverse comparison corresponding to this instruction.
  -> Instruction
mkCondBrInstrs ord_str ord_op inv_str inv_op =
  let (ord_g, ord_entry) = mkCondBrPattern ord_op
      (inv_g, inv_entry) = mkCondBrPattern inv_op
      ord_cs = mkMatchToBlockMovementConstraints ord_g
               ++
               mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 3
      inv_cs = mkMatchToBlockMovementConstraints inv_g
               ++
               mkNoDataReuseConstraints 7
               ++
               mkFallThroughConstraints 2
      ord_pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure ord_g (Just ord_entry) ord_cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim $ ord_str ++ " "
                                  , ASLocationOfDataNode 5
                                  , ASVerbatim ","
                                  , ASLocationOfDataNode 6
                                  , ASVerbatim ","
                                  , ASBlockOfLabelNode 2
                                  ]
          }
      inv_pat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure inv_g (Just inv_entry) inv_cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim $ inv_str ++ " "
                                  , ASLocationOfDataNode 5
                                  , ASVerbatim ","
                                  , ASLocationOfDataNode 6
                                  , ASVerbatim ","
                                  , ASBlockOfLabelNode 3
                                  ]
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [ord_pat, inv_pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
       }

-- | Makes the unconditional branch instructions.
mkBrInstrs :: [Instruction]
mkBrInstrs =
  let mkLabelNode = LabelNode $ BlockName ""
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Br) )
                , ( 1, NodeLabel 1 mkLabelNode )
                , ( 2, NodeLabel 2 mkLabelNode )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 0, 2, EdgeLabel ControlFlowEdge 0 0 )
                ]
            )
      cs = mkMatchToBlockMovementConstraints g
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g (Just 1) cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim "j "
                                  , ASBlockOfLabelNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Makes the return instructions.
mkRetInstrs :: [Instruction]
mkRetInstrs =
  let g n = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Ret) )
                , ( 1, NodeLabel 1 (LabelNode $ BlockName "") )
                , ( 2, NodeLabel 2 (DataNode (mkIntTempType n) Nothing) )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      bb_cs n = mkMatchToBlockMovementConstraints (g n)
      reg_cs  = mkDataLocConstraints [locID getRetRegister] 2
      pat n =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure (g n) (Just 1) (bb_cs n ++ reg_cs)
          , patOutputDataNodes = []
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate [ ASVerbatim "jr $31" ]
          }
  in [ Instruction
         { instrID = 0
           -- TODO: model 16-bits ret, properly, sometimes shifts are needed
           -- (see gsm.add.gsm_abs)
         , instrPatterns = [pat 16, pat 32]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Makes the 'mfhi' instruction.
mkMfhiInstrs :: [Instruction]
mkMfhiInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints [locID getHIRegister] 1
           ++
           mkDataLocConstraints (map locID getGPRegisters) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputDataNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim "mfhi "
                                  , ASLocationOfDataNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Makes the 'mflo' instruction.
mkMfloInstrs :: [Instruction]
mkMfloInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints [locID getLORegister] 1
           ++
           mkDataLocConstraints (map locID getGPRegisters) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputDataNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim "mflo "
                                  , ASLocationOfDataNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Makes the 'move' instruction.
mkPseudoMoveInstrs :: [Instruction]
mkPseudoMoveInstrs =
  let g = mkSimpleCopy32Pattern
      cs = mkDataLocConstraints (map locID getGPRegisters) 1
           ++
           mkDataLocConstraints (map locID getGPRegisters) 2
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g Nothing cs
          , patOutputDataNodes = [2]
          , patADDUC = True
          , patAsmStrTemplate = AssemblyStringTemplate
                                  [ ASVerbatim "move "
                                  , ASLocationOfDataNode 1
                                  , ASVerbatim ","
                                  , ASLocationOfDataNode 2
                                  ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
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

-- | Creates the list of MIPS instructions. Note that the instruction ID will be
-- (incorrectly) set to 0 for all instructions.
mkInstructions :: [Instruction]
mkInstructions =
  mkGenericPhiInstructions
  ++
  mkGenericBrFallThroughInstructions
  ++
  mkGenericEntityDefInstructions
  ++
  mkGenericCopyInstructions
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              getGPRegisters
    )
    [ ("add" , O.SIntOp O.Add)
    , ("addu", O.UIntOp O.Add)
    , ("sub" , O.SIntOp O.Sub)
    , ("subu", O.UIntOp O.Sub)
    , ("sllv", O.IntOp O.Shl)
    , ("srlv", O.IntOp O.LShr)
    , ("srav", O.IntOp O.AShr)
    ]
  ++ [
      mkSimpleNBitRegRegCompInst
              1
              "and"
              (O.CompArithOp $ O.IntOp O.And)
              getGPRegisters
              getGPRegisters
              getGPRegisters
     ]
  ++
  [ mkSimple32BitRegRegCompInst
              "rem"
              (O.CompArithOp $ O.SIntOp O.Rem)
              getGPRegisters
              getGPRegisters
              [getHIRegister]
  ]
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              [getLORegister]
    )
    [ ("mul" , O.SIntOp O.Mul)
    , ("div" , O.SIntOp O.Div)
    ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              (Range (-32768) 32767)
              32
              16
    )
    [ ("addi", O.SIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              (Range 0 65535)
              32
              16
    )
    [ ("addiu", O.UIntOp O.Add),
      ("andi",  O.IntOp O.And),
      ("ori",   O.IntOp O.Or)
    ]
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              [getRegisterByName $ LocationName "LO"]
    )
    [ ("mult" , O.SIntOp O.Mul)
    , ("multu", O.UIntOp O.Mul)
    ]
  ++
  map
    ( \a -> mkSimple32BitRegs1BitResultCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              getGPRegisters
    )
    [ ("and", O.IntOp O.And)
    , ("or" , O.IntOp O.Or)
    , ("xor", O.IntOp O.XOr)
    , ("slt", O.IntOp O.LT)
    ]
  ++
  map
    ( \(s1, op1, s2, op2) -> mkCondBrInstrs
                               s1
                               (O.CompArithOp op1)
                               s2
                               (O.CompArithOp op2)
    )
    [ ("bgt", O.SIntOp O.GT, "ble", O.SIntOp O.LE)
    , ("blt", O.SIntOp O.LT, "bge", O.SIntOp O.GE)
    , ("bge", O.SIntOp O.GE, "blt", O.SIntOp O.LT)
    , ("ble", O.SIntOp O.LE, "bgt", O.SIntOp O.GT)
    , ("beq", O.IntOp  O.Eq,  "bne", O.IntOp  O.NEq)
    , ("bne", O.IntOp  O.NEq, "beq", O.IntOp  O.Eq)
    ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              (Range 0 32)
              32
              5
    )
    [ ("sll", O.IntOp O.Shl)
    , ("srl", O.IntOp O.LShr)
    , ("sra", O.IntOp O.AShr)
    ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              (Range 0 65535)
              8
              16
    )
    [ ("xori", O.IntOp O.XOr) ]
  ++
  map
    ( \a -> mkSimpleNBitRegMBitImmCompInst
              (fst a)
              (O.CompArithOp $ snd a)
              getGPRegisters
              getGPRegisters
              (Range 0 65535)
              16
              16
    )
    [ ("sra", O.IntOp O.AShr) ]
  ++
  map
    ( \n -> mkSimpleNBitRegMBitImmCompInst
              "norz"
              (O.CompArithOp $ O.IntOp O.XOr)
              getGPRegisters
              getGPRegisters
              (Range (-1) (-1))
              n
              16
    )
    [ 8, 32 ]
  ++
    [ mkSimpleNBitRegMBitFirstImmCompInst
              "subuz"
              (O.CompArithOp $ O.UIntOp O.Sub)
              getGPRegisters
              getGPRegisters
              (Range 0 0)
              16
              16
    ]
  ++
  mkBrInstrs
  ++
  mkRetInstrs
  ++
  mkMoveInstrs

-- | Constructs the target machine data.
tmMips32 :: TargetMachine
tmMips32 = TargetMachine
             { tmID = toTargetMachineID "mips32"
             , tmInstructions = fixInstrIDs mkInstructions
             , tmLocations = getAllLocations
             }
