--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachines.Targets.Mips32
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
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

module Language.InstSel.TargetMachines.Targets.Mips32
  ( tmMips32 )
where

import Language.InstSel.Constraints.ConstraintBuilder
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs
import qualified Language.InstSel.OpStructures as OS
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachines
import Language.InstSel.TargetMachines.Targets.Generic
import Language.InstSel.Utils
  ( Range (..) )



-------------
-- Functions
-------------

-- | Creates all register classes, but there are no guarantees that the register
-- IDs will be correctly set!
mkRegClasses :: [Register]
mkRegClasses = mkGPRegisters ++ mkHILORegisters

regPrefix :: String
regPrefix = "$"

-- | Creates the list of general-purpose registers, but there are no guarantees
-- that the register IDs will be correctly set!
mkGPRegisters :: [Register]
mkGPRegisters =
  map
    ( \i -> Register
              { regID = 0
              , regName = RegisterName $ regPrefix ++ show i
              }
    )
    ([0..31] :: [Integer]) -- Cast needed to prevent compilation warning

-- | Creates the list of 'HILO' registers (which are actually memory
-- locations). Note that there are no guarantees that the register IDs will be
-- correctly set!
mkHILORegisters :: [Register]
mkHILORegisters =
  [ Register { regID = 0, regName = RegisterName "HILO" }
  , Register { regID = 0, regName = RegisterName "HI" }
  , Register { regID = 0, regName = RegisterName "LO" }
  ]

-- | Retrieves the register with a given register name. It is assumed that there
-- will exist exactly one such register.
getRegisterByName :: RegisterName -> Register
getRegisterByName rname =
  let regs = getAllRegisters
      found = filter (\r -> rname == regName r) regs
  in head found

-- | Retrieves all registers, where the register IDs have been set such that
-- every register is given a unique ID.
getAllRegisters :: [Register]
getAllRegisters = fixRegIDs $ mkRegClasses

-- | Retrieves all general-purpose registers, where the register IDs have been
-- set such that every register is given a unique ID.
getGPRegisters :: [Register]
getGPRegisters =
  map
    (\i -> getRegisterByName $ RegisterName $ regPrefix ++ show i)
    ([0..31] :: [Integer]) -- Cast needed to prevent compilation warning


-- | Retrieves the general-purpose register that serves as the return register.
-- The register ID will be correctly set.
getRetRegister :: Register
getRetRegister = getRegisterByName $ RegisterName $ regPrefix ++ "31"

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
      mkDataNode dt = DataNode { dataType = dt, dataOrigin = Nothing }
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
  -> [Register]
     -- ^ The register class of the first operand.
  -> [Register]
     -- ^ The register class of the second.
  -> [Register]
     -- ^ The register class of the destination.
  -> Instruction
mkGenericSimpleRegRegCompInst str op d1 d2 d3 r1 r2 r3 =
  let g = mkSimpleCompPattern op d1 d2 d3
      cs = concatMap
             ( \(r, nid) ->
                 mkRegAllocConstraints (map regID r) nid
             )
             (zip [r1, r2, r3] [1, 2, 3])
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAssemblyStr = AssemblyString
                                   [ ASVerbatim $ str ++ " "
                                   , ASRegisterOfDataNode 0
                                   , ASVerbatim ","
                                   , ASRegisterOfDataNode 1
                                   , ASVerbatim ","
                                   , ASRegisterOfDataNode 2
                                   ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- All data are assumed to be 32 bits in size.
mkSimple32BitRegRegCompInst
  :: String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Register]
     -- ^ The register class of the first operand.
  -> [Register]
     -- ^ The register class of the second.
  -> [Register]
     -- ^ The register class of the destination.
  -> Instruction
mkSimple32BitRegRegCompInst str op r1 r2 r3 =
  let dt = D.IntType 32
  in mkGenericSimpleRegRegCompInst str op dt dt dt r1 r2 r3

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- The input operands are assumed to be 32 bits in size, and the result is
-- assumed to be 1 bit in size.
mkSimple32BitRegs1BitResultCompInst
  :: String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Register]
     -- ^ The register class of the first operand.
  -> [Register]
     -- ^ The register class of the second.
  -> [Register]
     -- ^ The register class of the destination.
  -> Instruction
mkSimple32BitRegs1BitResultCompInst str op r1 r2 r3 =
  let dt32 = D.IntType 32
      dt1  = D.IntType  1
  in mkGenericSimpleRegRegCompInst str op dt32 dt32 dt1 r1 r2 r3

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- The first input operand and result are assumed to reside in one of the 32
-- general-purpose registers, and the second input operand is assumed to be a
-- 16-bit immediate of a given range.
mkSimple32BitReg16BitImmCompInst
  :: String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> [Register]
     -- ^ The register class of the first operand.
  -> [Register]
     -- ^ The register class of the destination.
  -> Range Integer
     -- ^ The range of the immediate (which is the second operand).
  -> Instruction
mkSimple32BitReg16BitImmCompInst str op r1 r3 imm =
  let g = mkSimpleCompPattern op (D.IntType 32) (D.IntType 16) (D.IntType 32)
      reg_cs = concatMap
               ( \(r, nid) ->
                   mkRegAllocConstraints (map regID r) nid
               )
               (zip [r1, r3] [1, 3])
      imm_cs = mkIntRangeConstraints 2 imm
      cs = reg_cs ++ imm_cs
      pat = InstrPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAssemblyStr = AssemblyString
                                   [ ASVerbatim $ str ++ " "
                                   , ASRegisterOfDataNode 3
                                   , ASVerbatim ","
                                   , ASRegisterOfDataNode 1
                                   , ASVerbatim ","
                                   , ASRegisterOfDataNode 2
                                   ]
              }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 1 }
       }

-- | Creates a simple conditional branch pattern.
mkSimpleCondBrPattern :: Graph
mkSimpleCondBrPattern =
  let mkLabelNode = LabelNode $ BasicBlockLabel ""
  in mkGraph
       ( map
           Node
           [ ( 0, NodeLabel 0 (ControlNode O.CondBranch) )
           , ( 1, NodeLabel 1 mkLabelNode )
           , ( 2, NodeLabel 2 mkLabelNode )
           , ( 3, NodeLabel 3 mkLabelNode )
           , ( 4, NodeLabel 4 (DataNode (D.IntType 1) Nothing) )
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

-- | Makes the conditional branch instructions.
mkCondBrInstrs :: [Instruction]
mkCondBrInstrs =
  let simple_g = mkSimpleCondBrPattern
      bb_alloc_cs = mkBBAllocConstraints simple_g
      fallthrough_cs = mkFallthroughConstraints 3
      cs = bb_alloc_cs ++ fallthrough_cs
      mkCompNode = ComputationNode { compOp = O.CompArithOp $ O.IntOp O.Eq }
      mk32BitDataNode = DataNode (D.IntType 32) Nothing
      complex_g =
        mkGraph
          ( getAllNodes simple_g
            ++
            map
              Node
              [ ( 5, NodeLabel 5 mkCompNode )
              , ( 6, NodeLabel 6 mk32BitDataNode )
              , ( 7, NodeLabel 7 mk32BitDataNode )
              ]
          )
          ( getAllEdges simple_g
            ++
            map
              Edge
              [ ( 5, 4, EdgeLabel DataFlowEdge 0 0 )
              , ( 6, 5, EdgeLabel DataFlowEdge 0 0 )
              , ( 7, 5, EdgeLabel DataFlowEdge 0 1 )
              ]
          )
      simple_pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure simple_g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString
                               [ ASVerbatim "beq "
                               , ASRegisterOfDataNode 6
                               , ASVerbatim ","
                               , ASRegisterOfDataNode 7
                               , ASVerbatim ","
                               , ASBBLabelOfLabelNode 2
                               ]
          }
      complex_pat =
        InstrPattern
          { patID = 1
          , patOS = OS.OpStructure complex_g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString
                               [ ASVerbatim "bne "
                               , ASRegisterOfDataNode 4
                               , ASVerbatim ",$0,"
                               , ASBBLabelOfLabelNode 2
                               ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [simple_pat, complex_pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Makes the conditional branch pseudoinstructions.
mkCondBrPseudoInstrs
  :: String
     -- ^ The assembly string corresponding to this instruction.
  -> O.CompOp
     -- ^ The operation corresponding to this instruction.
  -> Instruction
mkCondBrPseudoInstrs str op =
  let simple_g = mkSimpleCondBrPattern
      mkCompNode = ComputationNode { compOp = op }
      mk32BitDataNode = DataNode (D.IntType 32) Nothing
      complex_g =
        mkGraph
          ( getAllNodes simple_g
            ++
            map
              Node
              [ ( 5, NodeLabel 5 mkCompNode )
              , ( 6, NodeLabel 6 mk32BitDataNode )
              , ( 7, NodeLabel 7 mk32BitDataNode )
              ]
          )
          ( getAllEdges simple_g
            ++
            map
              Edge
              [ ( 5, 4, EdgeLabel DataFlowEdge 0 0 )
              , ( 6, 5, EdgeLabel DataFlowEdge 0 0 )
              , ( 7, 5, EdgeLabel DataFlowEdge 0 1 )
              ]
          )
      bb_alloc_cs = mkBBAllocConstraints simple_g
      fallthrough_cs = mkFallthroughConstraints 3
      cs = bb_alloc_cs ++ fallthrough_cs
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure complex_g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString
                               [ ASVerbatim $ str ++ " "
                               , ASRegisterOfDataNode 6
                               , ASVerbatim ","
                               , ASRegisterOfDataNode 7
                               , ASVerbatim ","
                               , ASBBLabelOfLabelNode 2
                               ]
          }
  in Instruction
       { instrID = 0
       , instrPatterns = [pat]
       , instrProps = InstrProperties { instrCodeSize = 8, instrLatency = 3 }
       }

-- | Makes the unconditional branch instructions.
mkBrInstrs :: [Instruction]
mkBrInstrs =
  let mkLabelNode = LabelNode $ BasicBlockLabel ""
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.CondBranch) )
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
      cs = mkBBAllocConstraints g
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString
                               [ ASVerbatim "j "
                                , ASBBLabelOfLabelNode 2
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
  let g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 (ControlNode O.Ret) )
                , ( 1, NodeLabel 1 (LabelNode $ BasicBlockLabel "") )
                , ( 2, NodeLabel 2 (DataNode (D.IntType 32) Nothing) )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel ControlFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      bb_cs = mkBBAllocConstraints g
      reg_cs = mkRegAllocConstraints [regID $ getRetRegister] 2
      cs = bb_cs ++ reg_cs
      pat =
        InstrPattern
          { patID = 0
          , patOS = OS.OpStructure g cs
          , patOutputDataNodes = []
          , patADDUC = True
          , patAssemblyStr = AssemblyString
                               [ ASVerbatim "j "
                                , ASBBLabelOfLabelNode 2
                                ]
          }
  in [ Instruction
         { instrID = 0
         , instrPatterns = [pat]
         , instrProps = InstrProperties { instrCodeSize = 4, instrLatency = 2 }
         }
     ]

-- | Creates the list of MIPS instructions. Note that the instruction ID will be
-- (incorrectly) set to 0 for all instructions.
mkInstructions :: [Instruction]
mkInstructions =
  mkGenericPhiInstructions
  ++
  mkGenericBrFallthroughInstructions
  ++
  mkGenericEntityDefInstructions
  ++
  mkGenericCopyInstructions
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (snd a)
              getGPRegisters
              getGPRegisters
              getGPRegisters
    )
    [ ("add" , O.CompArithOp $ O.SIntOp O.Add)
    , ("addu", O.CompArithOp $ O.UIntOp O.Add)
    , ("sub" , O.CompArithOp $ O.SIntOp O.Sub)
    , ("subu", O.CompArithOp $ O.UIntOp O.Sub)
    , ("mul" , O.CompArithOp $ O.SIntOp O.Mul)
    ]
  ++
  map
    ( \a -> mkSimple32BitReg16BitImmCompInst
              (fst a)
              (snd a)
              getGPRegisters
              getGPRegisters
              (Range (-32768) 32767)
    )
    [ ("addi", O.CompArithOp $ O.SIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimple32BitReg16BitImmCompInst
              (fst a)
              (snd a)
              getGPRegisters
              getGPRegisters
              (Range 0 65535)
    )
    [ ("addiu", O.CompArithOp $ O.UIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimple32BitRegRegCompInst
              (fst a)
              (snd a)
              getGPRegisters
              getGPRegisters
              [getRegisterByName $ RegisterName "LO"]
    )
    [ ("mult" , O.CompArithOp $ O.SIntOp O.Mul)
    , ("multu", O.CompArithOp $ O.UIntOp O.Mul)
    ]
  ++
  map
    ( \a -> mkSimple32BitRegs1BitResultCompInst
              (fst a)
              (snd a)
              getGPRegisters
              getGPRegisters
              getGPRegisters
    )
    [ ("and", O.CompArithOp $ O.IntOp O.And)
    , ("or" , O.CompArithOp $ O.IntOp O.Or)
    , ("xor", O.CompArithOp $ O.IntOp O.XOr)
    , ("slt", O.CompArithOp $ O.IntOp O.LT)
    ]
  ++
  mkCondBrInstrs
  ++
  map
    (\a -> mkCondBrPseudoInstrs (fst a) (snd a))
    [ ("bgt", O.CompArithOp $ O.IntOp O.GT)
    , ("blt", O.CompArithOp $ O.IntOp O.LT)
    , ("bge", O.CompArithOp $ O.IntOp O.GE)
    , ("ble", O.CompArithOp $ O.IntOp O.LE)
    ]
  ++
  mkBrInstrs
  ++
  mkRetInstrs

-- | Constructs the target machine data.
tmMips32 :: TargetMachine
tmMips32 = TargetMachine
             { tmID = toTargetMachineID "mips32"
             , tmInstructions = fixInstrIDs mkInstructions
             , tmRegisters = getAllRegisters
             }
