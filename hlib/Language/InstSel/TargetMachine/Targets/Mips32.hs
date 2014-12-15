--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.TargetMachine.Targets.Mips32
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

module Language.InstSel.TargetMachine.Targets.Mips32
  ( tmMips32 )
where

import Language.InstSel.Constraints.PCBuilder
import qualified Language.InstSel.DataTypes as D
import Language.InstSel.Graphs
import qualified Language.InstSel.OpStructures as OS
import qualified Language.InstSel.OpTypes as O
import Language.InstSel.TargetMachine
import Language.InstSel.Utils
  ( Range (..) )



-------------
-- Functions
-------------

-- | Creates all register classes. The register IDs will be correctly set such
-- that every registers gets a unique ID.
mkRegClasses :: [Register]
mkRegClasses = mkGPRegisters ++ mkHILORegister ++ mkHIRegister ++ mkLORegister

-- | Creates the list of general-purpose registers. The register IDs will be
-- correctly set such that every register gets a unique ID.
mkGPRegisters :: [Register]
mkGPRegisters =
  map
    ( \i -> Register
              { regID = toRegisterID i
              , regName = RegisterName $ "$" ++ show i
              }
    )
    ( [0..31] :: [Integer] ) -- Cast needed to prevent compilation warning

-- | Creates a list consisting only of the compound register 'HILO' (which is
-- actually a memory location). The register ID will be correctly set such that
-- every register gets a unique ID.
mkHILORegister :: [Register]
mkHILORegister = [Register { regID = 32, regName = RegisterName "HILO" }]

-- | Creates a list consisting only of the register component 'HI' (which is
-- actually a memory location). The register ID will be correctly set such that
-- every register gets a unique ID.
mkHIRegister :: [Register]
mkHIRegister = [Register { regID = 33, regName = RegisterName "HI" }]

-- | Creates a list consisting only of the register component 'LO' (which is
-- actually a memory location). The register ID will be correctly set such that
-- every register gets a unique ID.
mkLORegister :: [Register]
mkLORegister = [Register { regID = 34, regName = RegisterName "LO" }]

-- | Creates a simple pattern that consists of a single computation node, which
-- takes two data nodes as input, and produces another data node as output.
mkSimplePattern ::
     O.CompOp
     -- ^ The computation operation.
  -> D.DataType
     -- ^ The data type of the first operand.
  -> D.DataType
     -- ^ The data type of the second operand.
  -> D.DataType
     -- ^ The data type of the result.
  -> Graph
mkSimplePattern op src1 src2 dst =
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
-- All data are assumed to reside in one of the 32 general-purpose registers.
mkSimpleRegRegCompInst ::
     String
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
mkSimpleRegRegCompInst str op r1 r2 r3 =
  let g = mkSimplePattern op (D.IntType 32) (D.IntType 32) (D.IntType 32)
      cs = concatMap
             ( \(r, nid) ->
                 mkRegAllocConstraints (map regID r) nid
             )
             (zip [r1, r2, r3] [1, 2, 3])
      pat = InstPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAssIDMaps = [3, 1, 2]
              }
  in Instruction
       { instID = 0
       , instPatterns = [pat]
       , instProps = ( InstProperties { instCodeSize = 4, instLatency = 1 } )
       , instAssemblyStr = ( AssemblyString
                               [ AssemblyVerbatim (str ++ " ")
                               , AssemblyRegister 0
                               , AssemblyVerbatim ","
                               , AssemblyRegister 1
                               , AssemblyVerbatim ","
                               , AssemblyRegister 2
                               ]
                           )
       }

-- | Creates an instruction that consists of only a single computation node,
-- that takes two data nodes as input, and produces another data node as output.
-- The first input operand and result are assumed to reside in one of the 32
-- general-purpose registers, and the second input operand is assumed to be a
-- 16-bit immediate of a given range.
mkSimpleRegImmCompInst ::
     String
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
mkSimpleRegImmCompInst str op r1 r3 imm =
  let g = mkSimplePattern op (D.IntType 32) (D.IntType 16) (D.IntType 32)
      reg_cs = concatMap
               ( \(r, nid) ->
                   mkRegAllocConstraints (map regID r) nid
               )
               (zip [r1, r3] [1, 3])
      imm_cs = mkIntRangeConstraints 2 imm
      cs = reg_cs ++ imm_cs
      pat = InstPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = True
              , patAssIDMaps = [3, 1, 2]
              }
  in Instruction
       { instID = 0
       , instPatterns = [pat]
       , instProps = ( InstProperties { instCodeSize = 4, instLatency = 1 } )
       , instAssemblyStr = ( AssemblyString
                               [ AssemblyVerbatim (str ++ " ")
                               , AssemblyRegister 0
                               , AssemblyVerbatim ","
                               , AssemblyRegister 1
                               , AssemblyVerbatim ","
                               , AssemblyRegister 2
                               ]
                           )
       }

-- | Creates the list of MIPS instructions. Note that the instruction ID will be
-- (incorrectly) set to 0 for all instructions.
mkInstructions :: [Instruction]
mkInstructions =
  map
    ( \a -> mkSimpleRegRegCompInst
              (fst a)
              (snd a)
              mkGPRegisters
              mkGPRegisters
              mkGPRegisters
    )
    [ ("add" , O.CompArithOp $ O.SIntOp O.Add)
    , ("addu", O.CompArithOp $ O.UIntOp O.Add)
    , ("sub" , O.CompArithOp $ O.SIntOp O.Sub)
    , ("subu", O.CompArithOp $ O.UIntOp O.Sub)
    ]
  ++
  map
    ( \a -> mkSimpleRegImmCompInst
              (fst a)
              (snd a)
              mkGPRegisters
              mkGPRegisters
              (Range (-32768) 32767)
    )
    [ ("addi", O.CompArithOp $ O.SIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimpleRegImmCompInst
              (fst a)
              (snd a)
              mkGPRegisters
              mkGPRegisters
              (Range 0 65535)
    )
    [ ("addiu", O.CompArithOp $ O.UIntOp O.Add) ]
  ++
  map
    ( \a -> mkSimpleRegRegCompInst
              (fst a)
              (snd a)
              mkGPRegisters
              mkGPRegisters
              mkHILORegister
    )
    [ ("mult" , O.CompArithOp $ O.SIntOp O.Mul)
    , ("multu", O.CompArithOp $ O.UIntOp O.Mul)
    ]

-- | In order to not have to concern ourselves with instruction IDs being
-- unique, we let this function fix those for us afterwards. The function goes
-- over the list of instructions and reassigns the instruction IDs such that
-- each instruction gets a unique ID.
fixInstIDs :: [Instruction] -> [Instruction]
fixInstIDs insts =
  map ( \(new_iid, inst) -> inst { instID = new_iid } ) (zip [0..] insts)

-- | Constructs the target machine data.
tmMips32 :: TargetMachine
tmMips32 = TargetMachine
             { tmID = toTargetMachineID "mips32"
             , tmInstructions = fixInstIDs mkInstructions
             , tmRegisters = mkRegClasses
             }
