--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.OpStructures.LLVM.OSMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Builds an 'OpStructure' from a given LLVM function.
--
--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module Language.InstrSel.OpStructures.LLVM.OSMaker
  ( mkFunctionOS
  , mkPatternOS
  , toSymbolString
  )
where

import qualified Language.InstrSel.Constraints as C
  ( Constraint )
import qualified Language.InstrSel.DataTypes as D
import qualified Language.InstrSel.Graphs as G
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpStructures.Transformations as OS
import qualified Language.InstrSel.OpTypes as Op
import Language.InstrSel.PrettyShow
import qualified Language.InstrSel.Functions as F
import Language.InstrSel.Utils
  ( rangeFromSingleton
  , toNatural
  , isRight
  , fromRight
  )

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified LLVM.General.AST.Global as LLVM
  ( Global (..) )
import qualified LLVM.General.AST.IntegerPredicate as LLVMI

import Data.Maybe



--------------
-- Data types
--------------

-- | Represents a mapping from a symbol to a value node currently in the graph.
type SymToValueNodeMapping = (Symbol, G.Node)

-- | Represents a data flow that goes from a block node, identified by the given
-- ID, to an datum node. This is needed to draw the missing flow edges after
-- both the data-flow graph and the control-flow graph have been built.
type BlockToDatumDataFlow = (F.BlockName, G.Node)

-- | Represents a definition that goes from a block node, identified by the
-- given ID, an datum node. This is needed to draw the missing definition edges
-- after both the data-flow graph and the control-flow graph have been
-- built. Since the in-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the in-edge number of the data-flow edge is
-- also included in the tuple.
type BlockToDatumDef = (F.BlockName, G.Node, G.EdgeNr)

-- | Represents a definition that goes from an datum node to a block node,
-- identified by the given ID. This is needed to draw the missing definition
-- edges after both the data-flow graph and the control-flow graph have been
-- built. Since the out-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the out-edge number of the data-flow edge is
-- also included in the tuple.
type DatumToBlockDef = (G.Node, F.BlockName, G.EdgeNr)

-- | Retains various symbol names.
data Symbol
  = LocalStringSymbol String
  | GlobalStringSymbol String
  | TemporarySymbol Integer
  deriving (Show, Eq)

instance PrettyShow Symbol where
  pShow (LocalStringSymbol str) = "%" ++ str
  pShow (GlobalStringSymbol str) = "@" ++ str
  pShow (TemporarySymbol int) = "t" ++ show int

-- | Retains various constant values.
data Constant
  = IntConstant
      { intBitWidth :: Integer
        -- ^ Number of bits that represents the integer value.
      , signedIntValue :: Integer
        -- ^ The integer value. Note that this value is the signed-interpreted
        -- value of the value provided in the LLVM AST (see
        -- 'LLVMC.signedIntegerValue').
      }

  | FloatConstant { floatValue :: Float }
  | GlobalReferenceConstant { globalRefType :: D.DataType
                            , globalRefName :: Symbol
                            }
  deriving (Show, Eq)

instance PrettyShow Constant where
  pShow IntConstant { signedIntValue = v } = pShow v
  pShow FloatConstant { floatValue = v } = pShow v
  pShow GlobalReferenceConstant { globalRefName = s } = pShow s

-- | Represents the intermediate build data.
data BuildState
  = BuildState
      { opStruct :: OS.OpStructure
        -- ^ The current operation structure.
      , lastTouchedNode :: Maybe G.Node
        -- ^ The last node (if any) that was touched.
      , entryBlock :: Maybe F.BlockName
        -- ^ The block of the function entry point. A 'Nothing' value means that
        -- this value has not yet been assigned.
      , currentBlock :: Maybe F.BlockName
        -- ^ The block of the basic block currently being processed. A 'Nothing'
        -- value means that no basic block has yet been processed.
      , symMaps :: [SymToValueNodeMapping]
        -- ^ List of symbol-to-node mappings. If there are more than one mapping
        -- using the same symbol, then the last one occuring in the list should
        -- be picked.
      , blockToDatumDataFlows :: [BlockToDatumDataFlow]
        -- ^ List of block-to-datum flow dependencies that are yet to be
        -- converted into edges.
      , blockToDatumDefs :: [BlockToDatumDef]
        -- ^ List of block-to-datum definitions that are yet to be converted
        -- into edges.
      , datumToBlockDefs :: [DatumToBlockDef]
        -- ^ List of datum-to-block definitions that are yet to be converted
        -- into edges.
      , funcInputValues :: [G.Node]
        -- ^ The value nodes representing the function input arguments.
      }
  deriving (Show)

-- | Contains all the functions for building an 'OpStructure' from an AST
-- (represented as a 'LLVM.Global'). The idea is that each function will
-- recursively call the appropriate building function on every argument of the
-- part, thus traversing the entire AST and building the 'OpStructure' from the
-- bottom up.
data Builder
  = Builder
      { mkFromGlobal :: Builder -> BuildState -> LLVM.Global -> BuildState
      , mkFromBasicBlock :: Builder
                         -> BuildState
                         -> LLVM.BasicBlock
                         -> BuildState
      , mkFromNamed :: (Buildable n, Show n)
                    => Builder
                    -> BuildState
                    -> LLVM.Named n
                    -> BuildState
      , mkFromInstruction :: Builder
                          -> BuildState
                          -> LLVM.Instruction
                          -> BuildState
      , mkFromTerminator :: Builder
                         -> BuildState
                         -> LLVM.Terminator
                         -> BuildState
      , mkFromOperand :: Builder -> BuildState -> LLVM.Operand -> BuildState
      , mkFromParameter :: Builder
                        -> BuildState
                        -> LLVM.Parameter
                        -> BuildState
      }




----------------
-- Type classes
----------------

-- | Class for converting an LLVM symbol datum into a 'Symbol'.
class SymbolFormable a where
  toSymbol :: a -> Symbol

instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = LocalStringSymbol str
  toSymbol (LLVM.UnName int) = TemporarySymbol $ toInteger int

-- | Class for converting an LLVM constant datum into a 'Constant'.
class ConstantFormable a where
  toConstant :: a -> Constant

instance ConstantFormable LLVMC.Constant where
  toConstant i@(LLVMC.Int b _) =
    IntConstant { intBitWidth = fromIntegral b
                , signedIntValue = LLVMC.signedIntegerValue i
                }
  toConstant (LLVMC.GlobalReference t n) =
    GlobalReferenceConstant { globalRefType = toDataType t
                            , globalRefName = toSymbol n
                            }
  toConstant l = error $ "'toConstant' not implemented for " ++ show l

-- | Class for converting an LLVM datum into a 'DataType'.
class DataTypeFormable a where
  toDataType :: a -> D.DataType

instance DataTypeFormable Constant where
  toDataType IntConstant { intBitWidth = w, signedIntValue = v } =
    D.IntConstType { D.intConstValue = rangeFromSingleton v
                   , D.intConstNumBits = Just $ toNatural w
                   }
  toDataType GlobalReferenceConstant {} =
    -- TODO: fix so that the correct data type is applied
    D.AnyType
  toDataType c = error $ "'toDataType' not implemented for " ++ show c

instance DataTypeFormable LLVM.Type where
  toDataType (LLVM.IntegerType bits) =
    D.IntTempType { D.intTempNumBits = toNatural bits }
  toDataType (LLVM.PointerType _ _) =
    -- TODO: fix so that the correct data type is applied
    D.AnyType
  toDataType t = error $ "'toDataType' not implemented for " ++ show t

instance DataTypeFormable LLVM.Operand where
  toDataType (LLVM.LocalReference t _) = toDataType t
  toDataType (LLVM.ConstantOperand c) = toDataType (toConstant c)
  toDataType o = error $ "'toDataType' not implemented for " ++ show o

-- | Type class for helping a 'Builder' to traverse the AST. The idea is that
-- 'build' will be invoked on the current AST element, and then the element will
-- invoke the appropriate build function within the 'Builder', using itself as
-- argument. The main advantage of this is that processing lists of AST elements
-- becomes simpler as all such lists can be handled exactly the same way
-- (otherwise we require one function for processing a list of
-- 'LLVM.Instruction's and another for processing a list of 'LLVM.Parameter's).
class Buildable e where
  -- | Builds the corresponding function data-flow graph from a given LLVM
  -- element.
  build
    :: Builder
    -> BuildState
      -- ^ The current build state.
    -> e
       -- ^ The AST element to be processed.
    -> BuildState
       -- ^ The new build state.

instance (Buildable e) => Buildable [e] where
  build b st e = foldl (build b) st e

instance Buildable LLVM.Global where
  build b st e = (mkFromGlobal b) b st e

instance Buildable LLVM.BasicBlock where
  build b st e = (mkFromBasicBlock b) b st e

instance (Buildable n, Show n) => Buildable (LLVM.Named n) where
  build b st e = (mkFromNamed b) b st e

instance Buildable LLVM.Instruction where
  build b st e = (mkFromInstruction b) b st e

instance Buildable LLVM.Terminator where
  build b st e = (mkFromTerminator b) b st e

instance Buildable LLVM.Operand where
  build b st e = (mkFromOperand b) b st e

instance Buildable LLVM.Parameter where
  build b st e = (mkFromParameter b) b st e



-------------------
-- Build functions
-------------------

-- | Builds an 'OpStructure' from a function to be compiled. If the definition
-- is not a 'Function', an error is produced.
mkFunctionOS :: LLVM.Global -> OS.OpStructure
mkFunctionOS f@(LLVM.Function {}) =
  let st0 = mkInitBuildState
      st1 = build mkFunctionDFGBuilder st0 f
      st2 = build mkFunctionCFGBuilder st1 f
      st3 = updateOSEntryBlockNode
              st2
              (fromJust $ findBlockNodeWithID st2 (fromJust $ entryBlock st2))
      st4 = applyOSTransformations st3
      st5 = addMissingBlockToDatumDataFlowEdges st4
      st6 = addMissingBlockToDatumDefEdges st5
      st7 = addMissingDatumToBlockDefEdges st6
  in opStruct st7
mkFunctionOS _ = error "mkFunctionOS: not a Function"

-- | Builds an 'OpStructure' from an instruction pattern. If the definition is
-- not a 'Function', an error is produced.
mkPatternOS :: LLVM.Global -> OS.OpStructure
mkPatternOS f@(LLVM.Function {}) =
  let st0 = mkInitBuildState
      st1 = build mkPatternDFGBuilder st0 f
      st2 = build mkPatternCFGBuilder st1 f
      st3 = updateOSEntryBlockNode
              st2
              (fromJust $ findBlockNodeWithID st2 (fromJust $ entryBlock st2))
      st4 = applyOSTransformations st3
      st5 = addMissingBlockToDatumDefEdges st4
      st6 = addMissingDatumToBlockDefEdges st5
      st7 = removeUnusedBlockNodes st6
  in opStruct st7
mkPatternOS _ = error "mkPattern: not a Function"

-- | Creates an initial 'BuildState'.
mkInitBuildState :: BuildState
mkInitBuildState =
  BuildState { opStruct = OS.mkEmpty
             , lastTouchedNode = Nothing
             , entryBlock = Nothing
             , currentBlock = Nothing
             , symMaps = []
             , blockToDatumDataFlows = []
             , blockToDatumDefs = []
             , datumToBlockDefs = []
             , funcInputValues = []
             }

-- | Constructs a 'Builder' that will construct a function data-flow graph.
mkFunctionDFGBuilder :: Builder
mkFunctionDFGBuilder =
  Builder { mkFromGlobal     = mkFunctionDFGFromGlobal
          , mkFromBasicBlock = mkFunctionDFGFromBasicBlock
          , mkFromNamed      = mkFunctionDFGFromNamed
          , mkFromInstruction = mkFunctionDFGFromInstruction
          , mkFromTerminator =
              \_ _ t -> error $ "mkFromTerminator: not implemented for "
                                ++ show t
          , mkFromOperand    = mkFunctionDFGFromOperand
          , mkFromParameter  = mkFunctionDFGFromParameter
          }

-- | Constructs a 'Builder' that will construct a function control-flow graph.
mkFunctionCFGBuilder :: Builder
mkFunctionCFGBuilder =
  Builder { mkFromGlobal      = mkFunctionCFGFromGlobal
          , mkFromBasicBlock  = mkFunctionCFGFromBasicBlock
          , mkFromNamed       = mkFunctionCFGFromNamed
          , mkFromInstruction = mkFunctionCFGFromInstruction
          , mkFromTerminator  = mkFunctionCFGFromTerminator
          , mkFromOperand     = mkFunctionCFGFromOperand
          , mkFromParameter   =
              \_ _ p -> error $ "mkFromParameter: not implemented for "
                                ++ show p
          }

-- | Constructs a 'Builder' that will construct a pattern data-flow graph.
mkPatternDFGBuilder :: Builder
mkPatternDFGBuilder =
  mkFunctionDFGBuilder { mkFromInstruction = newInstrMk }
  where newInstrMk b st i@(LLVM.Call {}) =
          let call_op = LLVM.function i
          in if isRight call_op
             then case fromRight call_op
                  of (LLVM.ConstantOperand
                       (LLVMC.GlobalReference _ (LLVM.Name name)))
                       -> case name
                          of "setreg" -> mkPatternDFGFromSetregCall b st i
                             "param"  -> mkPatternDFGFromParamCall b st i
                             "return" -> st -- Will be processed in the CFG
                             _ -> -- Let the default builder handle it
                                  mkFunctionDFGFromInstruction b st i
                     _ -> -- Let the default builder handle it
                          mkFunctionDFGFromInstruction b st i
             else error "mkPatternDFGBuilder: CallableOperand is not an Operand"
        newInstrMk b st i = mkFunctionDFGFromInstruction b st i

-- | Constructs a 'Builder' that will construct a pattern control-flow graph.
mkPatternCFGBuilder :: Builder
mkPatternCFGBuilder =
  mkFunctionCFGBuilder { mkFromInstruction = newInstrMk
                       , mkFromTerminator = newTermMk
                       }
  where newInstrMk b st i@(LLVM.Call {}) =
          let call_op = LLVM.function i
          in if isRight call_op
             then case fromRight call_op
                  of (LLVM.ConstantOperand
                       (LLVMC.GlobalReference _ (LLVM.Name name)))
                       -> case name
                          of "return" -> mkPatternCFGFromReturnCall b st i
                             _ -> -- Let the default builder handle it
                                  mkFunctionCFGFromInstruction b st i
                     _ -> -- Let the default builder handle it
                          mkFunctionCFGFromInstruction b st i
             else error "mkPatternCFGBuilder: CallableOperand is not an Operand"
        newInstrMk b st i = mkFunctionCFGFromInstruction b st i
        newTermMk _ st (LLVM.Ret { LLVM.returnOperand = Nothing }) = st
        newTermMk _ _ _ =
          error "mkPatternCFGBuilder: non-void rets not allowed in patterns"

-- | Adds a 'G.ControlNode' of type 'Op.Ret'.
mkPatternCFGFromReturnCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> BuildState
mkPatternCFGFromReturnCall
  b
  st0
  (LLVM.Call { LLVM.arguments = [(LLVM.LocalReference _ arg, _)] })
  =
  let st1 = addNewNode st0 (G.ControlNode Op.Ret)
      g = getOSGraph st1
      rn = fromJust $ lastTouchedNode st1
      bn = fromJust $ findBlockNodeWithID st1 $ fromJust $ currentBlock st1
      vn = fromJust $ findValueNodeWithSym st0 $ toSymbol arg
      st2 = addNewEdge st1 G.DataFlowEdge vn rn
      st3 = addNewEdge st2 G.ControlFlowEdge bn rn
  in st3
mkPatternCFGFromReturnCall _ _ i =
  error $ "mkPatternCFGFromReturnCall: not implemented for " ++ show i



mkFunctionDFGFromGlobal
  :: Builder
  -> BuildState
  -> LLVM.Global
  -> BuildState
mkFunctionDFGFromGlobal b st0 f@(LLVM.Function {}) =
  let (params, _) = LLVM.parameters f
      st1 = build b st0 params
      st2 = build b st1 (LLVM.basicBlocks f)
  in st2
mkFunctionDFGFromGlobal _ _ g =
  error $ "mkFunctionDFGFromGlobal: not implemented for " ++ show g

mkFunctionDFGFromBasicBlock
  :: Builder
  -> BuildState
  -> LLVM.BasicBlock
  -> BuildState
mkFunctionDFGFromBasicBlock b st0 (LLVM.BasicBlock (LLVM.Name str) insts _) =
  let block_name = F.BlockName str
      st1 = if isNothing $ entryBlock st0
            then foldl (\st n -> addBlockToDatumDataFlow st (block_name, n))
                       (st0 { entryBlock = Just block_name })
                       (funcInputValues st0)
            else st0
      st2 = st1 { currentBlock = Just block_name }
      st3 = foldl (build b) st2 insts
  in st3
mkFunctionDFGFromBasicBlock _ _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "mkFunctionDFGFromBasicBlock: does not support unnamed basic blocks"

mkFunctionDFGFromNamed
  :: (Buildable n)
  => Builder
  -> BuildState
  -> (LLVM.Named n)
  -> BuildState
mkFunctionDFGFromNamed b st0 (name LLVM.:= expr) =
  let st1 = build b st0 expr
      sym = toSymbol name
      res_n = fromJust $ lastTouchedNode st1
      res_dt = G.getDataTypeOfValueNode res_n
      st2 = ensureValueNodeWithSymExists st1 sym res_dt
      sym_n = fromJust $ lastTouchedNode st2
      st3 = updateOSGraph st2 (G.mergeNodes sym_n res_n (getOSGraph st2))
      st4 = st3 { blockToDatumDefs =
                     map ( \(b', n, nr) ->
                           if res_n == n then (b', sym_n, nr) else (b', n, nr)
                         )
                         (blockToDatumDefs st3)
                }
      st5 = st4 { datumToBlockDefs =
                     map ( \(n, b', nr) ->
                           if res_n == n then (sym_n, b', nr) else (n, b', nr)
                         )
                         (datumToBlockDefs st4)
                }
  in st5
mkFunctionDFGFromNamed b st (LLVM.Do expr) = build b st expr

mkFunctionDFGFromInstruction
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> BuildState
mkFunctionDFGFromInstruction b st (LLVM.Add  nsw nuw op1 op2 _) =
  -- TODO: make use of nsw and nuw?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.Add)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FAdd _ op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.FloatOp Op.Add)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Sub  nsw nuw op1 op2 _) =
  -- TODO: make use of nsw and nuw?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.Sub)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FSub _ op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.FloatOp Op.Sub)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Mul nsw nuw op1 op2 _) =
  -- TODO: make use of nsw and nuw?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.Mul)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FMul _ op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.FloatOp Op.Mul)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.UDiv exact op1 op2 _) =
  -- TODO: make use of exact?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.UIntOp Op.Div)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.SDiv exact op1 op2 _) =
  -- TODO: make use of exact?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.SIntOp Op.Div)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FDiv _ op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.FloatOp Op.Div)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.URem op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.UIntOp Op.Rem)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.SRem op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.SIntOp Op.Rem)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FRem _ op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.FloatOp Op.Rem)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Shl nsw nuw op1 op2 _) =
  -- TODO: make use of nsw and nuw?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.Shl)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.LShr exact op1 op2 _) =
  -- TODO: make use of exact?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.LShr)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.AShr exact op1 op2 _) =
  -- TODO: make use of exact?
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.AShr)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.And op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.And)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Or op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.Or)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Xor op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (Op.CompArithOp $ Op.IntOp Op.XOr)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.ICmp p op1 op2 _) =
  -- TODO: add support for vectorized icmp
  mkFunctionDFGFromCompOp b
                          st
                          (D.IntTempType { D.intTempNumBits = 1 })
                          (fromLlvmIPred p)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FCmp p op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toTempDataType op1)
                          (fromLlvmFPred p)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Trunc op1 t1 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toDataType t1)
                          (Op.CompTypeConvOp Op.Trunc)
                          [op1]
mkFunctionDFGFromInstruction b st (LLVM.ZExt op1 t1 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toDataType t1)
                          (Op.CompTypeConvOp Op.ZExt)
                          [op1]
mkFunctionDFGFromInstruction b st (LLVM.SExt op1 t1 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (toDataType t1)
                          (Op.CompTypeConvOp Op.SExt)
                          [op1]
-- TODO: replace the 'addDatumToBlockDef' with proper dependencies from/to
-- state nodes.
mkFunctionDFGFromInstruction b st0 (LLVM.Load _ op1 _ _ _) =
  let st1 = mkFunctionDFGFromCompOp b
                                    st0
                                    (toDataType op1)
                                    (Op.CompMemoryOp Op.Load)
                                    [op1]
      n = fromJust $ lastTouchedNode st1
      bb = fromJust $ currentBlock st1
      st2 = addDatumToBlockDef st1 (n, bb, 0)
  in st2
-- TODO: replace the 'addDatumToBlockDef' with proper dependencies from/to
-- state nodes.
mkFunctionDFGFromInstruction b st0 (LLVM.Store _ op1 op2 _ _ _) =
  let st1 = mkFunctionDFGFromCompOp b
                                    st0
                                    D.AnyType -- This doesn't matter since the
                                              -- result node will be removed
                                              -- directly afterwards
                                    (Op.CompMemoryOp Op.Store)
                                    [op1, op2] -- TODO: check the order that
                                               -- it's correct TODO: remove the
                                               -- node below
      n = fromJust $ lastTouchedNode st1
  in st1
mkFunctionDFGFromInstruction b st0 (LLVM.Phi t phi_operands _) =
  let (operands, blocks) = unzip phi_operands
      block_names = map (\(LLVM.Name str) -> F.BlockName str) blocks
      operand_node_sts = scanl (build b) st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail operand_node_sts)
      st1 = last operand_node_sts
      st2 = addNewNode st1 G.PhiNode
      phi_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns phi_node
      st4 = foldl ( \st (n, block_id) ->
                    let g = getOSGraph st
                        dfe = head
                              $ filter G.isDataFlowEdge
                              $ G.getEdgesBetween g n phi_node
                    in addDatumToBlockDef st
                                           (n, block_id, G.getOutEdgeNr dfe)
                  )
                  st3
                  (zip operand_ns block_names)
      st5 = addNewNode st4 (G.ValueNode (toDataType t) Nothing)
      d_node = fromJust $ lastTouchedNode st5
      st6 = addNewEdge st5 G.DataFlowEdge phi_node d_node
      st7 = addBlockToDatumDef st6 (fromJust $ currentBlock st6, d_node, 0)
            -- Since we've just created the value node and only added a
            -- single data-flow edge to it, we are guaranteed that the in-edge
            -- number of that data-flow edge is 0.
  in st7
mkFunctionDFGFromInstruction _ _ i =
  error $ "mkFunctionDFGFromInstruction: not implemented for " ++ show i

-- | Inserts a new computation node representing the operation along with edges
-- to that computation node from the given operands (which will also be
-- processed). Lastly, a new value node representing the result will be added
-- along with an edge to that value node from the computation node.
mkFunctionDFGFromCompOp
  :: (Buildable o)
  => Builder
  -> BuildState
  -> D.DataType
     -- ^ The data type of the result.
  -> Op.CompOp
     -- ^ The computational operation.
  -> [o]
     -- ^ The operands.
  -> BuildState
mkFunctionDFGFromCompOp b st0 dt op operands =
  let sts = scanl (build b) st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ComputationNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
      st4 = addNewNode st3 (G.ValueNode dt Nothing)
      d_node = fromJust $ lastTouchedNode st4
      st5 = addNewEdge st4 G.DataFlowEdge op_node d_node
  in st5

mkFunctionDFGFromOperand
  :: Builder
  -> BuildState
  -> LLVM.Operand
  -> BuildState
mkFunctionDFGFromOperand _ st (LLVM.LocalReference t name) =
  ensureValueNodeWithSymExists st (toSymbol name) (toDataType t)
mkFunctionDFGFromOperand _ st (LLVM.ConstantOperand c) =
  addNewValueNodeWithConstant st (toConstant c)
mkFunctionDFGFromOperand _ _ o =
  error $ "mkFunctionDFGFromOperand: not implemented for " ++ show o

mkFunctionDFGFromParameter
  :: Builder
  -> BuildState
  -> LLVM.Parameter
  -> BuildState
mkFunctionDFGFromParameter _ st0 (LLVM.Parameter t name _) =
  let st1 = ensureValueNodeWithSymExists st0 (toSymbol name) (toDataType t)
      n = fromJust $ lastTouchedNode st1
      st2 = addFuncInputValue st1 n
  in st2

-- | Merges the two nodes in the function call.
mkPatternDFGFromSetregCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> BuildState
mkPatternDFGFromSetregCall
  b
  st0
  ( LLVM.Call { LLVM.arguments = [ (LLVM.LocalReference _ arg1, _)
                                 , (LLVM.LocalReference _ arg2, _)
                                 ]
              }
  )
  =
  let [n1, n2] = map (fromJust . findValueNodeWithSym st0 . toSymbol)
                     [arg1, arg2]
      g0 = getOSGraph st0
      g1 = G.mergeNodes n2 n1 g0
      g2 = G.updateOriginOfValueNode (fromJust $ G.getOriginOfValueNode n1)
                                     n2
                                     g1
      st1 = updateOSGraph st0 g2
      st2 = st1 { blockToDatumDataFlows =
                     map ( \(b', n) ->
                           if n1 == n then (b', n2) else (b', n)
                         )
                         (blockToDatumDataFlows st1)
                }
      st3 = st2 { blockToDatumDefs =
                     map ( \(b', n, nr) ->
                           if n1 == n then (b', n2, nr) else (b', n, nr)
                         )
                         (blockToDatumDefs st2)
                }
      st4 = st3 { datumToBlockDefs =
                     map ( \(n, b', nr) ->
                           if n1 == n then (n2, b', nr) else (n, b', nr)
                         )
                         (datumToBlockDefs st3)
                }
  in st4
mkPatternDFGFromSetregCall _ _ i@(LLVM.Call {}) =
  error $ "mkPatternDFGFromSetregCall: unexpected number or type of function "
          ++ "arguments in " ++ show i
mkPatternDFGFromSetregCall _ _ i =
  error $ "mkPatternDFGFromSetregCall: not implemented for " ++ show i

-- | Adds a new unnamed node with the same data type as the return type of the
-- function call.
mkPatternDFGFromParamCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> BuildState
mkPatternDFGFromParamCall b st i@(LLVM.Call {}) =
  let (LLVM.ConstantOperand (LLVMC.GlobalReference grt _)) =
        fromRight $ LLVM.function i
      (LLVM.PointerType { LLVM.pointerReferent = pt }) = grt
      (LLVM.FunctionType { LLVM.resultType = rt }) = pt
      dt = toDataType rt
  in addNewNode st (G.ValueNode dt Nothing)
mkPatternDFGFromParamCall _ _ i =
  error $ "mkPatternDFGFromParamCall: not implemented for " ++ show i

mkFunctionCFGFromGlobal :: Builder -> BuildState -> LLVM.Global -> BuildState
mkFunctionCFGFromGlobal b st f@(LLVM.Function {}) =
  build b st (LLVM.basicBlocks f)
mkFunctionCFGFromGlobal _ _ g =
  error $ "mkFunctionCFGFromGlobal: not implemented for " ++ show g

mkFunctionCFGFromBasicBlock
  :: Builder
  -> BuildState
  -> LLVM.BasicBlock
  -> BuildState
mkFunctionCFGFromBasicBlock b st0 ( LLVM.BasicBlock (LLVM.Name str)
                                                    insts
                                                    named_term_inst
                                  ) =
  let block_name = F.BlockName str
      term_inst = fromNamed named_term_inst
      st1 = if isNothing $ entryBlock st0
            then st1 { entryBlock = Just block_name }
            else st0
      st2 = ensureBlockNodeExists st1 block_name
      st3 = st2 { currentBlock = Just block_name }
      st4 = foldl (build b) st3 insts
      st5 = build b st4 term_inst
    in st5
mkFunctionCFGFromBasicBlock _ _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
  error $ "mkFunctionCFGFromBasicBlock: does not support unnamed basic blocks"

mkFunctionCFGFromInstruction
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> BuildState
mkFunctionCFGFromInstruction _ st _ = st

mkFunctionCFGFromNamed
  :: (Buildable n)
  => Builder
  -> BuildState
  -> (LLVM.Named n)
  -> BuildState
mkFunctionCFGFromNamed b st (_ LLVM.:= expr) = build b st expr
mkFunctionCFGFromNamed b st (LLVM.Do expr) = build b st expr

mkFunctionCFGFromTerminator
  :: Builder
  -> BuildState
  -> LLVM.Terminator
  -> BuildState
mkFunctionCFGFromTerminator b st (LLVM.Ret op _) =
    mkFunctionCFGFromControlOp b st Op.Ret (maybeToList op)
mkFunctionCFGFromTerminator b st0 (LLVM.Br (LLVM.Name dst) _) =
  let st1 = mkFunctionCFGFromControlOp b st0 Op.Br ([] :: [LLVM.Operand])
            -- Signature on last argument needed to please GHC...
      br_node = fromJust $ lastTouchedNode st1
      st2 = ensureBlockNodeExists st1 (F.BlockName dst)
      dst_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdge st2 G.ControlFlowEdge br_node dst_node
  in st3
mkFunctionCFGFromTerminator b st0 ( LLVM.CondBr op
                                                (LLVM.Name t_dst)
                                                (LLVM.Name f_dst)
                                                _
                                  ) =
  let st1 = mkFunctionCFGFromControlOp b st0 Op.CondBr [op]
      br_node = fromJust $ lastTouchedNode st1
      st2 = ensureBlockNodeExists st1 (F.BlockName t_dst)
      t_dst_node = fromJust $ lastTouchedNode st2
      st3 = ensureBlockNodeExists st2 (F.BlockName f_dst)
      f_dst_node = fromJust $ lastTouchedNode st3
      st4 = addNewEdgesManyDests st3
                                 G.ControlFlowEdge
                                 br_node
                                 [t_dst_node, f_dst_node]
  in st4
mkFunctionCFGFromTerminator _ _ t =
  error $ "mkFunctionCFGFromTerminator: not implemented for " ++ show t

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current block node and operands (which will also be processed).
mkFunctionCFGFromControlOp
  :: (Buildable o)
  => Builder
  -> BuildState
  -> Op.ControlOp
     -- ^ The control operations.
  -> [o]
     -- ^ The operands.
  -> BuildState
mkFunctionCFGFromControlOp b st0 op operands =
  let sts = scanl (build b) st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ControlNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdge st2
                       G.ControlFlowEdge
                       ( fromJust $
                           findBlockNodeWithID st2
                                               (fromJust $ currentBlock st2)
                       )
              op_node
      st4 = addNewEdgesManySources st3 G.DataFlowEdge operand_ns op_node
  in st4

mkFunctionCFGFromOperand :: Builder -> BuildState -> LLVM.Operand -> BuildState
mkFunctionCFGFromOperand _ st (LLVM.LocalReference t name) =
  ensureValueNodeWithSymExists st (toSymbol name) (toDataType t)
mkFunctionCFGFromOperand _ st (LLVM.ConstantOperand c) =
  addNewValueNodeWithConstant st (toConstant c)
mkFunctionCFGFromOperand _ _ o =
  error $ "mkFunctionCFGFromOperand: not implemented for " ++ show o



------------------
-- Help functions
------------------

-- | Converts a 'SymbolFormable' entity into a string. This is typically used
-- when referring to nodes whose name or origin is based on an LLVM entity (such
-- as a temporary or a variable).
toSymbolString :: (SymbolFormable s) => s -> String
toSymbolString = pShow . toSymbol

-- | Converts an argument into a temporary-oriented data type.
toTempDataType :: (DataTypeFormable t) => t -> D.DataType
toTempDataType a =
  conv $ toDataType a
  where conv d@(D.IntTempType {}) = d
        conv (D.IntConstType { D.intConstNumBits = Just b }) =
          D.IntTempType { D.intTempNumBits = b }
        conv d = error $ "toTempDataType: unexpected data type " ++ show d

-- | Gets the OS graph contained by the operation structure in a given state.
getOSGraph :: BuildState -> G.Graph
getOSGraph = OS.osGraph . opStruct

-- | Updates the OS graph contained by the operation structure in a given state.
updateOSGraph :: BuildState -> G.Graph -> BuildState
updateOSGraph st g =
  let os = opStruct st
  in st { opStruct = os { OS.osGraph = g } }

-- | Updates the OS entry block node contained by the operation structure in a
-- given state.
updateOSEntryBlockNode :: BuildState -> G.Node -> BuildState
updateOSEntryBlockNode st n =
  let os = opStruct st
  in st { opStruct = os { OS.osEntryBlockNode = Just (G.getNodeID n) } }

-- | Updates the last touched node information.
touchNode :: BuildState -> G.Node -> BuildState
touchNode st n = st { lastTouchedNode = Just n }

-- | Adds a new node into a given state.
addNewNode :: BuildState -> G.NodeType -> BuildState
addNewNode st0 nt =
  let (new_g, new_n) = G.addNewNode nt (getOSGraph st0)
      st1 = updateOSGraph st0 new_g
      st2 = touchNode st1 new_n
  in st2

-- | Adds a list of constraints to the 'OpStructure' in the given 'BuildState'.
addConstraints :: BuildState -> [C.Constraint] -> BuildState
addConstraints st cs =
  let os = opStruct st
      new_os = OS.addConstraints os cs
  in st { opStruct = new_os }

mkVarNameForConst :: Constant -> String
mkVarNameForConst c = "%const." ++ (pShow c)

-- | Adds a new value node representing a particular constant to a given state.
addNewValueNodeWithConstant :: BuildState -> Constant -> BuildState
addNewValueNodeWithConstant st0 c =
  -- TODO: fix so that each constant gets a unique variable name
  let st1 = addNewNode st0 ( G.ValueNode (toDataType c)
                                         (Just $ mkVarNameForConst c)
                           )
      new_n = fromJust $ lastTouchedNode st1
      st2 = addBlockToDatumDataFlow st1 (fromJust $ entryBlock st1, new_n)
  in st2

-- | Adds a new edge into a given state.
addNewEdge
  :: BuildState
     -- ^ The current state.
  -> G.EdgeType
  -> G.Node
     -- ^ The source node.
  -> G.Node
     -- ^ The destination node.
  -> BuildState
     -- ^ The new state.
addNewEdge st et src dst =
  let (new_g, _) = G.addNewEdge et (src, dst) (getOSGraph st)
  in updateOSGraph st new_g

-- | Adds many new edges of the same type into a given state.
addNewEdgesManySources
  :: BuildState
     -- ^ The current state.
  -> G.EdgeType
  -> [G.Node]
     -- ^ The source nodes.
  -> G.Node
     -- ^ The destination node.
  -> BuildState
     -- ^ The new state.
addNewEdgesManySources st et srcs dst =
  let es = zip srcs (repeat dst)
      f g e = fst $ G.addNewEdge et e g
  in updateOSGraph st $ foldl f (getOSGraph st) es

-- | Adds many new edges of the same type into a given state.
addNewEdgesManyDests
  :: BuildState
     -- ^ The current state.
  -> G.EdgeType
  -> G.Node
     -- ^ The source node.
  -> [G.Node]
     -- ^ The destination nodes.
  -> BuildState
     -- ^ The new state.
addNewEdgesManyDests st et src dsts =
  let es = zip (repeat src) dsts
      f g e = fst $ G.addNewEdge et e g
  in updateOSGraph st $ foldl f (getOSGraph st) es

-- | Adds a new symbol-to-node mapping to a given state.
addSymMap :: BuildState -> SymToValueNodeMapping -> BuildState
addSymMap st sm = st { symMaps = sm:(symMaps st) }

-- | Adds block-to-datum flow to a given state.
addBlockToDatumDataFlow :: BuildState -> BlockToDatumDataFlow -> BuildState
addBlockToDatumDataFlow st flow =
  st { blockToDatumDataFlows = flow:(blockToDatumDataFlows st) }

-- | Adds block-to-datum definition to a given state.
addBlockToDatumDef :: BuildState -> BlockToDatumDef -> BuildState
addBlockToDatumDef st def =
  st { blockToDatumDefs = def:(blockToDatumDefs st) }

-- | Adds datum-to-block definition to a given state.
addDatumToBlockDef :: BuildState -> DatumToBlockDef -> BuildState
addDatumToBlockDef st def =
  st { datumToBlockDefs = def:(datumToBlockDefs st) }

-- | Adds a value node representing a function argument to a given state.
addFuncInputValue :: BuildState -> G.Node -> BuildState
addFuncInputValue st n =
  st { funcInputValues = n:(funcInputValues st) }

-- | Finds the node ID (if any) of the value node to which a symbol is mapped.
findValueNodeWithSym :: BuildState -> Symbol -> Maybe G.Node
findValueNodeWithSym st sym = lookup sym (symMaps st)

-- | Gets the block node with a particular name in the graph of the given state.
-- If no such node exists, 'Nothing' is returned.
findBlockNodeWithID :: BuildState -> F.BlockName -> Maybe G.Node
findBlockNodeWithID st l =
  let block_nodes = filter G.isBlockNode $ G.getAllNodes $ getOSGraph st
      nodes_w_matching_blocks =
        filter (\n -> (G.nameOfBlock $ G.getNodeType n) == l) block_nodes
  in if length nodes_w_matching_blocks > 0
     then Just (head nodes_w_matching_blocks)
     else Nothing

-- | Checks that a value node with a particular symbol exists in the graph of
-- the given state. If it does then the last touched node is updated
-- accordingly, otherwise a new value node with the symbol is added. A
-- corresponding mapping is also added.
ensureValueNodeWithSymExists
  :: BuildState
  -> Symbol
  -> D.DataType
     -- ^ Data type to use upon creation if such a value node does not exist.
  -> BuildState
ensureValueNodeWithSymExists st0 sym dt =
  let n = findValueNodeWithSym st0 sym
  in if isJust n
     then touchNode st0 (fromJust n)
     else let st1 = addNewNode st0 (G.ValueNode dt (Just $ pShow sym))
              new_n = fromJust $ lastTouchedNode st1
              st2 = addSymMap st1 (sym, new_n)
          in st2

-- | Checks that a block node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated accordingly,
-- otherwise then a new block node is added.
ensureBlockNodeExists :: BuildState -> F.BlockName -> BuildState
ensureBlockNodeExists st l =
  let block_node = findBlockNodeWithID st l
  in if isJust block_node
     then touchNode st (fromJust block_node)
     else addNewNode st (G.BlockNode l)

-- | Converts an LLVM integer comparison op into an equivalent op of our own
-- data type.
fromLlvmIPred :: LLVMI.IntegerPredicate -> Op.CompOp
fromLlvmIPred LLVMI.EQ  = Op.CompArithOp $  Op.IntOp Op.Eq
fromLlvmIPred LLVMI.NE  = Op.CompArithOp $  Op.IntOp Op.NEq
fromLlvmIPred LLVMI.UGT = Op.CompArithOp $ Op.UIntOp Op.GT
fromLlvmIPred LLVMI.ULT = Op.CompArithOp $ Op.UIntOp Op.LT
fromLlvmIPred LLVMI.UGE = Op.CompArithOp $ Op.UIntOp Op.GE
fromLlvmIPred LLVMI.ULE = Op.CompArithOp $ Op.UIntOp Op.LE
fromLlvmIPred LLVMI.SGT = Op.CompArithOp $ Op.SIntOp Op.GT
fromLlvmIPred LLVMI.SLT = Op.CompArithOp $ Op.SIntOp Op.LT
fromLlvmIPred LLVMI.SGE = Op.CompArithOp $ Op.SIntOp Op.GE
fromLlvmIPred LLVMI.SLE = Op.CompArithOp $ Op.SIntOp Op.LE

-- | Converts an LLVM floating point comparison op into an equivalent op of our
-- own data type.
fromLlvmFPred :: LLVMF.FloatingPointPredicate -> Op.CompOp
fromLlvmFPred LLVMF.OEQ = Op.CompArithOp $ Op.OFloatOp Op.Eq
fromLlvmFPred LLVMF.ONE = Op.CompArithOp $ Op.OFloatOp Op.NEq
fromLlvmFPred LLVMF.OGT = Op.CompArithOp $ Op.OFloatOp Op.GT
fromLlvmFPred LLVMF.OGE = Op.CompArithOp $ Op.OFloatOp Op.GE
fromLlvmFPred LLVMF.OLT = Op.CompArithOp $ Op.OFloatOp Op.LT
fromLlvmFPred LLVMF.OLE = Op.CompArithOp $ Op.OFloatOp Op.LE
fromLlvmFPred LLVMF.ORD = Op.CompArithOp $  Op.FloatOp Op.Ordered
fromLlvmFPred LLVMF.UNO = Op.CompArithOp $  Op.FloatOp Op.Unordered
fromLlvmFPred LLVMF.UEQ = Op.CompArithOp $ Op.UFloatOp Op.Eq
fromLlvmFPred LLVMF.UGT = Op.CompArithOp $ Op.UFloatOp Op.GT
fromLlvmFPred LLVMF.UGE = Op.CompArithOp $ Op.UFloatOp Op.GE
fromLlvmFPred LLVMF.ULT = Op.CompArithOp $ Op.UFloatOp Op.LT
fromLlvmFPred LLVMF.ULE = Op.CompArithOp $ Op.UFloatOp Op.LE
fromLlvmFPred LLVMF.UNE = Op.CompArithOp $ Op.UFloatOp Op.NEq
fromLlvmFPred op = error $ "'fromLlvmFPred' not implemented for " ++ show op

-- | Adds the missing data or state flow edges from block nodes to data or state
-- nodes, as described in the given build state.
addMissingBlockToDatumDataFlowEdges :: BuildState -> BuildState
addMissingBlockToDatumDataFlowEdges st =
  let g0 = getOSGraph st
      deps = map ( \(l, n) ->
                   if G.isValueNode n
                   then (l, n, G.DataFlowEdge)
                   else if G.isStateNode n
                        then (l, n, G.StateFlowEdge)
                        else error ( "addMissingBlockToDatumDataFlowEdges: "
                                     ++ "This should never happen"
                                   )
                 )
                 (blockToDatumDataFlows st)
      g1 =
        foldr ( \(l, n, et) g ->
                let pair = (fromJust $ findBlockNodeWithID st l, n)
                in fst $ G.addNewEdge et pair g
              )
              g0
              deps
  in updateOSGraph st g1

-- | Adds the missing block-to-datum definition edges, as described in the
-- given build state.
addMissingBlockToDatumDefEdges :: BuildState -> BuildState
addMissingBlockToDatumDefEdges st =
  let g0 = getOSGraph st
      defs = blockToDatumDefs st
      g1 = foldr ( \(block_id, dn, nr) g ->
                   let ln = fromJust $ findBlockNodeWithID st block_id
                       (g', new_e) = G.addNewDefEdge (ln, dn) g
                       new_el = (G.getEdgeLabel new_e) { G.inEdgeNr = nr }
                       g'' = G.updateEdgeLabel new_el new_e g'
                   in g''
                 )
                 g0
                 defs
  in updateOSGraph st g1

-- | Adds the missing datum-to-block definition edges, as described in the
-- given build state.
addMissingDatumToBlockDefEdges :: BuildState -> BuildState
addMissingDatumToBlockDefEdges st =
  let g0 = getOSGraph st
      defs = datumToBlockDefs st
      g1 = foldr ( \(dn, block_id, nr) g ->
                   let ln = fromJust $ findBlockNodeWithID st block_id
                       (g', new_e) = G.addNewDefEdge (dn, ln) g
                       new_el = (G.getEdgeLabel new_e) { G.outEdgeNr = nr }
                       g'' = G.updateEdgeLabel new_el new_e g'
                   in g''
                 )
                 g0
                 defs
  in updateOSGraph st g1

-- | Gets the LLVM instruction from a named expression.
fromNamed :: LLVM.Named i -> i
fromNamed (_ LLVM.:= i) = i
fromNamed (LLVM.Do i) = i

-- | Applies various transformations on the 'OpStructure', such as
-- canonicalizing copy operations.
applyOSTransformations :: BuildState -> BuildState
applyOSTransformations st =
  let os0 = opStruct st
      b2ddfs0 = blockToDatumDataFlows st
      os1 = OS.canonicalizeCopies os0
      -- Canonicalizing copies will remove value nodes with constants, which
      -- must then be removed from the book keeping held within the build state.
      b2ddfs1 = filter (\(_, n) -> G.isNodeInGraph (OS.osGraph os1) n) b2ddfs0
  in st { opStruct = os1
        , blockToDatumDataFlows = b2ddfs1
        }

-- | Removes all block nodes from a 'BuildState' which have no edges.
removeUnusedBlockNodes :: BuildState -> BuildState
removeUnusedBlockNodes st =
  let g = getOSGraph st
      ns = filter G.isBlockNode $ G.getAllNodes g
      unused = filter (\n -> length (G.getNeighbors g n) == 0) ns
      new_g = foldr G.delNode g unused
      new_st = updateOSGraph st new_g
      entry_block = findBlockNodeWithID st (fromJust $ entryBlock st)
  in if isJust entry_block
     then new_st
     else new_st { entryBlock = Nothing }
