{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Language.InstrSel.OpStructures.LLVM.OSMaker
  ( SymbolFormable (..)
  , mkFunctionOS
  , mkPatternOS
  )
where

import qualified Language.InstrSel.Constraints as C
  ( Constraint )
import qualified Language.InstrSel.Constraints.ConstraintBuilder as C
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
  , splitOn
  , scanlM
  )

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified LLVM.General.AST.Global as LLVM
  ( Global (..) )
import qualified LLVM.General.AST.IntegerPredicate as LLVMI

import Control.Monad
  ( foldM )

import Data.Maybe

import Data.List
  ( intercalate
  , nub
  )



--------------
-- Data types
--------------

-- | Represents a mapping from a symbol to a value node currently in the graph.
type SymToValueNodeMapping = (Symbol, G.NodeID)

-- | Represents a flow that goes from a block node, identified by the given ID,
-- to an datum node. This is needed to draw the pending flow edges after both
-- the data-flow graph and the control-flow graph have been built. If the datum
-- node is a value node then a data-flow edge is added, and if it is a state
-- node then a state-flow edge is added.
type PendingBlockToDatumFlow = (F.BlockName, G.NodeID)

-- | Represents a definition that goes from a block node, identified by the
-- given ID, an datum node. This is needed to draw the pending definition edges
-- after both the data-flow graph and the control-flow graph have been
-- built. Since the in-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the in-edge number of the data-flow edge is
-- also included in the tuple.
type PendingBlockToDatumDef = (F.BlockName, G.NodeID, G.EdgeNr)

-- | Represents a definition that goes from an datum node to a block node,
-- identified by the given ID. This is needed to draw the pending definition
-- edges after both the data-flow graph and the control-flow graph have been
-- built. Since the out-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the out-edge number of the data-flow edge is
-- also included in the tuple.
type PendingDatumToBlockDef = (G.NodeID, F.BlockName, G.EdgeNr)

-- | Retains various symbol names.
data Symbol
  = LocalStringSymbol String
  | GlobalStringSymbol String
  | TemporarySymbol Integer
  deriving (Show, Eq)

instance PrettyShow Symbol where
  pShow (LocalStringSymbol str) = "%" ++ str
  pShow (GlobalStringSymbol str) = "@" ++ str
  pShow (TemporarySymbol int) = "%" ++ show int

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
  | NullConstant
  deriving (Show, Eq)

instance PrettyShow Constant where
  pShow IntConstant { signedIntValue = v } = pShow v
  pShow FloatConstant { floatValue = v } = pShow v
  pShow GlobalReferenceConstant { globalRefName = s } = pShow s
  pShow NullConstant = "null"

-- | Represents the intermediate build data.
data BuildState
  = BuildState
      { opStruct :: OS.OpStructure
        -- ^ The current operation structure.
      , lastTouchedNode :: Maybe G.Node
        -- ^ The last node (if any) that was touched.
      , lastTouchedStateNode :: Maybe G.Node
        -- ^ The state node (if any) that was touched. Note that this can point
        -- to the same node as indicated by 'lastTouchedNode'.
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
      , blockToDatumDataFlows :: [PendingBlockToDatumFlow]
        -- ^ List of block-to-datum flow dependencies that are yet to be
        -- converted into edges.
      , blockToDatumDefs :: [PendingBlockToDatumDef]
        -- ^ List of block-to-datum definitions that are yet to be converted
        -- into edges.
      , datumToBlockDefs :: [PendingDatumToBlockDef]
        -- ^ List of datum-to-block definitions that are yet to be converted
        -- into edges.
      , funcInputValues :: [G.NodeID]
        -- ^ The value nodes representing the function input arguments.
      , patExtValues :: [G.NodeID]
        -- ^ The value nodes representing the external values of the pattern.
      }
  deriving (Show)

-- | Contains all the functions for building an 'OpStructure' from an AST
-- (represented as a 'LLVM.Global'). The idea is that each function will
-- recursively call the appropriate building function on every argument of the
-- part, thus traversing the entire AST and building the 'OpStructure' from the
-- bottom up.
data Builder
  = Builder
      { mkFromGlobal :: Builder
                     -> BuildState
                     -> LLVM.Global
                     -> Either String BuildState
      , mkFromBasicBlock :: Builder
                         -> BuildState
                         -> LLVM.BasicBlock
                         -> Either String BuildState
      , mkFromNamed :: Builder
                    -> BuildState
                    -> LLVM.Named LLVM.Instruction
                    -> Either String BuildState
      , mkFromInstruction :: Builder
                          -> BuildState
                          -> LLVM.Instruction
                          -> Either String BuildState
      , mkFromTerminator :: Builder
                         -> BuildState
                         -> LLVM.Terminator
                         -> Either String BuildState
      , mkFromOperand :: Builder
                      -> BuildState
                      -> LLVM.Operand
                      -> Either String BuildState
      , mkFromParameter :: Builder
                        -> BuildState
                        -> LLVM.Parameter
                        -> Either String BuildState
      }




----------------
-- Type classes
----------------

-- | Class for converting an LLVM symbol datum into a 'Symbol'.
class SymbolFormable a where
  toSymbol :: a -> Either String Symbol

instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = Right $ LocalStringSymbol str
  toSymbol (LLVM.UnName int) = Right $ TemporarySymbol $ toInteger int

-- | Class for converting an LLVM constant operand into a corresponding
-- 'Constant'.
class ConstantFormable a where
  toConstant :: a -> Either String Constant

instance ConstantFormable LLVMC.Constant where
  toConstant i@(LLVMC.Int b _) =
    Right $ IntConstant { intBitWidth = fromIntegral b
                        , signedIntValue = LLVMC.signedIntegerValue i
                        }
  toConstant (LLVMC.GlobalReference t n) =
    do rt <- toOpDataType t
       sym <- toSymbol n
       return $ GlobalReferenceConstant { globalRefType = rt
                                        , globalRefName = sym
                                        }
  toConstant (LLVMC.Null (LLVM.PointerType {})) = Right NullConstant
  toConstant l = Left $ "toConstant: not implemented for " ++ show l

-- | Class for converting an LLVM operand into a corresponding operand
-- 'D.DataType'.
class OperandDataTypeFormable a where
  toOpDataType :: a -> Either String D.DataType

instance OperandDataTypeFormable Constant where
  toOpDataType IntConstant { intBitWidth = w, signedIntValue = v } =
    Right $ D.IntConstType { D.intConstValue = rangeFromSingleton v
                           , D.intConstNumBits = Just $ toNatural w
                           }
  toOpDataType NullConstant = Right D.PointerNullType
  toOpDataType c = Left $ "toOpDataType: not implemented for " ++ show c

instance OperandDataTypeFormable LLVM.Type where
  toOpDataType (LLVM.IntegerType bits) =
    Right $ D.IntTempType { D.intTempNumBits = toNatural bits }
  toOpDataType (LLVM.PointerType _ _) = return D.PointerTempType
  toOpDataType t = Left $ "toOpDataType: not implemented for " ++ show t

instance OperandDataTypeFormable LLVM.Operand where
  toOpDataType (LLVM.LocalReference t _) = toOpDataType t
  toOpDataType (LLVM.ConstantOperand c) =
    do const_d <- toConstant c
       toOpDataType const_d
  toOpDataType o = Left $ "toOpDataType: not implemented for " ++ show o

-- | Class for converting the 'LLVM.function' value of a 'LLVM.Call' operation
-- into a corresponding return 'D.DataType'.
class ReturnDataTypeFormable a where
  toReturnDataType :: a -> Either String D.DataType

instance ReturnDataTypeFormable LLVM.Type where
  -- This is how LLVM wraps the return type of a function
  toReturnDataType (LLVM.PointerType (LLVM.FunctionType t _ _) _) =
    toOpDataType t
  toReturnDataType LLVM.VoidType = return D.VoidType
  toReturnDataType t = Left $ "toReturnDataType: not implemented for " ++ show t

instance ReturnDataTypeFormable LLVMC.Constant where
  toReturnDataType (LLVMC.GlobalReference t _) = toReturnDataType t
  toReturnDataType t = Left $ "toReturnDataType: not implemented for " ++ show t

instance ReturnDataTypeFormable LLVM.Operand where
  toReturnDataType (LLVM.LocalReference t _) = toReturnDataType t
  toReturnDataType (LLVM.ConstantOperand o) = toReturnDataType o
  toReturnDataType o = Left $ "toReturnDataType: not implemented for " ++ show o

instance ReturnDataTypeFormable LLVM.CallableOperand where
  toReturnDataType (Right o) = toReturnDataType o
  toReturnDataType o = Left $ "toReturnDataType: not implemented for " ++ show o

-- | Class for converting an LLVM operand into a 'F.FunctionName'.
class FunctionNameFormable a where
  toFunctionName :: a -> Either String F.FunctionName

instance FunctionNameFormable LLVM.Name where
  -- TODO: temporary fix
  toFunctionName n = Right $ F.toFunctionName $ nameToString n
  -- toFunctionName (LLVM.Name str) = Right $ F.toFunctionName str
  -- toFunctionName n = Left $ "toFunctionName: not implemented for " ++ show n

instance FunctionNameFormable LLVMC.Constant where
  toFunctionName (LLVMC.GlobalReference _ n) = toFunctionName n
  toFunctionName c = Left $ "toFunctionName: not implemented for " ++ show c

instance FunctionNameFormable LLVM.Operand where
  toFunctionName (LLVM.LocalReference _ n) = toFunctionName n
  toFunctionName (LLVM.ConstantOperand c) = toFunctionName c
  toFunctionName o = Left $ "toFunctionName: not implemented for " ++ show o

instance FunctionNameFormable LLVM.CallableOperand where
  toFunctionName (Right o) = toFunctionName o
  toFunctionName o = Left $ "toFunctionName: not implemented for " ++ show o

instance FunctionNameFormable String where
  toFunctionName str = Right $ F.toFunctionName str

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
    -> Either String BuildState
       -- ^ An error message or the new build state.

instance (Buildable e) => Buildable [e] where
  build b st e = foldM (build b) st e

instance Buildable LLVM.Global where
  build b st e = (mkFromGlobal b) b st e

instance Buildable LLVM.BasicBlock where
  build b st e = (mkFromBasicBlock b) b st e

instance Buildable (LLVM.Named LLVM.Instruction) where
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

-- | Builds an 'OpStructure', together with the input value nodes, from a
-- function to be compiled. If the definition is not a 'Function', or any other
-- error occurs, an error message is returned.
mkFunctionOS :: LLVM.Global -> Either String (OS.OpStructure, [G.NodeID])
mkFunctionOS f@(LLVM.Function {}) =
  do st0 <- mkInitBuildState
     st1 <- build mkFunctionDFGBuilder st0 f
     st2 <- build mkFunctionCFGBuilder st1 f
     entry_name <- getEntryBlock st2
     entry_node <- getBlockNodeWithName st2 entry_name
     st3 <- updateOSEntryBlockNode st2 entry_node
     st4 <- applyOSTransformations st3
     st5 <- addPendingBlockToDatumFlowEdges st4
     st6 <- addPendingBlockToDatumDefEdges st5
     st7 <- addPendingDatumToBlockDefEdges st6
     return $ (opStruct st7, funcInputValues st7)
mkFunctionOS _ = Left "mkFunctionOS: not a Function"

-- | Builds an 'OpStructure', together with the external value nodes, from an
-- instruction pattern. If the definition is not a 'Function', or any other
-- error occurs, an error message is returned.
mkPatternOS :: LLVM.Global -> Either String (OS.OpStructure, [G.NodeID])
mkPatternOS f@(LLVM.Function {}) =
  do st0 <- mkInitBuildState
     st1 <- build mkPatternDFGBuilder st0 f
     st2 <- build mkPatternCFGBuilder st1 f
     entry_name <- getEntryBlock st2
     entry_node <- getBlockNodeWithName st2 entry_name
     st3 <- updateOSEntryBlockNode st2 entry_node
     st4 <- applyOSTransformations st3
     st5 <- addPendingBlockToDatumDefEdges st4
     st6 <- addPendingDatumToBlockDefEdges st5
     st7 <- removeUnusedBlockNodes st6
     return $ (opStruct st7, patExtValues st7)
mkPatternOS _ = Left "mkPattern: not a Function"

-- | Creates an initial 'BuildState'.
mkInitBuildState :: Either String BuildState
mkInitBuildState =
  Right $ BuildState { opStruct = OS.mkEmpty
                     , lastTouchedNode = Nothing
                     , lastTouchedStateNode = Nothing
                     , entryBlock = Nothing
                     , currentBlock = Nothing
                     , symMaps = []
                     , blockToDatumDataFlows = []
                     , blockToDatumDefs = []
                     , datumToBlockDefs = []
                     , funcInputValues = []
                     , patExtValues = []
                     }

-- | Constructs a 'Builder' that will construct a function data-flow graph.
mkFunctionDFGBuilder :: Builder
mkFunctionDFGBuilder =
  Builder { mkFromGlobal      = mkFunctionDFGFromGlobal
          , mkFromBasicBlock  = mkFunctionDFGFromBasicBlock
          , mkFromNamed       = mkFunctionDFGFromNamed
          , mkFromInstruction = mkFunctionDFGFromInstruction
          , mkFromTerminator  =
              \_ _ t -> Left $ "mkFromTerminator: not implemented for "
                                ++ show t
          , mkFromOperand     = mkFunctionDFGFromOperand
          , mkFromParameter   = mkFunctionDFGFromParameter
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
              \_ _ p -> Left $ "mkFromParameter: not implemented for " ++ show p
          }

-- | Constructs a 'Builder' that will construct a pattern data-flow graph.
mkPatternDFGBuilder :: Builder
mkPatternDFGBuilder =
  mkFunctionDFGBuilder { mkFromBasicBlock  = newBlockMk
                       , mkFromNamed       = newNamedMk
                       , mkFromInstruction = newInstrMk
                       }
  where newBlockMk b st0 bb =
          -- Let the default builder handle it, but remove the pending
          -- definition edge to the state node if this basic block has produced
          -- a state node in the graph.
          do st1 <- mkFunctionDFGFromBasicBlock b st0 bb
             let st_n = lastTouchedStateNode st1
             return $ if isJust st_n
                      then let n = fromJust st_n
                               es = blockToDatumDefs st1
                               pruned_es = filter ( \(_, n', _) ->
                                                    G.getNodeID n /= n'
                                                  )
                                                  es
                           in st1 { blockToDatumDefs = pruned_es }
                      else st1
        newInstrMk b st i@(LLVM.Call {}) =
          let f_name = retrieveFunctionName i
          in if isJust f_name
             then case (extractFunctionNamePart $ fromJust f_name)
                  of "setreg"          -> mkPatternDFGFromSetregCall b st i
                     "param"           -> mkPatternDFGFromParamCall b st i
                     "call"            -> mkPatternDFGFromFunCall b st i
                     "uncond-br"       -> -- Will be processed in the CFG
                                          return st
                     "cond-br-or-fall" -> -- Will be processed in the CFG
                                          return st
                     "return"          -> -- Will be processed in the CFG
                                          return st
                     _ -> -- Let the default builder handle it
                          mkFunctionDFGFromInstruction b st i
             else -- Let the default builder handle it
                  mkFunctionDFGFromInstruction b st i
        newInstrMk b st i = mkFunctionDFGFromInstruction b st i
        newNamedMk b st0 ((LLVM.UnName _) LLVM.:= expr@(LLVM.Call {})) =
          -- Unnamed calls to functions that returns some value must not yield a
          -- result data node, meaning it must be removed after the call
          -- instruction has been processed.
          do st1 <- build b st0 expr
             ret_t <- toReturnDataType $ LLVM.function expr
             if D.isVoidType ret_t
             then return st1
             else do let g = getOSGraph st1
                     ret_n <- getLastTouchedValueNode st1
                     let new_g = G.delNode ret_n g
                     st2 <- updateOSGraph st1 new_g
                     return st2
        newNamedMk b st i = -- Let the default builder handle it
                            mkFunctionDFGFromNamed b st i

-- | Constructs a 'Builder' that will construct a pattern control-flow graph.
mkPatternCFGBuilder :: Builder
mkPatternCFGBuilder =
  mkFunctionCFGBuilder { mkFromInstruction = newInstrMk
                       , mkFromTerminator = newTermMk
                       }
  where
  newInstrMk b st i@(LLVM.Call {}) =
    let f_name = retrieveFunctionName i
    in if isJust f_name
       then case (extractFunctionNamePart $ fromJust f_name)
            of "return"          -> mkPatternCFGFromReturnCall b st i
               "uncond-br"       -> mkPatternCFGFromUncondBrCall b st i
               "cond-br-or-fall" -> mkPatternCFGFromCondBrOrFallCall b st i
               _ -> -- Let the default builder handle it
                    mkFunctionCFGFromInstruction b st i
       else -- Let the default builder handle it
            mkFunctionCFGFromInstruction b st i
  newInstrMk b st i = mkFunctionCFGFromInstruction b st i
  newTermMk _ st (LLVM.Ret { LLVM.returnOperand = Nothing }) = return st
  newTermMk _ _ (LLVM.Ret { LLVM.returnOperand = Just _ }) =
    Left "mkPatternCFGBuilder: non-void returns not supported"
  newTermMk b st i@(LLVM.Br {}) = mkFunctionCFGFromTerminator b st i
  newTermMk b st i@(LLVM.CondBr {}) = mkFunctionCFGFromTerminator b st i
  newTermMk _ _ i =
    Left $ "mkPatternCFGBuilder: cannot handle terminator: " ++ show i

-- | Gets the last touched node, and checks that it is a value node.
getLastTouchedValueNode :: BuildState -> Either String G.Node
getLastTouchedValueNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isValueNode n
             then return n
             else Left $ "getLastTouchedValueNode: last touched node is not " ++
                         "a value node: " ++ show n
     else Left "getLastTouchedValueNode: has no last touched node"

-- | Gets the last touched node, and checks that it is a control node.
getLastTouchedControlNode :: BuildState -> Either String G.Node
getLastTouchedControlNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isControlNode n
             then return n
             else Left $ "getLastTouchedControlNode: last touched node is " ++
                         "not a control node: " ++ show n
     else Left "getLastTouchedControlNode: has no last touched node"

-- | Gets the last touched node, and checks that it is a call node.
getLastTouchedCallNode :: BuildState -> Either String G.Node
getLastTouchedCallNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isCallNode n
             then return n
             else Left $ "getLastTouchedCallNode: last touched node is not " ++
                         "a call node: " ++ show n
     else Left "getLastTouchedCallNode: has no last touched node"

-- | Gets the last touched node, and checks that it is a computation node.
getLastTouchedComputationNode :: BuildState -> Either String G.Node
getLastTouchedComputationNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isComputationNode n
             then return n
             else Left $ "getLastTouchedComputationNode: last touched node " ++
                         "is not a computation node: " ++ show n
     else Left "getLastTouchedComputationNode: has no last touched node"

-- | Gets the last touched node, and checks that it is a block node.
getLastTouchedBlockNode :: BuildState -> Either String G.Node
getLastTouchedBlockNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isBlockNode n
             then return n
             else Left $ "getLastTouchedBlockNode: last touched node is not " ++
                         "a block node: " ++ show n
     else Left "getLastTouchedBlockNode: has no last touched node"

-- | Gets the last touched node, and checks that it is a phi node.
getLastTouchedPhiNode :: BuildState -> Either String G.Node
getLastTouchedPhiNode st =
  let maybe_n = lastTouchedNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isPhiNode n
             then return n
             else Left $ "getLastTouchedPhiNode: last touched node is not a " ++
                         "phi node: " ++ show n
     else Left "getLastTouchedPhiNode: has no last touched node"

-- | Gets the last touched state node, and checks that it is a state node.
getLastTouchedStateNode :: BuildState -> Either String G.Node
getLastTouchedStateNode st =
  let maybe_n = lastTouchedStateNode st
  in if isJust maybe_n
     then let n = fromJust maybe_n
          in if G.isStateNode n
             then return n
             else Left $ "getLastTouchedStateNode: last touched state node " ++
                         "is not a state node: " ++ show n
     else Left "getLastTouchedStateNode: has no last touched state node"

-- | Gets the entry block in a given state.
getEntryBlock :: BuildState -> Either String F.BlockName
getEntryBlock st =
  let e = entryBlock st
  in if isJust e
     then return $ fromJust e
     else Left "getEntryBlock: has no entry block"

-- | Gets the current block in a given state.
getCurrentBlock :: BuildState -> Either String F.BlockName
getCurrentBlock st =
  let e = currentBlock st
  in if isJust e
     then return $ fromJust e
     else Left "getCurrentBlock: has no current block"

-- | Gets the name of a given 'LLVM.Call' instruction. If it is not a
-- 'LLVM.Call', or if it does not have a proper name, 'Nothing' is returned.
retrieveFunctionName :: LLVM.Instruction -> Maybe String
retrieveFunctionName ( LLVM.Call { LLVM.function =
                               Right
                               ( LLVM.ConstantOperand
                                 ( LLVMC.GlobalReference _ (LLVM.Name name)
                                 )
                               )
                            }
                )
  = Just name
retrieveFunctionName _ = Nothing

-- | Gets the name of a given 'LLVM.Call' instruction.
getFunctionName :: LLVM.Instruction -> Either String String
getFunctionName i =
  let name = retrieveFunctionName i
  in if isJust name
     then return $ fromJust name
     else Left $ "getFunctionName: unexpected instruction: " ++ show i

-- | Extracts the name part of a given function name. This is needed because
-- some function calls has parameters embedded into the function name, such as
-- labels (which cannot be given as a function argument in LLVM IR).
extractFunctionNamePart :: String -> String
extractFunctionNamePart = head . splitOn "."

-- | Extracts the label part of a given function name.
extractFunctionLabelPart :: String -> String
extractFunctionLabelPart = intercalate "." . tail . splitOn "."

-- | Adds a 'G.ControlNode' of type 'Op.Ret'.
mkPatternCFGFromReturnCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternCFGFromReturnCall
  _
  st0
  (LLVM.Call { LLVM.arguments = [] })
  =
  do st1 <- addNewNode st0 (G.ControlNode Op.Ret)
     rn <- getLastTouchedControlNode st1
     current_b <- getCurrentBlock st1
     bn <- getBlockNodeWithName st1 current_b
     st2 <- addNewEdge st1 G.ControlFlowEdge bn rn
     return st2
mkPatternCFGFromReturnCall
  _
  st0
  (LLVM.Call { LLVM.arguments = [(LLVM.LocalReference _ arg, _)] })
  =
  do st1 <- addNewNode st0 (G.ControlNode Op.Ret)
     rn <- getLastTouchedControlNode st1
     current_b <- getCurrentBlock st1
     bn <- getBlockNodeWithName st1 current_b
     arg_sym <- toSymbol arg
     vn <- getValueNodeMappedToSym st0 arg_sym
     st2 <- addNewEdge st1 G.DataFlowEdge vn rn
     st3 <- addNewEdge st2 G.ControlFlowEdge bn rn
     return st3
mkPatternCFGFromReturnCall _ _ i =
  Left $ "mkPatternCFGFromReturnCall: not implemented for " ++ show i

mkPatternCFGFromUncondBrCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternCFGFromUncondBrCall
  b
  st0
  i@(LLVM.Call { LLVM.arguments = [] })
  =
  do f_name <- getFunctionName i
     let label = "%" ++ extractFunctionLabelPart f_name
     st1 <- mkFunctionCFGFromControlOp b st0 Op.Br ([] :: [LLVM.Operand])
     br_node <- getLastTouchedControlNode st1
     st2 <- ensureBlockNodeExists st1 $ F.toBlockName label
     dst_node <- getLastTouchedBlockNode st2
     st3 <- addNewEdgesManyDests st2
                                 G.ControlFlowEdge
                                 br_node
                                 [dst_node]
     return st3
mkPatternCFGFromUncondBrCall _ _ i =
  Left $ "mkPatternCFGFromUncondBrCall: not implemented for " ++ show i

mkPatternCFGFromCondBrOrFallCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternCFGFromCondBrOrFallCall
  b
  st0
  i@(LLVM.Call { LLVM.arguments = [(arg@(LLVM.LocalReference _ _), _)] })
  =
  do f_name <- getFunctionName i
     let t_label = "%" ++ extractFunctionLabelPart f_name
     st1 <- mkFunctionCFGFromControlOp b st0 Op.CondBr [arg]
     br_node <- getLastTouchedControlNode st1
     st2 <- ensureBlockNodeExists st1 $ F.toBlockName t_label
     t_dst_node <- getLastTouchedBlockNode st2
     st3 <- ensureBlockNodeExists st2 F.mkEmptyBlockName
     f_dst_node <- getLastTouchedBlockNode st3
     st4 <- addNewEdgesManyDests st3
                                 G.ControlFlowEdge
                                 br_node
                                 [t_dst_node, f_dst_node]
     st5 <- addConstraints st4 $
            C.mkFallThroughConstraints (G.getNodeID f_dst_node)
     return st5
mkPatternCFGFromCondBrOrFallCall _ _ i =
  Left $ "mkPatternCFGFromCondBrOrFallCall: not implemented for " ++ show i

mkFunctionDFGFromGlobal
  :: Builder
  -> BuildState
  -> LLVM.Global
  -> Either String BuildState
mkFunctionDFGFromGlobal b st0 f@(LLVM.Function {}) =
  do let (params, _) = LLVM.parameters f
     st1 <- build b st0 params
     st2 <- build b st1 (LLVM.basicBlocks f)
     return st2
mkFunctionDFGFromGlobal _ _ g =
  Left $ "mkFunctionDFGFromGlobal: not implemented for " ++ show g

mkFunctionDFGFromBasicBlock
  :: Builder
  -> BuildState
  -> LLVM.BasicBlock
  -> Either String BuildState
mkFunctionDFGFromBasicBlock b st0 (LLVM.BasicBlock b_name insts _) =
  do let block_name = F.BlockName $ nameToString b_name
     st1 <- if isNothing $ entryBlock st0
            then foldM (\st n -> addPendingBlockToDatumFlow st (block_name, n))
                       (st0 { entryBlock = Just block_name })
                       (funcInputValues st0)
            else return st0
     let st2 = st1 { currentBlock = Just block_name
                   , lastTouchedStateNode = Nothing
                   }
     st3 <- foldM (build b) st2 insts
     let st_n = lastTouchedStateNode st3
     st4 <- if isJust st_n
            then addPendingBlockToDatumDef st3 ( block_name
                                               , G.getNodeID $ fromJust st_n
                                               , 0
                                               )
            else return st3
     return st4

mkFunctionDFGFromNamed
  :: Builder
  -> BuildState
  -> (LLVM.Named LLVM.Instruction)
  -> Either String BuildState
mkFunctionDFGFromNamed b st0 (name LLVM.:= expr) =
  do st1 <- build b st0 expr
     sym <- toSymbol name
     res_n <- getLastTouchedValueNode st1
     let res_dt = G.getDataTypeOfValueNode res_n
     -- We use 'ensureValueNodeWithSymExists' as there may already exist a value
     -- node with this symbol (typically incurred by phi functions). In such
     -- cases, we want to merge the existing node with the result value node
     -- produced by the expression.
     st2 <- ensureValueNodeWithSymExists st1 sym res_dt
     sym_n <- getLastTouchedValueNode st2
     st3 <- updateOSGraph st2 (G.mergeNodes sym_n res_n (getOSGraph st2))
     st4 <- replaceNodeIDInBuildState (G.getNodeID res_n)
                                      (G.getNodeID sym_n)
                                      st3
     return st4
mkFunctionDFGFromNamed b st (LLVM.Do expr) = build b st expr

mkFunctionDFGFromInstruction
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkFunctionDFGFromInstruction b st (LLVM.Add _ _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.Add)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FAdd _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.FloatOp Op.Add)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Sub _ _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.Sub)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FSub _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.FloatOp Op.Sub)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Mul _ _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.Mul)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FMul _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.FloatOp Op.Mul)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.UDiv _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.UIntOp Op.Div)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.SDiv _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.SIntOp Op.Div)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FDiv _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.FloatOp Op.Div)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.URem op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.UIntOp Op.Rem)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.SRem op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.SIntOp Op.Rem)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FRem _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.FloatOp Op.Rem)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Shl _ _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.Shl)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.LShr _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.LShr)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.AShr _ op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.AShr)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.And op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.And)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Or op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.Or)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Xor op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompArithOp $ Op.IntOp Op.XOr)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.ICmp p op1 op2 _) =
  mkFunctionDFGFromCompOp b
                          st
                          (D.IntTempType { D.intTempNumBits = 1 })
                          (fromLlvmIPred p)
                          [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.FCmp p op1 op2 _) =
  do dt <- toTempDataType op1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (fromLlvmFPred p)
                             [op1, op2]
mkFunctionDFGFromInstruction b st (LLVM.Trunc op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeConvOp Op.Trunc)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.ZExt op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeConvOp Op.ZExt)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.SExt op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeConvOp Op.SExt)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.IntToPtr op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeConvOp Op.IntToPtr)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.PtrToInt op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeConvOp Op.PtrToInt)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.BitCast op1 t1 _) =
  do dt <- toOpDataType t1
     mkFunctionDFGFromCompOp b
                             st
                             dt
                             (Op.CompTypeCastOp Op.BitCast)
                             [op1]
mkFunctionDFGFromInstruction b st (LLVM.Load _ op1 _ _ _) =
  do t <- dereferencePointerOp op1
     dt <- toOpDataType t
     mkFunctionDFGFromMemOp b
                            st
                            dt
                            Op.Load
                            [op1]
mkFunctionDFGFromInstruction _ st0 (LLVM.Alloca t _ _ _) =
  -- TODO: fix with proper handling that includes stack adjustments
  do dt <- toOpDataType t
     st1 <- addNewNode st0 (G.ValueNode dt Nothing)
     b <- getCurrentBlock st1
     n <- getLastTouchedValueNode st1
     st2 <- addPendingBlockToDatumFlow st1 (b, G.getNodeID n)
     return st2
mkFunctionDFGFromInstruction b st0 (LLVM.Store _ addr_op val_op _ _ _) =
  do sts <- scanlM (build b) st0 [addr_op, val_op]
     operand_ns <- mapM getLastTouchedValueNode (tail sts)
     let st1 = last sts
     st2 <- addNewNode st1 (G.ComputationNode $ Op.CompMemoryOp Op.Store)
     op_node <- getLastTouchedComputationNode st2
     st3 <- addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
     st4 <- ensureStateNodeHasBeenTouched st3
     st_n4 <- getLastTouchedStateNode st4
     st5 <- addNewEdge st4 G.StateFlowEdge st_n4 op_node
     st6 <- addNewStateNode st5
     st_n6 <- getLastTouchedStateNode st6
     st7 <- addNewEdge st6 G.StateFlowEdge op_node st_n6
     return st7
mkFunctionDFGFromInstruction b st0 (LLVM.Call _ _ _ f args _ _) =
  do sts <- scanlM (build b) st0 $ map fst args
     operand_ns <- mapM getLastTouchedValueNode (tail sts)
     let st1 = last sts
     func_name <- toFunctionName f
     st2 <- addNewNode st1 (G.CallNode func_name)
     op_node <- getLastTouchedCallNode st2
     st3 <- addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
     st4 <- ensureStateNodeHasBeenTouched st3
     st_n4 <- getLastTouchedStateNode st4
     st5 <- addNewEdge st4 G.StateFlowEdge st_n4 op_node
     st6 <- addNewStateNode st5
     st_n6 <- getLastTouchedStateNode st6
     st7 <- addNewEdge st6 G.StateFlowEdge op_node st_n6
     -- Note that the result value node, if any, MUST be inserted last as the
     -- last touched node in this case must be a value node and not a state
     -- node.
     ret_type <- toReturnDataType f
     if D.isVoidType ret_type
     then return st7
     else do st8 <- addNewNode st7 (G.ValueNode ret_type Nothing)
             d_node <- getLastTouchedValueNode st8
             st9 <- addNewEdge st8 G.DataFlowEdge op_node d_node
             return st9
mkFunctionDFGFromInstruction b st0 (LLVM.Phi t phi_operands _) =
  do let (operands, blocks) = unzip phi_operands
         block_names = map (\b_name -> F.BlockName $ nameToString b_name) blocks
     operand_node_sts <- scanlM (build b) st0 operands
     operand_ns <- mapM getLastTouchedValueNode (tail operand_node_sts)
     let st1 = last operand_node_sts
     st2 <- addNewNode st1 G.PhiNode
     phi_node <- getLastTouchedPhiNode st2
     st3 <- foldM ( \st (n, name) ->
                    do let g = getOSGraph st
                           (g', new_e) = G.addNewDtFlowEdge (n, phi_node) g
                       st' <- updateOSGraph st g'
                       addPendingDatumToBlockDef st'
                                                 ( G.getNodeID n
                                                 , name
                                                 , G.getEdgeOutNr new_e
                                                 )
                  )
                  st2
                  (zip operand_ns block_names)
     dt <- toOpDataType t
     st4 <- addNewNode st3 (G.ValueNode dt Nothing)
     d_node <- getLastTouchedValueNode st4
     st5 <- addNewEdge st4 G.DataFlowEdge phi_node d_node
     current_b <- getCurrentBlock st5
     st6 <- -- Since we've just created the value node and only added a
            -- single data-flow edge to it, we are guaranteed that the edge-in
            -- number of that data-flow edge is 0.
            addPendingBlockToDatumDef st5 (current_b, G.getNodeID d_node, 0)
     return st6
mkFunctionDFGFromInstruction _ _ i =
  Left $ "mkFunctionDFGFromInstruction: not implemented for " ++ show i

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
  -> Either String BuildState
mkFunctionDFGFromCompOp b st0 dt op operands =
  do sts <- scanlM (build b) st0 operands
     operand_ns <- mapM getLastTouchedValueNode (tail sts)
     let st1 = last sts
     st2 <- addNewNode st1 (G.ComputationNode op)
     op_node <- getLastTouchedComputationNode st2
     st3 <- addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
     st4 <- addNewNode st3 (G.ValueNode dt Nothing)
     d_node <- getLastTouchedValueNode st4
     st5 <- addNewEdge st4 G.DataFlowEdge op_node d_node
     return st5

-- | Takes an operand of pointer type and returns the type after the pointer has
-- been dereferenced.
dereferencePointerOp :: LLVM.Operand -> Either String LLVM.Type
dereferencePointerOp (LLVM.LocalReference (LLVM.PointerType t _) _) = return t
dereferencePointerOp o = Left $ "dereferencePointerOp: " ++ show o ++
                                " is not a pointer"

-- | Inserts a new memory node representing the operation along with edges to
-- that computation node from the given operands (which will also be
-- processed). In addition, an input state node and output state node is
-- inserted. Lastly, a new value node representing the result will be added
-- along with an edge to that value node from the computation node.
mkFunctionDFGFromMemOp
  :: (Buildable o)
  => Builder
  -> BuildState
  -> D.DataType
     -- ^ The data type of the result.
  -> Op.MemoryOp
     -- ^ The memory computational operation.
  -> [o]
     -- ^ The operands.
  -> Either String BuildState
mkFunctionDFGFromMemOp b st0 dt op operands =
  do st1 <- mkFunctionDFGFromCompOp b st0 dt (Op.CompMemoryOp op) operands
     res_node <- getLastTouchedValueNode st1
     op_node <- let preds = G.getPredecessors (getOSGraph st1) res_node
                in if length preds == 1
                   then return $ head preds
                   else if length preds == 0
                        then Left $ "mkFunctionDFGFromMemOp: " ++
                                    show res_node ++ " has no predecessors"
                        else Left $ "mkFunctionDFGFromMemOp: " ++
                                    show res_node ++ " has multiple " ++
                                    "predecessors"
     st2 <- ensureStateNodeHasBeenTouched st1
     st_n2 <- getLastTouchedStateNode st2
     st3 <- addNewEdge st2 G.StateFlowEdge st_n2 op_node
     st4 <- addNewStateNode st3
     st_n4 <- getLastTouchedStateNode st4
     st5 <- addNewEdge st4 G.StateFlowEdge op_node st_n4
     -- Note that the result value node MUST be the last touched node.
     st6 <- touchNode st5 res_node
     return st6

mkFunctionDFGFromOperand
  :: Builder
  -> BuildState
  -> LLVM.Operand
  -> Either String BuildState
mkFunctionDFGFromOperand _ st (LLVM.LocalReference t name) =
  do sym <- toSymbol name
     dt <- toOpDataType t
     ensureValueNodeWithSymExists st sym dt
mkFunctionDFGFromOperand _ st (LLVM.ConstantOperand c) =
  do const_d <- toConstant c
     addNewValueNodeWithConstant st const_d
mkFunctionDFGFromOperand _ _ o =
  Left $ "mkFunctionDFGFromOperand: not implemented for " ++ show o

mkFunctionDFGFromParameter
  :: Builder
  -> BuildState
  -> LLVM.Parameter
  -> Either String BuildState
mkFunctionDFGFromParameter _ st0 (LLVM.Parameter t name _) =
  do sym <- toSymbol name
     dt <- toOpDataType t
     st1 <- ensureValueNodeWithSymExists st0 sym dt
     n <- getLastTouchedValueNode st1
     st2 <- addFuncInputValue st1 (G.getNodeID n)
     return st2

-- | Merges the two nodes in the function call.
mkPatternDFGFromSetregCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternDFGFromSetregCall
  _
  st0
  ( LLVM.Call { LLVM.arguments = [ (LLVM.LocalReference _ arg1, _)
                                 , (LLVM.LocalReference _ arg2, _)
                                 ]
              }
  )
  =
  do [n1, n2] <- mapM ( \arg ->
                        do sym <- toSymbol arg
                           getValueNodeMappedToSym st0 sym
                      ) $
                 [arg1, arg2]
     let g0 = getOSGraph st0
         do_merge = length (G.getDtFlowInEdges g0 n1) == 0 &&
                    length (G.getDtFlowOutEdges g0 n1) == 0
     g1 <- if do_merge
           then do let g = G.mergeNodes n2 n1 g0
                   sym1 <- toSymbol arg1
                   n1_origin <- let o = G.getOriginOfValueNode n1
                                in if isJust o
                                   then return $ fromJust o
                                   else Left $ "mkPatternDFGFromSetregCall: " ++
                                               "node mapped symbol '" ++
                                               pShow sym1 ++ "' has no origin"
                   return $ G.updateOriginOfValueNode n1_origin n2 g
           else return g0
     st1 <- updateOSGraph st0 g1
     st2 <- if do_merge
            then replaceNodeIDInBuildState (G.getNodeID n1) (G.getNodeID n2) st1
            else do let os = C.addSameDataLocConstraints [ G.getNodeID n1
                                                         , G.getNodeID n2
                                                         ]
                                                         (opStruct st1)
                        st = st1 { opStruct = os }
                    addPatExtValue st (G.getNodeID n2)
     return st2
mkPatternDFGFromSetregCall _ _ i@(LLVM.Call {}) =
  Left $ "mkPatternDFGFromSetregCall: unexpected number or type of function "
          ++ "arguments in " ++ show i
mkPatternDFGFromSetregCall _ _ i =
  Left $ "mkPatternDFGFromSetregCall: not implemented for " ++ show i

-- | Adds a new unnamed node with the same data type as the return type of the
-- function call.
mkPatternDFGFromParamCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternDFGFromParamCall _ st0 i@(LLVM.Call {}) =
  do rt <- case i of ( LLVM.Call
                       { LLVM.function =
                         ( Right
                           ( LLVM.ConstantOperand
                             ( LLVMC.GlobalReference
                               ( LLVM.PointerType
                                 { LLVM.pointerReferent =
                                   ( LLVM.FunctionType
                                     { LLVM.resultType = llvm_rt }
                                   )
                                 }
                               )
                               _
                             )
                           )
                         )
                       }) -> return llvm_rt
                     _ -> Left $ "mkPatternDFGFromParamCall: unexpected " ++
                                 "instruction: " ++ show i
     dt <- toOpDataType rt
     st1 <- addNewNode st0 (G.ValueNode dt Nothing)
     n <- getLastTouchedValueNode st1
     st2 <- addPatExtValue st1 (G.getNodeID n)
     return st2
mkPatternDFGFromParamCall _ _ i =
  Left $ "mkPatternDFGFromParamCall: not implemented for " ++ show i

-- | Adds a call node, where the data types of all input and output values is
-- 'D.AnyType'.
mkPatternDFGFromFunCall
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkPatternDFGFromFunCall b st0 i@(LLVM.Call {}) =
  do st1 <- mkFunctionDFGFromInstruction b st0 i
     -- The call node will have the wrong name as the true name of the call is
     -- embedded into the function name, so we need to fix that.
     old_f_name <- toFunctionName $ LLVM.function i
     f_name <- getFunctionName i
     new_f_name <- toFunctionName $
                   "%" ++ extractFunctionLabelPart f_name
     let g = getOSGraph st1
     call_n <- let ns = G.findCallNodesWithName g old_f_name
               in if length ns == 1
                  then return $ head ns
                  else if length ns == 0
                       then Left $ "mkPatternDFGFromFunCall: found no call " ++
                                   "node with name '" ++ pShow old_f_name ++ "'"
                       else Left $ "mkPatternDFGFromFunCall: found multiple " ++
                                   "call node with name '" ++
                                   pShow old_f_name ++ "'"
     st2 <- updateOSGraph st1 (G.updateNameOfCallNode new_f_name call_n g)
     -- If a local reference argument to the call does not have any ingoing
     -- data-flow edges, then its data type is irrelevant and must be set to
     -- 'D.AnyType'.
     let isRefArg (LLVM.LocalReference {}) = True
         isRefArg _ = False
         local_ref_args = filter isRefArg $
                          map fst $
                          LLVM.arguments i
     arg_syms <- mapM (\(LLVM.LocalReference _ n) -> toSymbol n) local_ref_args
     st3 <- foldM
              ( \st sym ->
                do let g' = getOSGraph st
                   n <- getValueNodeMappedToSym st sym
                   let es = G.getDtFlowInEdges g' n
                   if length es == 0
                   then updateOSGraph st $
                        G.updateDataTypeOfValueNode D.AnyType n g'
                   else return st
              )
              st2
              arg_syms
     -- If the function returns any data, then the data type of its value node
     -- is irrelevant and must be set to 'D.AnyType'. This also requires the
     -- last touched node to be updated.
     ret_t <- toReturnDataType $ LLVM.function i
     st4 <- if not $ D.isVoidType ret_t
            then do old_ret_n <- getLastTouchedValueNode st1
                    let old_g = getOSGraph st3
                        new_g = G.updateDataTypeOfValueNode D.AnyType
                                                            old_ret_n
                                                            old_g
                    st' <- updateOSGraph st3 new_g
                    new_ret_n <- getNodeWithID st' (G.getNodeID old_ret_n)
                    let st'' = st' { lastTouchedNode = Just new_ret_n }
                    return st''
            else return st3
     return st4
mkPatternDFGFromFunCall _ _ i =
  Left $ "mkPatternDFGFromFunCall: not implemented for " ++ show i

mkFunctionCFGFromGlobal :: Builder
                        -> BuildState
                        -> LLVM.Global
                        -> Either String BuildState
mkFunctionCFGFromGlobal b st f@(LLVM.Function {}) =
  build b st (LLVM.basicBlocks f)
mkFunctionCFGFromGlobal _ _ g =
  Left $ "mkFunctionCFGFromGlobal: not implemented for " ++ show g

mkFunctionCFGFromBasicBlock
  :: Builder
  -> BuildState
  -> LLVM.BasicBlock
  -> Either String BuildState
mkFunctionCFGFromBasicBlock b st0 ( LLVM.BasicBlock b_name
                                                    insts
                                                    named_term_inst
                                  ) =
  do let block_name = F.BlockName $ nameToString b_name
         term_inst = fromNamed named_term_inst
     let st1 = if isNothing $ entryBlock st0
               then st0 { entryBlock = Just block_name }
               else st0
     st2 <- ensureBlockNodeExists st1 block_name
     let st3 = st2 { currentBlock = Just block_name }
     st4 <- foldM (build b) st3 insts
     st5 <- build b st4 term_inst
     return st5

mkFunctionCFGFromInstruction
  :: Builder
  -> BuildState
  -> LLVM.Instruction
  -> Either String BuildState
mkFunctionCFGFromInstruction _ st _ = return st

mkFunctionCFGFromNamed
  :: (Buildable n)
  => Builder
  -> BuildState
  -> (LLVM.Named n)
  -> Either String BuildState
mkFunctionCFGFromNamed b st (_ LLVM.:= expr) = build b st expr
mkFunctionCFGFromNamed b st (LLVM.Do expr) = build b st expr

mkFunctionCFGFromTerminator
  :: Builder
  -> BuildState
  -> LLVM.Terminator
  -> Either String BuildState
mkFunctionCFGFromTerminator b st (LLVM.Ret op _) =
  mkFunctionCFGFromControlOp b st Op.Ret (maybeToList op)
mkFunctionCFGFromTerminator b st0 (LLVM.Br dst _) =
  do st1 <- mkFunctionCFGFromControlOp b st0 Op.Br ([] :: [LLVM.Operand])
     br_node <- getLastTouchedControlNode st1
     st2 <- ensureBlockNodeExists st1 (F.BlockName $ nameToString dst)
     dst_node <- getLastTouchedBlockNode st2
     st3 <- addNewEdge st2 G.ControlFlowEdge br_node dst_node
     return st3
mkFunctionCFGFromTerminator b st0 (LLVM.CondBr op t_dst f_dst _)
  =
  do st1 <- mkFunctionCFGFromControlOp b st0 Op.CondBr [op]
     br_node <- getLastTouchedControlNode st1
     st2 <- ensureBlockNodeExists st1 (F.BlockName $ nameToString t_dst)
     t_dst_node <- getLastTouchedBlockNode st2
     st3 <- ensureBlockNodeExists st2 (F.BlockName $ nameToString f_dst)
     f_dst_node <- getLastTouchedBlockNode st3
     st4 <- addNewEdgesManyDests st3
                                 G.ControlFlowEdge
                                 br_node
                                 [t_dst_node, f_dst_node]
     return st4
mkFunctionCFGFromTerminator _ st (LLVM.Unreachable _) =
  -- Do nothing
  return st
mkFunctionCFGFromTerminator _ _ t =
  Left $ "mkFunctionCFGFromTerminator: not implemented for " ++ show t

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
  -> Either String BuildState
mkFunctionCFGFromControlOp b st0 op operands =
  do sts <- scanlM (build b) st0 operands
     operand_ns <- mapM getLastTouchedValueNode (tail sts)
     let st1 = last sts
     st2 <- addNewNode st1 (G.ControlNode op)
     op_node <- getLastTouchedControlNode st2
     current_b <- getCurrentBlock st2
     bn <- getBlockNodeWithName st2 current_b
     st3 <- addNewEdge st2 G.ControlFlowEdge bn op_node
     st4 <- addNewEdgesManySources st3 G.DataFlowEdge operand_ns op_node
     return st4

mkFunctionCFGFromOperand :: Builder
                         -> BuildState
                         -> LLVM.Operand
                         -> Either String BuildState
mkFunctionCFGFromOperand _ st (LLVM.LocalReference t name) =
  do sym <- toSymbol name
     dt <- toOpDataType t
     ensureValueNodeWithSymExists st sym dt
mkFunctionCFGFromOperand _ st (LLVM.ConstantOperand c) =
  do const_d <- toConstant c
     addNewValueNodeWithConstant st const_d
mkFunctionCFGFromOperand _ _ o =
  Left $ "mkFunctionCFGFromOperand: not implemented for " ++ show o



------------------
-- Help functions
------------------

-- | Converts an argument into a temporary-oriented data type.
toTempDataType
  :: (OperandDataTypeFormable t)
  => t
  -> Either String D.DataType
     -- ^ An error message or the resulting data type.
toTempDataType a =
  do dt <- toOpDataType a
     conv dt
  where conv d@(D.IntTempType {}) = Right d
        conv (D.IntConstType { D.intConstNumBits = Just b }) =
          Right $ D.IntTempType { D.intTempNumBits = b }
        conv d = Left $ "toTempDataType: unexpected data type " ++ show d

-- | Gets the OS graph contained by the operation structure in a given state.
getOSGraph :: BuildState -> G.Graph
getOSGraph = OS.osGraph . opStruct

-- | Updates the OS graph contained by the operation structure in a given state.
updateOSGraph :: BuildState -> G.Graph -> Either String BuildState
updateOSGraph st g =
  let os = opStruct st
  in return $ st { opStruct = os { OS.osGraph = g } }

-- | Updates the OS entry block node contained by the operation structure in a
-- given state.
updateOSEntryBlockNode :: BuildState -> G.Node -> Either String BuildState
updateOSEntryBlockNode st n =
  let nid = G.getNodeID n
      os = opStruct st
  in return $ st { opStruct = os { OS.osEntryBlockNode = Just nid } }

-- | Updates the last touched node information.
touchNode :: BuildState -> G.Node -> Either String BuildState
touchNode st n = return $ st { lastTouchedNode = Just n }

-- | Adds a new node into a given state. This also updates the last touched node
-- setting.
addNewNode :: BuildState -> G.NodeType -> Either String BuildState
addNewNode st0 nt =
  do let (new_g, new_n) = G.addNewNode nt (getOSGraph st0)
     st1 <- updateOSGraph st0 new_g
     st2 <- touchNode st1 new_n
     return st2

-- | Adds a new state node into a given state. This also updates the last
-- touched node and last touched state node setting.
addNewStateNode :: BuildState -> Either String BuildState
addNewStateNode st0 =
  do st1 <- addNewNode st0 G.StateNode
     let st2 = st1 { lastTouchedStateNode = lastTouchedNode st1 }
     return st2

-- | Adds a list of constraints to the 'OpStructure' in the given 'BuildState'.
addConstraints :: BuildState -> [C.Constraint] -> Either String BuildState
addConstraints st cs =
  let os = opStruct st
      new_os = OS.addConstraints os cs
  in return $ st { opStruct = new_os }

mkVarNameForConst :: Constant -> String
mkVarNameForConst c = "%const." ++ (pShow c)

-- | Adds a new value node representing a particular constant to a given state.
addNewValueNodeWithConstant :: BuildState
                            -> Constant
                            -> Either String BuildState
addNewValueNodeWithConstant st0 c =
  do op_t <- toOpDataType c
     st1 <- addNewNode st0 $ G.ValueNode op_t (Just $ mkVarNameForConst c)
     new_n <- getLastTouchedValueNode st1
     entry_b <- getEntryBlock st1
     st2 <- addPendingBlockToDatumFlow st1 (entry_b, G.getNodeID new_n)
     return st2

-- | Adds a new edge into a given state.
addNewEdge
  :: BuildState
     -- ^ The current state.
  -> G.EdgeType
  -> G.Node
     -- ^ The source node.
  -> G.Node
     -- ^ The destination node.
  -> Either String BuildState
     -- ^ An error message or the new state.
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
  -> Either String BuildState
     -- ^ An error message or the new state.
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
  -> Either String BuildState
     -- ^ An error message or the new state.
addNewEdgesManyDests st et src dsts =
  do let es = zip (repeat src) dsts
         f g' e = fst $ G.addNewEdge et e g'
         g = foldl f (getOSGraph st) es
     updateOSGraph st g

-- | Adds a new symbol-to-node mapping to a given state.
addSymMap :: BuildState -> SymToValueNodeMapping -> Either String BuildState
addSymMap st sm = return $ st { symMaps = sm:(symMaps st) }

-- | Adds block-to-datum flow to a given state.
addPendingBlockToDatumFlow
  :: BuildState
  -> PendingBlockToDatumFlow
  -> Either String BuildState
addPendingBlockToDatumFlow st flow =
  return $ st { blockToDatumDataFlows = flow:(blockToDatumDataFlows st) }

-- | Adds block-to-datum definition to a given state.
addPendingBlockToDatumDef :: BuildState
                          -> PendingBlockToDatumDef
                          -> Either String BuildState
addPendingBlockToDatumDef st def =
  return $ st { blockToDatumDefs = def:(blockToDatumDefs st) }

-- | Adds datum-to-block definition to a given state.
addPendingDatumToBlockDef :: BuildState
                          -> PendingDatumToBlockDef
                          -> Either String BuildState
addPendingDatumToBlockDef st def =
  return $ st { datumToBlockDefs = def:(datumToBlockDefs st) }

-- | Adds a value node representing a function argument to a given state.
addFuncInputValue :: BuildState -> G.NodeID -> Either String BuildState
addFuncInputValue st n =
  return $ st { funcInputValues = nub $ n:(funcInputValues st) }

-- | Adds a value node representing an external value of a pattern to a given
-- state.
addPatExtValue :: BuildState -> G.NodeID -> Either String BuildState
addPatExtValue st n =
  return $ st { patExtValues = nub $ n:(patExtValues st) }

-- | Finds the nodes in the graph of the givne state that matches a given node
-- ID.
findNodesWithID :: BuildState -> G.NodeID -> [G.Node]
findNodesWithID st nid = G.findNodesWithNodeID (getOSGraph st) nid

-- | Gets the node with a given ID.
getNodeWithID :: BuildState -> G.NodeID -> Either String G.Node
getNodeWithID st nid =
  do let ns = findNodesWithID st nid
     if length ns == 1
     then return $ head ns
     else if length ns == 0
          then Left $ "getNodeWithID: found no node with ID " ++ pShow nid
          else Left $ "getNodeWithID: found multiple nodes with ID " ++
                      pShow nid

-- | Gets the value node to which a symbol is mapped.
getValueNodeMappedToSym :: BuildState -> Symbol -> Either String G.Node
getValueNodeMappedToSym st sym =
  let nid = lookup sym (symMaps st)
  in if isJust nid
     then let ns = findNodesWithID st (fromJust nid)
          in if length ns == 1
             then return $ head ns
             else if length ns == 0
                  then Left $ "getValueNodeMappedToSym: found no node with " ++
                              "ID " ++ pShow (fromJust nid) ++ ", mapped " ++
                              "from symbol '" ++ pShow sym ++ "'"
                  else Left $ "getValueNodeMappedToSym: found multiple " ++
                              "nodes with ID " ++ pShow (fromJust nid) ++
                              ", mapped from symbol '" ++ pShow sym ++ "'"
     else Left $ "getValueNodeMappedToSym: found no value node mapped from " ++
                 "symbol '" ++ pShow sym ++ "'"

-- | Returns the block nodes in the graph of the given state that match a given
-- name.
findBlockNodesWithName :: BuildState -> F.BlockName -> [G.Node]
findBlockNodesWithName st name =
  let block_nodes = filter G.isBlockNode $ G.getAllNodes $ getOSGraph st
  in filter (\n -> (G.nameOfBlock $ G.getNodeType n) == name) block_nodes

-- | Gets the block node with a particular name in the graph of the given state.
getBlockNodeWithName :: BuildState -> F.BlockName -> Either String G.Node
getBlockNodeWithName st name =
  let ns = findBlockNodesWithName st name
  in if length ns == 1
     then return $ head ns
     else if length ns == 0
          then Left $ "getBlockNodeWithName: found no block node with name " ++
                      "'" ++ pShow name ++ "'"
          else Left $ "getBlockNodeWithName: found multiple block nodes " ++
                      "with name '" ++ pShow name ++ "'"

-- | Checks that a value node with a particular symbol exists in the graph of
-- the given state. If it does then the last touched node is updated
-- accordingly, otherwise a new value node with the symbol is added. A
-- corresponding mapping is also added.
ensureValueNodeWithSymExists
  :: BuildState
  -> Symbol
  -> D.DataType
     -- ^ Data type to use upon creation if such a value node does not exist.
  -> Either String BuildState
ensureValueNodeWithSymExists st0 sym dt =
  let nid = lookup sym (symMaps st0)
  in if isJust nid
     then let ns = findNodesWithID st0 (fromJust nid)
          in if length ns == 1
             then touchNode st0 (head ns)
             else if length ns == 0
                  then Left $ "ensureValueNodeWithSymExists: found no " ++
                              "value node with ID " ++ pShow (fromJust nid) ++
                              ", mapped from symbol '" ++ pShow sym ++ "'"
                  else Left $ "ensureValueNodeWithSymExists: found multiple " ++
                              "value nodes with ID " ++ pShow (fromJust nid) ++
                              ", mapped from symbol '" ++ pShow sym ++ "'"
     else do st1 <- addNewNode st0 (G.ValueNode dt (Just $ toOrigin sym))
             new_n <- getLastTouchedValueNode st1
             st2 <- addSymMap st1 (sym, G.getNodeID new_n)
             return st2

-- | Checks that a block node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated accordingly,
-- otherwise then a new block node is added.
ensureBlockNodeExists :: BuildState -> F.BlockName -> Either String BuildState
ensureBlockNodeExists st name =
  let ns = findBlockNodesWithName st name
  in if length ns == 0
     then addNewNode st (G.BlockNode name)
     else if length ns == 1
          then touchNode st (head ns)
          else Left $ "ensureBlockNodeExists: found multiple block nodes " ++
                      "with name '" ++ pShow name ++ "'"

-- | Checks that a state node has been touched. If not, then a new state node is
-- added and set as touched. Also, if this is a function graph, a pending
-- block-to-datum data-flow edge is added from the current block to the new
-- state node.
ensureStateNodeHasBeenTouched :: BuildState -> Either String BuildState
ensureStateNodeHasBeenTouched st0 =
  if isJust (lastTouchedStateNode st0)
  then return st0
  else do st1 <- addNewStateNode st0
          n <- getLastTouchedStateNode st1
          current_b <- getCurrentBlock st1
          st2 <- addPendingBlockToDatumFlow st1 (current_b, G.getNodeID n)
           -- Note that, if this is a pattern, then the flow above will not be
           -- inserted as all 'PendingBlockToDatumFlow' entities are ignored.
          return st2

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

-- | Adds the pending data or state flow edges from block nodes to data or state
-- nodes, as described in the given build state.
addPendingBlockToDatumFlowEdges :: BuildState -> Either String BuildState
addPendingBlockToDatumFlowEdges st =
  do let g0 = getOSGraph st
     deps <- mapM ( \(name, nid) ->
                    do n <- getNodeWithID st nid
                       p <- if G.isValueNode n
                            then return (name, n, G.DataFlowEdge)
                            else if G.isStateNode n
                                 then return (name, n, G.StateFlowEdge)
                                 else Left $
                                      "addPendingBlockToDatumFlowEdges: " ++
                                      show n ++ "is of unexpected type"
                       return p
                  ) $
             blockToDatumDataFlows st
     g1 <- foldM ( \g (name, n, et) ->
                   do bn <- getBlockNodeWithName st name
                      return $ fst $ G.addNewEdge et (bn, n) g
                 )
                 g0
                 deps
     updateOSGraph st g1

-- | Adds the pending block-to-datum definition edges, as described in the
-- given build state.
addPendingBlockToDatumDefEdges :: BuildState -> Either String BuildState
addPendingBlockToDatumDefEdges st =
  do let g0 = getOSGraph st
         defs = blockToDatumDefs st
     g1 <- foldM ( \g (name, dn_id, nr) ->
                   do dn <- getNodeWithID st dn_id
                      bn <- getBlockNodeWithName st name
                      let (g', new_e) = G.addNewDefEdge (bn, dn) g
                          g'' = G.updateEdgeInNr nr new_e g'
                      return g''
                 )
                 g0
                 defs
     updateOSGraph st g1

-- | Adds the pending datum-to-block definition edges, as described in the
-- given build state.
addPendingDatumToBlockDefEdges :: BuildState -> Either String BuildState
addPendingDatumToBlockDefEdges st =
  do let g0 = getOSGraph st
         defs = datumToBlockDefs st
     g1 <- foldM ( \g (dn_id, name, nr) ->
                   do dn <- getNodeWithID st dn_id
                      bn <- getBlockNodeWithName st name
                      let (g', new_e) = G.addNewDefEdge (dn, bn) g
                          g'' = G.updateEdgeOutNr nr new_e g'
                      return g''
                 )
                 g0
                 defs
     updateOSGraph st g1

-- | Gets the LLVM instruction from a named expression.
fromNamed :: LLVM.Named i -> i
fromNamed (_ LLVM.:= i) = i
fromNamed (LLVM.Do i) = i

-- | Applies various transformations on the 'OpStructure', such as
-- canonicalizing copy operations.
applyOSTransformations :: BuildState -> Either String BuildState
applyOSTransformations st =
  let os0 = opStruct st
      b2ddfs0 = blockToDatumDataFlows st
      os1 = OS.canonicalizeCopies os0
      -- Canonicalizing copies will remove value nodes with constants, which
      -- must then be removed from the book keeping held within the build state.
      b2ddfs1 = filter ( \(_, nid) ->
                         length (G.findNodesWithNodeID (OS.osGraph os1) nid) > 0
                       )
                       b2ddfs0
  in return $ st { opStruct = os1
                 , blockToDatumDataFlows = b2ddfs1
                 }

-- | Removes all block nodes from a 'BuildState' which have no edges.
removeUnusedBlockNodes :: BuildState -> Either String BuildState
removeUnusedBlockNodes st =
  do let g = getOSGraph st
         ns = filter G.isBlockNode $ G.getAllNodes g
         unused = filter (\n -> length (G.getNeighbors g n) == 0) ns
         new_g = foldr G.delNode g unused
     new_st <- updateOSGraph st new_g
     let entry_name = entryBlock st
         entry_ns = if isJust entry_name
                    then findBlockNodesWithName new_st (fromJust entry_name)
                    else []
     return $ if length entry_ns > 0
              then new_st
              else let new_os = opStruct new_st
                   in new_st { opStruct =
                                 new_os { OS.osEntryBlockNode = Nothing }
                             , entryBlock = Nothing
                             }

-- | Converts an 'LLVM.Named' entity into a 'String'.
nameToString :: LLVM.Name -> String
nameToString (LLVM.Name str) = str
nameToString (LLVM.UnName int) = show int

-- | Converts a 'Symbol' into 'String' to be used as origin for value nodes.
toOrigin :: Symbol -> String
toOrigin = pShow

-- | Replaces all occurrances of a given 'G.NodeID' in a given state with
-- another 'G.NodeID'.
replaceNodeIDInBuildState
  :: G.NodeID
     -- ^ The node ID to be replaced.
  -> G.NodeID
     -- ^ The node ID to replace with.
  -> BuildState
     -- ^ The build state to update.
  -> Either String BuildState
replaceNodeIDInBuildState old_nid new_nid st0 =
  let st1 = st0 { blockToDatumDataFlows =
                    map ( \old@(b', nid) ->
                          if old_nid == nid then (b', new_nid) else old
                        ) $
                    blockToDatumDataFlows st0
                }
      st2 = st1 { blockToDatumDefs =
                    map ( \old@(b', n, nr) ->
                          if old_nid == n then (b', new_nid, nr) else old
                        ) $
                    blockToDatumDefs st1
                }
      st3 = st2 { datumToBlockDefs =
                    map ( \old@(n, b', nr) ->
                          if old_nid == n then (new_nid, b', nr) else old
                        ) $
                    datumToBlockDefs st2
                }
      st4 = st3 { funcInputValues =
                    map (\n -> if old_nid == n then new_nid else n) $
                    funcInputValues st3
                }

      st5 = st4 { patExtValues =
                    map (\n -> if old_nid == n then new_nid else n) $
                    patExtValues st4
                }
      st6 = st5 { symMaps =
                    map ( \old@(sym, n) ->
                          if old_nid == n then (sym, new_nid) else old
                        ) $
                    symMaps st5
                }
  in return st6
