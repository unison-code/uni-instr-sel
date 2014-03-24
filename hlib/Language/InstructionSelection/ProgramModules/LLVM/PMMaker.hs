--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM.PMMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts and LLVM IR module into the internal program format.
--
-- TODO: add data type information to data nodes
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.LLVM.PMMaker (
  mkProgramModule
) where

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.IntegerPredicate as LLVMI
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.ProgramModules.Base as PM
import Language.InstructionSelection.DataTypes
import Language.InstructionSelection.PrettyPrint
import Language.InstructionSelection.Utils
import Data.Maybe



mkProgramModule :: LLVM.Module -> PM.Module
mkProgramModule m =
  let funcs = concatMap mkFunction $ LLVM.moduleDefinitions m
  in PM.Module funcs

mkFunction :: LLVM.Definition -> [PM.Function]
mkFunction (LLVM.GlobalDefinition g) = fromGlobalDef g
mkFunction _ = []

fromGlobalDef :: LLVM.Global -> [PM.Function]
fromGlobalDef (LLVM.Function _ _ _ _ _ (LLVM.Name name) _ _ _ _ _ bbs) =
  let (os, _, _) = (extend (OS.empty, G.BBLabel "", []) bbs)
  in [PM.Function name os]
fromGlobalDef _ = []

data Symbol
    = LocalStringSymbol String
    | GlobalStringSymbol String
    | TemporarySymbol Integer
    deriving (Eq)
instance Show Symbol where
  show (LocalStringSymbol str) = "%" ++ str
  show (GlobalStringSymbol str) = "@" ++ str
  show (TemporarySymbol int) = "t" ++ show int

class SymbolFormable a where
  toSymbol :: a -> Symbol
instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = LocalStringSymbol str
  toSymbol (LLVM.UnName int) = TemporarySymbol $ toInteger int

type Mapping = (G.NodeId, Symbol)
type State = ( OS.OpStructure       -- ^ The corresponding operation structure.
             , G.BBLabel            -- ^ Current label that the process is in.
             , [Mapping]            -- ^ List of node-to-symbol mappings. If
                                    -- there are more than one mapping using the
                                    -- same symbol, then the last one occuring
                                    -- in the list should be picked.
             )

class LlvmToOS a where

  -- | Extends a given 'OpStructure' with the information given by an LLVM data
  -- type. The first is given via a tuple which contains other auxiliary
  -- information needed for this process.
  --
  -- NOTE: Each declaration node and use node will be kept separate to be merged
  -- as a post-step.

  extend :: State    -- ^ Current state.
            -> a     -- ^ The LLVM data type to process.
            -> State -- ^The new, extended state.

currentOS :: State -> OS.OpStructure
currentOS (os, _, _) = os
updateOS :: State -> OS.OpStructure -> State
updateOS (_, l, ms) os = (os, l, ms)
currentGraph :: State -> G.Graph
currentGraph = OS.graph . currentOS
updateGraph :: State -> G.Graph -> State
updateGraph t g = updateOS t $ OS.updateGraph (currentOS t) g
newNodeId :: State -> G.NodeId
newNodeId = G.newNodeId . currentGraph
addNewNodeWithNL :: State -> G.NodeLabel -> State
addNewNodeWithNL t nl = updateGraph t $ G.addNewNode nl (currentGraph t)
addNewNodeWithNI :: State -> G.NodeInfo -> State
addNewNodeWithNI t ni = addNewNodeWithNL t (G.NodeLabel (newNodeId t) ni)
lastAddedNode :: State -> Maybe G.Node
lastAddedNode = G.lastAddedNode . currentGraph
addNewEdge :: State
              -> G.Node -- ^ Source node.
              -> G.Node -- ^ Destination node.
              -> State
addNewEdge t src dst = updateGraph t $ G.addNewEdge (src, dst) (currentGraph t)
addNewEdgesManySources :: State
                          -> [G.Node] -- ^ Source nodes.
                          -> G.Node   -- ^ Destination node.
                          -> State
addNewEdgesManySources t srcs dst =
  let es = zip srcs (repeat dst)
  in updateGraph t $ foldl (flip G.addNewEdge) (currentGraph t) es
addConstraint :: State -> OS.Constraint -> State
addConstraint t c = updateOS t $ OS.addConstraint (currentOS t) c
currentLabel :: State -> G.BBLabel
currentLabel (_, l, _) = l
updateLabel :: State -> G.BBLabel -> State
updateLabel (os, _, ms) l = (os, l, ms)
currentMappings :: State -> [Mapping]
currentMappings (_, _, ms) = ms
addMapping :: State -> Mapping -> State
addMapping (os, l, ms) m = (os, l, ms ++ [m])

nodeIdFromSym :: [Mapping] -> Symbol -> Maybe G.NodeId
nodeIdFromSym ms sym =
  let ns = filter (\t -> snd t == sym) ms
  in if not $ null ns
        then Just $ fst $ last ns
        else Nothing

instance (LlvmToOS a) => LlvmToOS [a] where
  extend = foldl extend

instance (LlvmToOS n) => LlvmToOS (LLVM.Named n) where
  extend t (name LLVM.:= expr) =
    let t1 = extend t name
        dst_node = fromJust $ lastAddedNode t1
        t2 = extend t1 expr
        expr_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 expr_node dst_node
    in t3
  extend t (LLVM.Do expr) = extend t expr

instance LlvmToOS LLVM.BasicBlock where
  extend t (LLVM.BasicBlock (LLVM.Name l) insts term_inst) =
    let t1 = updateLabel t (G.BBLabel l)
        t2 = foldl extend t1 insts
        t3 = extend t2 term_inst
    in t3

instance LlvmToOS LLVM.Name where
  extend t name@(LLVM.Name str) =
    let sym = toSymbol name
        existing_nid = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id existing_nid
        t1 = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo (G.NTData Nothing)
                                                  (currentLabel t)
                                                  (show sym)))
        t2 = maybe (addMapping t1 (nid, sym)) (\_ -> t1) existing_nid
    in t2

  extend t name@(LLVM.UnName int) =
    let sym = toSymbol name
        existing_nid = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id existing_nid
        t1 = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo (G.NTData Nothing)
                                                  (currentLabel t)
                                                  (show sym)))
        t2 = maybe (addMapping t1 (nid, sym)) (\_ -> t1) existing_nid
    in t2

insertBinaryOp :: (LlvmToOS op) => State -> Op.BinaryOp -> op -> op -> State
insertBinaryOp t op lhs rhs =
  let t1 = extend t lhs
      lhs_node = fromJust $ lastAddedNode t1
      t2 = extend t1 rhs
      rhs_node = fromJust $ lastAddedNode t2
      t3 = addNewNodeWithNI t2 (G.NodeInfo (G.NTBinaryOp op)
                                (currentLabel t2) (prettyShow op))
      op_node = fromJust $ lastAddedNode t3
      t4 = addNewEdgesManySources t3 [lhs_node, rhs_node] op_node
  in t4

instance LlvmToOS LLVM.Instruction where
  extend t (LLVM.Add nsw nuw op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.IAdd) op1 op2
  extend t (LLVM.FAdd op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.FAdd) op1 op2
  extend t (LLVM.Sub nsw nuw op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.ISub) op1 op2
  extend t (LLVM.FSub op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.FSub) op1 op2
  extend t (LLVM.Mul nsw nuw op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.IMul) op1 op2
  extend t (LLVM.FMul op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.FMul) op1 op2
  extend t (LLVM.UDiv exact op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.IUDiv) op1 op2
  extend t (LLVM.SDiv exact op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.ISDiv) op1 op2
  extend t (LLVM.FDiv op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.FDiv) op1 op2
  extend t (LLVM.URem op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.IURem) op1 op2
  extend t (LLVM.SRem op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.ISRem) op1 op2
  extend t (LLVM.FRem op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.FRem) op1 op2
  extend t (LLVM.Shl nsw nuw op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.Shl) op1 op2
  extend t (LLVM.LShr exact op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.LShr) op1 op2
  extend t (LLVM.AShr exact op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.AShr) op1 op2
  extend t (LLVM.And op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.And) op1 op2
  extend t (LLVM.Or op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.Or) op1 op2
  extend t (LLVM.Xor op1 op2 _) =
    insertBinaryOp t (Op.BinArithmeticOp Op.Xor) op1 op2
  extend t l@(LLVM.Load vol addr atom align _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Store vol addr val atom align _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.ICmp iPred op1 op2 _) =
    insertBinaryOp t (Op.BinCompareOp $ toICmp iPred) op1 op2
  extend t (LLVM.FCmp fPred op1 op2 _) =
    insertBinaryOp t (Op.BinCompareOp $ toFCmp fPred) op1 op2
  extend t (LLVM.Phi typ ins _) =
    let f (ns, local_t) e =
          let local_t1 = processPhiElem local_t e
              n = fromJust $ lastAddedNode local_t1
          in (ns ++ [n], local_t1)
        (data_nodes, t1) = foldl f ([], t) ins
        t2 = addNewNodeWithNI t1 (G.NodeInfo G.NTPhi (currentLabel t1) "phi")
        phi_node = fromJust $ lastAddedNode t2
        t3 = addNewEdgesManySources t2 data_nodes phi_node
        t4 = insertAndConnectDataNode t3
    in t4
  extend t l@(LLVM.Call isTail cc retAttr funOp args funAttrs _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Select cond trueV falseV _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l = error $ "'extend' not implemented for " ++ show l

processPhiElem :: State -> (LLVM.Operand, LLVM.Name) -> State
processPhiElem t (op, _) = extend t op
  -- TODO: what to do with label on the element?

insertAndConnectDataNode :: State -> State
insertAndConnectDataNode t =
  let op_node = fromJust $ lastAddedNode t
      t1 = addNewNodeWithNI t (G.NodeInfo (G.NTData Nothing) (currentLabel t)
                               "")
      d_node = fromJust $ lastAddedNode t1
      t2 = addNewEdge t1 op_node d_node
  in t2

toICmp :: LLVMI.IntegerPredicate -> Op.CompareOp
toICmp LLVMI.EQ = Op.ICmpEq
toICmp LLVMI.NE = Op.ICmpNEq
toICmp LLVMI.UGT = Op.IUCmpGT
toICmp LLVMI.ULT = Op.IUCmpLT
toICmp LLVMI.UGE = Op.IUCmpGE
toICmp LLVMI.ULE = Op.IUCmpLE
toICmp LLVMI.SGT = Op.ISCmpGT
toICmp LLVMI.SLT = Op.ISCmpLT
toICmp LLVMI.SGE = Op.ISCmpGE
toICmp LLVMI.SLE = Op.ISCmpLE

toFCmp :: LLVMF.FloatingPointPredicate -> Op.CompareOp
toFCmp LLVMF.False = Op.FFalse
toFCmp LLVMF.OEQ = Op.FOCmpEq
toFCmp LLVMF.OGT = Op.FOCmpGT
toFCmp LLVMF.OGE = Op.FOCmpGE
toFCmp LLVMF.OLT = Op.FOCmpLT
toFCmp LLVMF.OLE = Op.FOCmpLE
toFCmp LLVMF.ONE = Op.FOCmpNEq
toFCmp LLVMF.ORD = Op.FCmpOrd
toFCmp LLVMF.UNO = Op.FCmpUn
toFCmp LLVMF.UEQ = Op.FUCmpEq
toFCmp LLVMF.UGT = Op.FUCmpGT
toFCmp LLVMF.UGE = Op.FUCmpGE
toFCmp LLVMF.ULT = Op.FUCmpLT
toFCmp LLVMF.ULE = Op.FUCmpLE
toFCmp LLVMF.UNE = Op.FUCmpNEq
toFCmp LLVMF.True = Op.FTrue

instance LlvmToOS LLVM.Terminator where
  extend t (LLVM.Ret op _) =
    let t1 = addNewNodeWithNI t (G.NodeInfo G.NTRet (currentLabel t) "ret")
        ret_node = fromJust $ lastAddedNode t1
        t2 = if isJust op
                then let t3 = extend t1 $ fromJust op
                         op_node = fromJust $ lastAddedNode t3
                     in addNewEdge t3 op_node ret_node
                else t1
    in t2
  extend t l@(LLVM.Br dst _) = error $ "'extend' not implemented for " ++ show l
  extend t (LLVM.CondBr op (LLVM.Name trueDst) (LLVM.Name falseDst) _) =
    let t1 = extend t op
        op_node = fromJust $ lastAddedNode t1
        t2 = addNewNodeWithNI t1 (G.NodeInfo (G.NTCondBranch (G.BBLabel trueDst)
                                              (G.BBLabel falseDst))
                                  (currentLabel t1) "br")
        br_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 op_node br_node
    in t3
  extend t l@(LLVM.IndirectBr op dsts _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Switch op defDst dsts _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Invoke cc retAttr funOp args funAttr retDst expDst _) =
    error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Resume op _) = error $ "'extend' not implemented for " ++ show l
  extend t l@(LLVM.Unreachable _) = error $ "'extend' not implemented for " ++ show l
  extend t l = error $ "'extend' not implemented for " ++ show l
  -- TODO: implement these functions

instance LlvmToOS LLVM.Operand where
  extend t (LLVM.LocalReference name) = extend t name
  extend t (LLVM.ConstantOperand c) = extend t c

instance LlvmToOS LLVMC.Constant where
  extend t (LLVMC.Int b v) =
    let t1 = addNewNodeWithNI t (G.NodeInfo
                                 (G.NTData $ Just $ fromIWidth b)
                                 (currentLabel t) (show v))
        n = fromJust $ lastAddedNode t1
        t2 = addConstraint t1 (OS.ConstantValueConstraint (G.nodeId n)
                                [Range (OS.IntConstant v) (OS.IntConstant v)])
    in t2

  extend t l = error $ "'extend' not implemented for " ++ show l
