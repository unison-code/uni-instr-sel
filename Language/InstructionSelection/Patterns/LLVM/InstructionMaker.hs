--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.LLVM.InstructionMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts an LLVM pattern into the internal instruction format.
--
-- TODO: Proper edges are not drawn to indicate which value is actually used
-- when the value is reflected through a register symbol.
--------------------------------------------------------------------------------



module Language.InstructionSelection.Patterns.LLVM.InstructionMaker (
  mkInstruction
) where

import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.Patterns.Base as P
import qualified Language.InstructionSelection.Patterns.LLVM.Base as LLVM
import Language.InstructionSelection.Utils
import Data.Maybe



mkInstruction :: LLVM.Instruction -> P.Instruction
mkInstruction (LLVM.Instruction ass ps) =
  let sts = map (extend (OS.empty, G.BBLabel "", [])) ps
      oss = map (\(os, _, _) -> os) sts
  in P.Instruction ass oss

data Symbol
    = StringSymbol String
    | TemporarySymbol Integer
    deriving (Show, Eq)

class SymbolFormable a where
  toSymbol :: a -> Symbol
instance SymbolFormable LLVM.Symbol where
  toSymbol (LLVM.Symbol str) = StringSymbol str
instance SymbolFormable LLVM.Temporary where
  toSymbol (LLVM.Temporary int) = TemporarySymbol int
instance (SymbolFormable a, SymbolFormable b) =>
         SymbolFormable (Either a b) where
  toSymbol (Left l) = toSymbol l
  toSymbol (Right r) = toSymbol r
instance SymbolFormable LLVM.AliasValue where
  toSymbol (LLVM.AVTemporary temp) = toSymbol temp
  toSymbol (LLVM.AVSymbol sym) = toSymbol sym
  toSymbol s = error $ "Unmatched to symbol " ++ (show s)

class RegisterFormable a where
  toRegisters :: a -> [OS.Register]
instance RegisterFormable LLVM.DataSpace where
  toRegisters (LLVM.DSRegisterClass rc) = toRegisters rc
  toRegisters s = error $ "Not handling " ++ (show s) ++ " yet"
  -- TODO: handle memory space
instance RegisterFormable LLVM.RegisterClass where
  toRegisters (LLVM.RegisterClass regs) = concatMap toRegisters regs
instance RegisterFormable LLVM.Register where
  toRegisters (LLVM.Register str) = [OS.Register str]

class ConstRangeFormable a where
  toConstRange :: a -> (Range OS.Constant)
instance (Integral i) => ConstRangeFormable (Range i) where
  toConstRange (Range lower upper) = Range (OS.IntConstant $ toInteger lower)
                                     (OS.IntConstant $ toInteger upper)

toRegisterFlag :: LLVM.RegisterFlag -> OS.RegisterFlag
toRegisterFlag (LLVM.RegisterFlag flag reg) =
  OS.RegisterFlag flag (head $ toRegisters reg)

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

instance LlvmToOS LLVM.Pattern where

  -- | After the entire LLVM AST has been processed, the alias constraints are
  -- handled a bit differently from the other constraints. The constraints used
  -- in the internal pattern all refer to a specific node in the graph, but some
  -- of the symbols appearing in the alias list will have no such corresponding
  -- node. We solve this by adding additional mappings by letting them refer to
  -- nodes corresponding to some other symbol appearing in the alias list, and
  -- then removing the no-node symbols from the list.

  extend t (LLVM.Pattern stmts cons) =
    let t1 = foldl extend t stmts
        aliases = concatMap (\(LLVM.AliasesConstraint as) -> as)
                  $ filter LLVM.isAliasesConstraint cons
        t2 = addMappingsForNoNodeSymbols t1 aliases
        t3 = foldl extend t2 cons
    in t3

addMappingsForNoNodeSymbols :: State -> [[LLVM.AliasValue]] -> State
addMappingsForNoNodeSymbols t aliases =
  foldl addMappingsForNoNodeSymbols' t aliases
addMappingsForNoNodeSymbols' :: State -> [LLVM.AliasValue] -> State
addMappingsForNoNodeSymbols' t [] = t
addMappingsForNoNodeSymbols' t (_:[]) = t
addMappingsForNoNodeSymbols' t avs =
  let syms = map toSymbol avs
      toNodeId = nodeIdFromSym (currentMappings t)
      hasNodeId = isJust . toNodeId
      syms_with_no_node = filter (not . hasNodeId) syms
      nid = fromJust . toNodeId . head $ filter hasNodeId syms
  in foldl addMapping t $ zip (repeat nid) syms_with_no_node

instance LlvmToOS LLVM.Statement where
  extend t (LLVM.AssignmentStmt tmp expr) =
    let t1 = extend t tmp
        dst_node = fromJust $ lastAddedNode t1
        t2 = extend t1 expr
        expr_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 expr_node dst_node
    in t3

  extend t (LLVM.SetRegStmt dst expr) =
    let t1 = extend t dst
        dst_node = fromJust $ lastAddedNode t1
        t2 = extend t1 expr
        expr_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 expr_node dst_node
    in t3

  extend t (LLVM.StoreStmt addr_expr _ _ value_expr) =
    let t1 = extend t addr_expr
        addr_node = fromJust $ lastAddedNode t1
        t2 = extend t1 value_expr
        value_node = fromJust $ lastAddedNode t2
        t3 = addNewNodeWithNI t2 (G.NodeInfo G.NTMemoryStore
                                  (currentLabel t2) "store")
        store_node = fromJust $ lastAddedNode t3
        t4 = addNewEdgesManySources t3 [addr_node, value_node] store_node
    in t4

  extend t (LLVM.UncondBranchStmt (LLVM.Label l)) =
    addNewNodeWithNI t (G.NodeInfo (G.NTUncondBranch (G.BBLabel l))
                        (currentLabel t) "br")

  extend t (LLVM.CondBranchStmt reg (LLVM.Label true_l) (LLVM.Label false_l)) =
    let t1 = extend t reg
        reg_node = fromJust $ lastAddedNode t1
        t2 = addNewNodeWithNI t1 (G.NodeInfo (G.NTCondBranch (G.BBLabel true_l)
                                              (G.BBLabel false_l))
                                  (currentLabel t1) "br")
        br_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 reg_node br_node
    in t3

  extend t (LLVM.LabelStmt (LLVM.Label l)) = updateLabel t (G.BBLabel l)

instance LlvmToOS LLVM.SetRegDestination where
  extend t (LLVM.SRDRegister reg) = extend t reg
  extend t (LLVM.SRDRegisterFlag flag) = extend t flag
  extend t (LLVM.SRDSymbol sym) = extend t sym
  extend t (LLVM.SRDTemporary temp) = extend t temp

instance LlvmToOS LLVM.Temporary where
  extend t temp@(LLVM.Temporary int) =
    let sym = toSymbol temp
        existing_nid = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id existing_nid
        t1 = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t)
                                                  ("t" ++ show int)))
        t2 = maybe (addMapping t1 (nid, sym)) (\_ -> t1) existing_nid
    in t2

instance LlvmToOS LLVM.Register where
  extend t (LLVM.Register str) =
    let t1 = addNewNodeWithNI t (G.NodeInfo G.NTData (currentLabel t) "")
        n = fromJust $ lastAddedNode t1
        t2 = addConstraint t1 (OS.AllocateInRegisterConstraint (G.nodeId n)
                                [OS.Register str])
    in t2

instance LlvmToOS LLVM.RegisterFlag where
  -- TODO: implement

instance LlvmToOS LLVM.Symbol where
  extend t llvm_sym@(LLVM.Symbol str) =
    let nid = newNodeId t
        t1 = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t) str))
        sym = toSymbol llvm_sym
        existing_nid = nodeIdFromSym (currentMappings t) sym
        t2 = if isNothing existing_nid
                 then addMapping t1 (nid, sym)
              else addConstraint t1 (OS.AssignSameRegisterConstraint nid
                                     (fromJust existing_nid))
    in t2

instance LlvmToOS LLVM.StmtExpression where
  extend t (LLVM.BinaryOpStmtExpr op _ lhs rhs) =
    let t1 = extend t lhs
        lhs_node = fromJust $ lastAddedNode t1
        t2 = extend t1 rhs
        rhs_node = fromJust $ lastAddedNode t2
        t3 = addNewNodeWithNI t2 (G.NodeInfo (G.NTBinaryOp op)
                                  (currentLabel t2) (Op.tochars op))
        op_node = fromJust $ lastAddedNode t3
        t4 = addNewEdgesManySources t3 [lhs_node, rhs_node] op_node
    in t4

  extend t (LLVM.UnaryOpStmtExpr op _ expr) =
    let t1 = extend t expr
        expr_node = fromJust $ lastAddedNode t1
        t2 = addNewNodeWithNI t1 (G.NodeInfo (G.NTUnaryOp op)
                                  (currentLabel t1) (Op.tochars op))
        op_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 expr_node op_node
    in t3

  extend t (LLVM.LoadStmtExpr _ _ expr) =
    let t1 = extend t expr
        expr_node = fromJust $ lastAddedNode t1
        t2 = addNewNodeWithNI t1 (G.NodeInfo G.NTMemoryLoad
                                   (currentLabel t1) "load")
        op_node = fromJust $ lastAddedNode t2
        t3 = addNewEdge t2 expr_node op_node
    in t3

  extend t (LLVM.PhiStmtExpr phis) =
    let f (ns, local_t) e =
          let local_t1 = extend local_t e
              n = fromJust $ lastAddedNode local_t1
          in (ns ++ [n], local_t1)
        (data_nodes, t1) = foldl f ([], t) phis
        t2 = addNewNodeWithNI t1 (G.NodeInfo G.NTPhi (currentLabel t1) "phi")
        phi_node = fromJust $ lastAddedNode t2
        t3 = addNewEdgesManySources t2 data_nodes phi_node
        t4 = insertAndConnectDataNode t3
        last_node = fromJust $ lastAddedNode t4
        makeSameRegs lt (n1, n2) = addConstraint lt
                                   (OS.AssignSameRegisterConstraint
                                    (G.nodeId n1) (G.nodeId n2))
        t5 = foldl makeSameRegs t4 $ zip (repeat last_node) data_nodes
    in t5

  extend t (LLVM.DataStmtExpr d) = extend t d

  extend t (LLVM.SizeStmtExpr reg) = extend t reg

  extend t (LLVM.RegRangeStmtExpr val (Range lo up)) =
    let t1 = extend t val
        val_node = fromJust $ lastAddedNode t1
        t2 = extend t1 lo
        lo_node = fromJust $ lastAddedNode t2
        shift_op = Op.BinArithmeticOp Op.LShr
        t3 = addNewNodeWithNI t2 (G.NodeInfo (G.NTBinaryOp shift_op)
                                  (currentLabel t2) (show shift_op))
        shift_node = fromJust $ lastAddedNode t3
        t4 = addNewEdgesManySources t3 [val_node, lo_node] shift_node
        t5 = insertAndConnectDataNode t4
        d_node = fromJust $ lastAddedNode t5
        t6 = insertMaskNodesForRegRange t5 lo up
        mask_node = fromJust $ lastAddedNode t6
        and_op = Op.BinArithmeticOp Op.And
        t7 = addNewNodeWithNI t6 (G.NodeInfo (G.NTBinaryOp and_op)
                                             (currentLabel t6)
                                             (show and_op))
        and_node = fromJust $ lastAddedNode t7
        t8 = addNewEdgesManySources t7 [d_node, mask_node] and_node
    in t8

insertMaskNodesForRegRange :: State -> LLVM.ProgramData -> LLVM.ProgramData
                              -> State
insertMaskNodesForRegRange t lo up =
  let t1 = extend t up
      up_node = fromJust $ lastAddedNode t1
      t2 = extend t1 lo
      lo_node = fromJust $ lastAddedNode t2
      op1 = Op.BinArithmeticOp Op.ISub
      t3 = addNewNodeWithNI t2 (G.NodeInfo (G.NTBinaryOp op1)
                                (currentLabel t2) (show op1))
      op1_node = fromJust $ lastAddedNode t3
      t4 = addNewEdgesManySources t3 [up_node, lo_node] op1_node
      t5 = insertAndConnectDataNode t4
      d1_node = fromJust $ lastAddedNode t5
      t6 = extend t5 (LLVM.ConstIntValue 1)
      c1_node = fromJust $ lastAddedNode t6
      op2 = Op.BinArithmeticOp Op.IAdd
      t7 = addNewNodeWithNI t6 (G.NodeInfo (G.NTBinaryOp op2) (currentLabel t6)
                                (show op2))
      op2_node = fromJust $ lastAddedNode t7
      t8 = addNewEdgesManySources t7 [d1_node, c1_node] op2_node
      t9 = insertAndConnectDataNode t8
      d2_node = fromJust $ lastAddedNode t9
      t10 = extend t9 (LLVM.ConstIntValue 1)
      c2_node = fromJust $ lastAddedNode t10
      op3 = Op.BinArithmeticOp Op.Shl
      t11 = addNewNodeWithNI t10 (G.NodeInfo (G.NTBinaryOp op3)
                                  (currentLabel t10) (show op3))
      op3_node = fromJust $ lastAddedNode t11
      t12 = addNewEdgesManySources t11 [c2_node, d2_node] op3_node
      t13 = insertAndConnectDataNode t12
      d3_node = fromJust $ lastAddedNode t13
      t14 = extend t13 (LLVM.ConstIntValue 1)
      c3_node = fromJust $ lastAddedNode t14
      op4 = Op.BinArithmeticOp Op.ISub
      t15 = addNewNodeWithNI t14 (G.NodeInfo (G.NTBinaryOp op4)
                                  (currentLabel t14) (show op4))
      op4_node = fromJust $ lastAddedNode t15
      t16 = addNewEdgesManySources t15 [d3_node, c3_node] op4_node
      t17 = insertAndConnectDataNode t16
  in t17

insertAndConnectDataNode :: State -> State
insertAndConnectDataNode t =
  let op_node = fromJust $ lastAddedNode t
      t1 = addNewNodeWithNI t (G.NodeInfo G.NTData (currentLabel t) "")
      d_node = fromJust $ lastAddedNode t1
      t2 = addNewEdge t1 op_node d_node
  in t2

-- TODO: implement
-- LLVM.FP2IStmtExpr ExprResultSize StmtExpression ExprResultSize
-- LLVM.TruncStmtExpr ExprResultSize StmtExpression ExprResultSize


instance LlvmToOS LLVM.PhiElement where
  extend t (LLVM.PhiElement expr l) = extend t expr
  -- TODO: what to do with label on the expression?

instance LlvmToOS LLVM.ProgramData where
  extend t (LLVM.PDConstant c) = extend t c
  extend t (LLVM.PDTemporary temp) = extend t temp
  extend t (LLVM.PDRegister reg) = extend t reg
  extend t (LLVM.PDSymbol sym) = extend t sym
  -- TODO: what to do with PDNoValue?

instance LlvmToOS LLVM.ProgramStorage where
  extend t (LLVM.PSTemporary temp) = extend t temp
  extend t (LLVM.PSRegister reg) = extend t reg
  extend t (LLVM.PSSymbol sym) = extend t sym

instance LlvmToOS LLVM.ConstantValue where
  extend t (LLVM.ConstIntValue val) =
    let t1 = addNewNodeWithNI t (G.NodeInfo G.NTData (currentLabel t)
                                 (show val))
        n = fromJust $ lastAddedNode t1
        t2 = addConstraint t1 (OS.ConstantValueConstraint (G.nodeId n)
                                [Range (OS.IntConstant val)
                                 (OS.IntConstant val)])
    in t2

instance (LlvmToOS l, LlvmToOS r) => LlvmToOS (Either l r) where
  extend t (Left l) = extend t l
  extend t (Right r) = extend t r

instance LlvmToOS LLVM.Constraint where
  extend t (LLVM.AllocateInConstraint store space) =
    let nid = nodeIdFromSym (currentMappings t) (toSymbol store)
        regs = toRegisters space
    in if isJust nid
          then addConstraint t $ OS.AllocateInRegisterConstraint (fromJust nid)
               regs
       else t
  extend t (LLVM.RegFlagConstraint flag ranges) =
    addConstraint t $ OS.RegFlagConstraint (toRegisterFlag flag)
                      (map toConstRange ranges)
  extend t (LLVM.ImmediateConstraint imm ranges) =
    let nid = fromJust $ nodeIdFromSym (currentMappings t) (toSymbol imm)
        const_ranges = map toConstRange ranges
    in addConstraint t $ OS.ConstantValueConstraint nid const_ranges
  extend t (LLVM.AliasesConstraint as) =
    let f localT avs =
          let only_values = filter (not . LLVM.isAVNoValue) avs
              ns = mapMaybe (nodeIdFromSym (currentMappings localT) . toSymbol)
                   only_values
          in addConstraint localT $ OS.AliasConstraint ns
    in foldl f t as
  -- TODO: handle address constraints
