--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.LLVM.OSMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts an LLVM pattern into the internal operation structure-based pattern
-- format.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Patterns.LLVM.OSMaker (
  mkOpStructure
) where

import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.Patterns.LLVM as LLVM
import qualified Language.InstructionSelection.Graphs as G
import Language.InstructionSelection.Utils
import Data.Maybe
import Debug.Trace



mkOpStructure :: LLVM.Pattern -> OS.OpStructure
mkOpStructure llvm =
  let (os, _, _) = extend (OS.empty, G.BBLabel "", []) llvm
  in os

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
instance SymbolFormable (Either LLVM.Temporary LLVM.Symbol) where
  toSymbol (Left temp) = toSymbol temp
  toSymbol (Right sym) = toSymbol sym
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
instance ConstRangeFormable (Range Integer) where
  toConstRange (Range lower upper) = Range (OS.IntConstant lower)
                                     (OS.IntConstant upper)

toRegisterFlag :: LLVM.RegisterFlag -> OS.RegisterFlag
toRegisterFlag (LLVM.RegisterFlag flag reg) =
  OS.RegisterFlag flag (head $ toRegisters reg)

type Mapping = (G.NodeId, Symbol)
type State = ( OS.OpStructure       -- ^ The corresponding operation structure.
             , G.BBLabel            -- ^ Current label that the process is in.
             , [Mapping]            -- ^ List of node-to-symbol mappings.
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
    let t' = foldl extend t stmts
        aliases = concatMap (\(LLVM.AliasesConstraint as) -> as)
                  $ filter LLVM.isAliasesConstraint cons
        t'' = addMappingsForNoNodeSymbols t' aliases
        t''' = foldl extend t'' cons
    in t'''

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
    let t' = extend t tmp
        dst_node = fromJust $ lastAddedNode t'
        t'' = extend t' expr
        expr_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' expr_node dst_node
    in t'''

  extend t (LLVM.SetRegStmt dst expr) =
    let t' = extend t dst
        dst_node = fromJust $ lastAddedNode t'
        t'' = extend t' expr
        expr_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' expr_node dst_node
    in t'''

  extend t (LLVM.StoreStmt addr_expr _ _ value_expr) =
    let t' = extend t addr_expr
        addr_node = fromJust $ lastAddedNode t'
        t'' = extend t' value_expr
        value_node = fromJust $ lastAddedNode t''
        t''' = addNewNodeWithNI t'' (G.NodeInfo G.NTMemoryStore
                                     (currentLabel t'') "store")
        store_node = fromJust $ lastAddedNode t'''
        t'''' = addNewEdgesManySources t''' [addr_node, value_node] store_node
    in t''''

  extend t (LLVM.UncondBranchStmt (LLVM.Label l)) =
    addNewNodeWithNI t (G.NodeInfo (G.NTUncondBranch (G.BBLabel l))
                        (currentLabel t) "br")

  extend t (LLVM.CondBranchStmt reg (LLVM.Label true_l) (LLVM.Label false_l)) =
    let t' = extend t reg
        reg_node = fromJust $ lastAddedNode t'
        t'' = addNewNodeWithNI t' (G.NodeInfo (G.NTCondBranch (G.BBLabel true_l)
                                               (G.BBLabel false_l))
                                   (currentLabel t') "br")
        br_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' reg_node br_node
    in t'''

  extend t (LLVM.LabelStmt (LLVM.Label l)) = updateLabel t (G.BBLabel l)

instance LlvmToOS LLVM.SetRegDestination where
  extend t (LLVM.SRDRegister reg) = extend t reg
  extend t (LLVM.SRDRegisterFlag flag) = extend t flag
  extend t (LLVM.SRDSymbol sym) = extend t sym
  extend t (LLVM.SRDTemporary temp) = extend t temp

instance LlvmToOS LLVM.Temporary where
  extend t temp@(LLVM.Temporary int) =
    let sym = toSymbol temp
        nid_already_in_use = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id nid_already_in_use
        t' = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t)
                                                  ("t" ++ show int)))
        t'' = maybe (addMapping t' (nid, sym)) (\_ -> t') nid_already_in_use
    in t''

instance LlvmToOS LLVM.Register where
  extend t (LLVM.Register str) =
    let t' = addNewNodeWithNI t (G.NodeInfo G.NTData (currentLabel t) "")
        n = fromJust $ lastAddedNode t'
        t'' = addConstraint t' (OS.AllocateInRegisterConstraint (G.nodeId n)
                                [OS.Register str])
    in t''

instance LlvmToOS LLVM.RegisterFlag where
  extend t (LLVM.RegisterFlag _ _) = t
  -- TODO: implement

instance LlvmToOS LLVM.Symbol where
  extend t org_sym@(LLVM.Symbol str) =
    let sym = toSymbol org_sym
        nid_already_in_use = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id nid_already_in_use
        t' = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t) str))

        t'' = maybe (addMapping t' (nid, sym)) (\_ -> t') nid_already_in_use
    in t''

instance LlvmToOS LLVM.StmtExpression where
  extend t (LLVM.BinaryOpStmtExpr op _ lhs rhs) =
    let t' = extend t lhs
        lhs_node = fromJust $ lastAddedNode t'
        t'' = extend t' rhs
        rhs_node = fromJust $ lastAddedNode t''
        t''' = addNewNodeWithNI t'' (G.NodeInfo (G.NTBinaryOp op)
                                     (currentLabel t'') (show op))
        op_node = fromJust $ lastAddedNode t'''
        t'''' = addNewEdgesManySources t''' [lhs_node, rhs_node] op_node
    in t''''

  extend t (LLVM.UnaryOpStmtExpr op _ expr) =
    let t' = extend t expr
        expr_node = fromJust $ lastAddedNode t'
        t'' = addNewNodeWithNI t' (G.NodeInfo (G.NTUnaryOp op)
                                   (currentLabel t') (show op))
        op_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' expr_node op_node
    in t'''

  extend t (LLVM.LoadStmtExpr _ _ expr) =
    let t' = extend t expr
        expr_node = fromJust $ lastAddedNode t'
        t'' = addNewNodeWithNI t' (G.NodeInfo G.NTMemoryLoad
                                   (currentLabel t') "load")
        op_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' expr_node op_node
    in t'''

  extend t (LLVM.PhiStmtExpr phis) =
    let f (ns, local_t) e =
          let local_t' = extend local_t e
              n = fromJust $ lastAddedNode local_t'
          in (ns ++ [n], local_t')
        (data_nodes, t') = foldl f ([], t) phis
        t'' = addNewNodeWithNI t' (G.NodeInfo G.NTPhi (currentLabel t') "phi")
        phi_node = fromJust $ lastAddedNode t''
        t''' = addNewEdgesManySources t'' data_nodes phi_node
    in t'''

  extend t (LLVM.DataStmtExpr d) = extend t d

  extend t (LLVM.SizeStmtExpr reg) = extend t reg

-- TODO: implement
-- LLVM.FP2IStmtExpr ExprResultSize StmtExpression ExprResultSize
-- LLVM.TruncStmtExpr ExprResultSize StmtExpression ExprResultSize
-- LLVM.RegRangeStmtExpr Register (Range AnyData)

instance LlvmToOS LLVM.PhiElement where
  extend t (LLVM.PhiElement expr l) = extend t expr
  -- TODO: what to do with label on the expression?

instance LlvmToOS LLVM.ProgramData where
  extend t (LLVM.PDConstant c) = extend t c
  extend t (LLVM.PDTemporary temp) = extend t temp
  extend t (LLVM.PDRegister reg) = extend t reg
  extend t (LLVM.PDSymbol sym) = extend t sym
  -- TODO: what to do with PDNoValue?

instance LlvmToOS LLVM.ConstantValue where
  extend t (LLVM.ConstIntValue val) =
    let t' = addNewNodeWithNI t (G.NodeInfo G.NTData (currentLabel t)
                                 (show val))
        n = fromJust $ lastAddedNode t'
        t'' = addConstraint t'' (OS.ConstantValueConstraint (G.nodeId n)
                                 [Range (OS.IntConstant val)
                                  (OS.IntConstant val)])
    in t''

instance LlvmToOS (Either LLVM.Register LLVM.Temporary) where
  extend t (Left reg) = extend t reg
  extend t (Right tmp) = extend t tmp

instance LlvmToOS LLVM.Constraint where
  extend t (LLVM.AllocateInConstraint store space) =
    let nid = fromJust $ nodeIdFromSym (currentMappings t) (toSymbol store)
        regs = toRegisters space
    in trace (show t ++ "\n\n") $ addConstraint t $ OS.AllocateInRegisterConstraint nid regs
  extend t (LLVM.RegFlagConstraint flag ranges) =
    addConstraint t $ OS.RegFlagConstraint (toRegisterFlag flag)
                      (map toConstRange ranges)
  extend t (LLVM.ImmediateConstraint imm ranges) =
    let nid = fromJust $ nodeIdFromSym (currentMappings t) (toSymbol imm)
        const_ranges = map toConstRange ranges
    in addConstraint t $ OS.ConstantValueConstraint nid const_ranges
  extend t (LLVM.AliasesConstraint as) = foldl extend t as
  -- TODO: handle address constraints

instance LlvmToOS [LLVM.AliasValue] where
  extend t avs =
    let only_values = filter (not . LLVM.isAVNoValue) avs
        ns = mapMaybe (nodeIdFromSym (currentMappings t) . toSymbol) only_values
    in addConstraint t $ OS.AliasConstraint ns
