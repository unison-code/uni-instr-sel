--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.LLVM.GeneralPatternMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts an LLVM pattern into the internal general graph-based format.
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Patterns.LLVM.GeneralPatternMaker (
  toGeneralPattern
) where

import qualified Language.InstructionSelection.Patterns as General
import qualified Language.InstructionSelection.Patterns.LLVM as LLVM
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Utils
import Data.Graph.Inductive.Graph hiding (Graph)
import Debug.Trace -- TODO: remove when no longer necessary



toGeneralPattern :: LLVM.Pattern -> General.Pattern
toGeneralPattern = mergeAdjacentDataNodes . mergeIdenticalNodes
                   . resolveAliases . graphify

-- | Converts an LLVM pattern into a graph-based representation, without
-- resolving anything like aliases or the like. Hence the output is illegal
-- from a pattern point of view, but it is much easier to legalize it once it's
-- in a graphical format.

graphify :: LLVM.Pattern -> General.Pattern
graphify llvm =
  let (_, nodes, edges, mappings, constraints) =
        formGraph llvm (BBLabel "", [], [], [], [])
      graph = Graph (mkGraph nodes edges)
  in General.Pattern graph constraints

class GraphFormable a where

  -- | Forms a graph from a given LLVM tree node. It is expected that this
  -- process will be done bottom-up and that the node to be used by a parent
  -- always appears at the end in the list of nodes which simplifies the
  -- process. Hence we can also assume that the node IDs will be in strictly
  -- increasing order as they appear in the node list. Each declaration node and
  -- use node will be kept separate and must be merged by a following stage.

  formGraph :: a                      -- ^ LLVM tree node to form graph from.
            -> ( BBLabel              -- ^ Current label that the process is in.
               , [LNode NodeLabel]    -- ^ List of nodes created so far.
               , [LEdge EdgeLabel]    -- ^ List of edges created so far.
               , [(NodeId, Symbol)]   -- ^ List of node-to-symbol mappings
                                      -- created so far.
               , [General.Constraint]
               )
            -> ( BBLabel
               , [LNode NodeLabel]
               , [LEdge EdgeLabel]
               , [(NodeId, Symbol)]
               , [General.Constraint]
               )

instance GraphFormable LLVM.Pattern where
  formGraph (LLVM.Pattern stmts cons) t =
    let f t stmt = formGraph stmt t
        t' = foldl f t stmts
        t'' = addConstraints (convertConstraints cons (getMappingList t')) t'
    in t''

instance GraphFormable LLVM.Statement where
  formGraph (LLVM.AssignmentStmt tmp expr) t =
    let t' = formGraph tmp t
        dst_node = last $ getNodeList t'
        t'' = formGraph expr t'
        expr_node = last $ getNodeList t''
        e = createNewEdge expr_node dst_node (getEdgeList t'')
        t''' = addEdge e t''
    in t'''

  formGraph (LLVM.SetRegStmt reg expr) t =
    let t' = formGraph reg t
        reg_node = last $ getNodeList t'
        t'' = formGraph expr t'
        expr_node = last $ getNodeList t''
        e = createNewEdge expr_node reg_node (getEdgeList t'')
        t''' = addEdge e t''
    in t'''

  formGraph (LLVM.StoreStmt addr_expr str ressize value_expr) t =
    let t' = formGraph addr_expr t
        addr_node = last $ getNodeList t'
        t'' = formGraph value_expr t'
        value_node = last $ getNodeList t''
        store_node_int = nextNodeInt $ getNodeList t''
        store_node_id = toNatural $ toInteger store_node_int
        store_node = ( store_node_int
                     , NodeLabel (store_node_id)
                       NTMemoryStore (getCurrentLabel t) "store"
                     )
        t''' = addNode store_node t''
        edges = createNewEdgesManySources [addr_node, value_node] store_node
                (getEdgeList t''')
        t'''' = addEdges edges t'''
    in t''''

  formGraph (LLVM.UncondBranchStmt (LLVM.Label l)) t =
    let br_node_int = nextNodeInt $ getNodeList t
        br_node_id = toNatural $ toInteger br_node_int
        br_node = ( br_node_int
                  , NodeLabel br_node_id
                    (NTUncondBranch (BBLabel l)) (getCurrentLabel t) "br"
                  )
    in addNode br_node t

  formGraph (LLVM.CondBranchStmt reg (LLVM.Label true_l) (LLVM.Label false_l))
            t =
    let t' = formGraph reg t
        reg_node = last $ getNodeList t'
        br_node_int = nextNodeInt $ getNodeList t'
        br_node_id = toNatural $ toInteger br_node_int
        br_node = ( br_node_int
                  , NodeLabel br_node_id
                    (NTCondBranch (BBLabel true_l) (BBLabel false_l))
                    (getCurrentLabel t) "br"
                  )
        t'' = addNode br_node t'
        e = createNewEdge reg_node br_node (getEdgeList t'')
        t''' = addEdge e t''
    in t'''

  formGraph (LLVM.LabelStmt (LLVM.Label label)) t =
    changeCurrentLabel (BBLabel label) t

instance GraphFormable LLVM.Temporary where
  formGraph (LLVM.Temporary temp_int) t =
    let temp_node_int = nextNodeInt $ getNodeList t
        temp_node_id = toNatural $ toInteger temp_node_int
        temp_node = ( temp_node_int
                    , NodeLabel temp_node_id
                      NTRegister (getCurrentLabel t) ("t" ++ show temp_int)
                    )
        mapping = (temp_node_id, TemporarySymbol temp_int)
        t' = addNode temp_node $ addMapping mapping t
    in t'

instance GraphFormable LLVM.Register where
  formGraph (LLVM.Register reg) t =
    let reg_node_int = nextNodeInt $ getNodeList t
        reg_node_id = toNatural $ toInteger reg_node_int
        reg_node = ( reg_node_int
                    , NodeLabel reg_node_id
                      NTRegister (getCurrentLabel t) ""
                    )
        constraint = General.AllocateInRegisterConstraint
                     (toNatural $ toInteger reg_node_int)
                     [(General.Register reg)]
    in addConstraint constraint $ addNode reg_node t

instance GraphFormable LLVM.RegisterSymbol where
  formGraph (LLVM.RegisterSymbol sym) t =
    let reg_node_int = nextNodeInt $ getNodeList t
        reg_node_id = toNatural $ toInteger reg_node_int
        reg_node = ( reg_node_int
                    , NodeLabel (toNatural $ toInteger reg_node_int)
                      NTRegister (getCurrentLabel t) sym
                    )
        mapping = (reg_node_int, RegisterSymbol sym)
    in addNode reg_node $ addMapping mapping t

instance GraphFormable LLVM.StmtExpression where
  formGraph (LLVM.BinaryOpStmtExpr op _ lhs rhs) t =
    let t' = formGraph lhs t
        lhs_node = last $ getNodeList t'
        t'' = formGraph rhs t'
        rhs_node = last $ getNodeList t''
        op_node_int = nextNodeInt $ getNodeList t''
        op_node_id = toNatural $ toInteger op_node_int
        op_node = ( op_node_int
                  , NodeLabel op_node_id
                    (NTBinaryOp op) (getCurrentLabel t) (show op)
                  )
        t''' = addNode op_node t''
        edges = createNewEdgesManySources [lhs_node, rhs_node] op_node
                (getEdgeList t''')
        t'''' = addEdges edges t'''
    in t''''

  formGraph (LLVM.UnaryOpStmtExpr op _ expr) t =
    let t' = formGraph expr t
        expr_node = last $ getNodeList t'
        op_node_int = nextNodeInt $ getNodeList t'
        op_node_id = toNatural $ toInteger op_node_int
        op_node = ( op_node_int
                  , NodeLabel op_node_id
                    (NTUnaryOp op) (getCurrentLabel t) (show op)
                  )
        t'' = addNode op_node t'
        e = createNewEdge expr_node op_node (getEdgeList t'')
        t''' = addEdge e t''
    in t'''

  formGraph (LLVM.LoadStmtExpr _ _ expr) t =
    let t' = formGraph expr t
        expr_node = last $ getNodeList t'
        load_node_int = nextNodeInt $ getNodeList t'
        load_node_id = toNatural $ toInteger load_node_int
        load_node = ( load_node_int
                    , NodeLabel load_node_id
                      NTMemoryLoad (getCurrentLabel t) "load"
                    )
        t'' = addNode load_node t'
        e = createNewEdge expr_node load_node (getEdgeList t'')
        t''' = addEdge e t''
    in t'''

  formGraph (LLVM.PhiStmtExpr phis) t =
    let f (ns, t) elem =
          let t' = formGraph elem t
              data_node = last $ getNodeList t'
          in (ns ++ [data_node], t')
        (data_nodes, t') = foldl f ([], t) phis
        phi_node_int = nextNodeInt $ getNodeList t'
        phi_node_id = toNatural $ toInteger phi_node_int
        phi_node = ( phi_node_int
                   , NodeLabel phi_node_id
                     NTPhi (getCurrentLabel t) "phi"
                   )
        t'' = addNode phi_node t'
        edges = createNewEdgesManySources data_nodes phi_node (getEdgeList t)
        t''' = addEdges edges t''
    in t'''

  formGraph (LLVM.DataStmtExpr d) t = formGraph d t

  formGraph (LLVM.SizeStmtExpr reg) t = formGraph reg t

-- TODO: implement
-- LLVM.FP2IStmtExpr ExprResultSize StmtExpression ExprResultSize
-- LLVM.TruncStmtExpr ExprResultSize StmtExpression ExprResultSize
-- LLVM.RegRangeStmtExpr Register (Range AnyData)

instance GraphFormable LLVM.PhiElement where
  formGraph (LLVM.PhiElement expr l) t = formGraph expr t

instance GraphFormable LLVM.ProgramData where
  formGraph (LLVM.PDConstant const) t = formGraph const t
  formGraph (LLVM.PDImmediate imm) t = formGraph imm t
  formGraph (LLVM.PDTemporary temp) t = formGraph temp t
  formGraph (LLVM.PDRegister reg) t = formGraph reg t
  -- TODO: what to do with PDNoValue?

instance GraphFormable LLVM.ConstantValue where
  formGraph (LLVM.ConstIntValue val) t =
    let const_node_int = nextNodeInt $ getNodeList t
        const_node_id = toNatural $ toInteger const_node_int
        const_node = ( const_node_int
                     , NodeLabel const_node_id
                       NTConstant (getCurrentLabel t) (show val)
                     )
        constraint = General.ConstantValueConstraint const_node_id
                     [Range (General.IntConstant val) (General.IntConstant val)]
    in addConstraint constraint $ addNode const_node t

instance GraphFormable LLVM.ImmediateSymbol where
  formGraph (LLVM.ImmediateSymbol imm_sym) t =
    let imm_node_int = nextNodeInt $ getNodeList t
        imm_node_id = toNatural $ toInteger imm_node_int
        imm_node = ( imm_node_int
                   , NodeLabel imm_node_id
                     NTConstant (getCurrentLabel t) imm_sym
                   )
    in addNode imm_node t

instance GraphFormable (Either LLVM.Register LLVM.Temporary) where
  formGraph (Left reg) t = formGraph reg t
  formGraph (Right tmp) t = formGraph tmp t

getCurrentLabel (l, _, _, _, _) = l
changeCurrentLabel l (_, ns, es, ms, cs) = (l, ns, es, ms, cs)
getNodeList (_, ns, _, _, _) = ns
replaceNodeList ns (l, _, es, ms, cs) = (l, ns, es, ms, cs)
getEdgeList (_, _, es, _, _) = es
getMappingList (_, _, _, ms, _) = ms
getNodeInt (int, _) = int
nextNodeInt [] = 0
nextNodeInt ns = (getNodeInt $ last ns) + 1
addNode n (l, ns, es, ms, cs) = (l, ns ++ [n], es, ms, cs)
addNodes more_ns (l, ns, es, ms, cs) = (l, ns ++ more_ns, es, ms, cs)
addEdge e (l, ns, es, ms, cs) = (l, ns, es ++ [e], ms, cs)
addEdges more_es (l, ns, es, ms, cs) = (l, ns, es ++ more_es, ms, cs)
addMapping m (l, ns, es, ms, cs) = (l, ns, es, ms ++ [m], cs)
addMappings more_ms (l, ns, es, ms, cs) = (l, ns, es, ms ++ more_ms, cs)
addConstraint c (l, ns, es, ms, cs) = (l, ns, es, ms, cs ++ [c])
addConstraints more_cs (l, ns, es, ms, cs) = (l, ns, es, ms, cs ++ more_cs)

createNewEdge src@(src_id, _) dst@(dst_id, _) es =
  let src_edges = getSourceEdgesPerNode src es
      dst_edges = getDestEdgesPerNode dst es
      src_edge_nr =
        1 + (maximum $ map (\(_, _, (EdgeLabel nr _)) -> nr) src_edges)
      dst_edge_nr =
        1 + (maximum $ map (\(_, _, (EdgeLabel _ nr)) -> nr) dst_edges)
  in (src_id, dst_id, (EdgeLabel src_edge_nr dst_edge_nr))
getSourceEdgesPerNode (id, _) es =
  filter f es
  where f (src_id, dst_id, _) = if id == src_id then True else False
getDestEdgesPerNode (id, _) es =
  filter f es
  where f (src_id, dst_id, _) = if id == dst_id then True else False
createNewEdgesManySources srcs dst es =
  foldl f [] srcs
  where f new_edges node = new_edges ++ [createNewEdge node dst es]
createNewEdgesManyDests src dsts es =
  foldl f [] dsts
  where f new_edges node = new_edges ++ [createNewEdge src node es]

data Symbol
    = RegisterSymbol String
    | TemporarySymbol Integer
    | ImmediateSymbol String
    | ConstantSymbol Integer
    deriving (Show, Eq)

class SymbolFormable a where
  toSymbol :: a -> Symbol
instance SymbolFormable LLVM.RegisterSymbol where
  toSymbol (LLVM.RegisterSymbol str) = RegisterSymbol str
instance SymbolFormable LLVM.Temporary where
  toSymbol (LLVM.Temporary int) = TemporarySymbol int
instance SymbolFormable LLVM.Register where
  toSymbol (LLVM.RegBySymbol sym) = toSymbol sym
  toSymbol (LLVM.RegByTemporary temp) = toSymbol temp
instance SymbolFormable LLVM.AnyStorage where
  toSymbol (LLVM.ASTemporary temp) = toSymbol temp
  toSymbol (LLVM.ASRegister reg) = toSymbol reg

class RegisterFormable a where
  toRegisters :: a -> [General.Register]
  toRegister :: a -> General.Register
instance RegisterFormable LLVM.AnyStorageSpace where
  toRegisters (LLVM.ASSDataSpace ds) = toRegisters ds
instance RegisterFormable LLVM.DataSpace where
  toRegisters (LLVM.DSRegisterClass rc) = toRegisters rc
instance RegisterFormable LLVM.RegisterClass where
  toRegisters (LLVM.RegisterClass regs) = map toRegister regs
instance RegisterFormable LLVM.Register where
  toRegister (LLVM.RegByRegister str) = General.Register str

convertConstraints :: [LLVM.Constraint]
                   -> [(NodeId, Symbol)]
                   -> [General.Constraint]
convertConstraints cons maps = concatMap (convertConstraint maps) cons
convertConstraint maps (LLVM.AllocateInConstraint store space) =
  let sym = toSymbol store
      node_id = head $ filter (\(n, s) -> s == sym) maps
      regs = toRegisters space
  in General.AllocateInRegisterConstraint node_id regs
convertConstraint maps (LLVM.ImmediateConstraint imm ranges) = []
convertConstraint maps (LLVM.AliasesConstraint aliases) = []
-- TOOD: handle reg-flag constraint
-- TODO: handle address constraints

resolveAliases :: General.Pattern -> General.Pattern
resolveAliases g = g
-- TODO: implement

mergeIdenticalNodes :: General.Pattern -> General.Pattern
mergeIdenticalNodes g = g
-- TODO: implement

mergeAdjacentDataNodes :: General.Pattern -> General.Pattern
mergeAdjacentDataNodes g = g
-- TODO: implement
