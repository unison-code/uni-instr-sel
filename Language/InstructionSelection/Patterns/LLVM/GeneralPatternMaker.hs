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

module Language.InstructionSelection.Patterns.LLVM.GeneralPatternMaker (
  toGeneralPattern
) where

import qualified Language.InstructionSelection.Patterns as General
import qualified Language.InstructionSelection.Patterns.LLVM as LLVM
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Utils
import Data.Graph.Inductive.Graph hiding (Graph)

toGeneralPattern :: LLVM.Pattern -> General.Pattern
toGeneralPattern = mergeIdenticalNodes . graphify

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

  formGraph :: a                   -- ^ LLVM tree node to form graph from.
            -> ( BBLabel           -- ^ Current label that the process is in.
               , [LNode NodeLabel] -- ^ List of nodes created so far.
               , [LEdge EdgeLabel] -- ^ List of edges created so far.
               , [(Node, Symbol)]  -- ^ List of node-to-symbol mappings created
                                   -- so far.
               , [General.Constraint]
               )
            -> ( BBLabel
               , [LNode NodeLabel]
               , [LEdge EdgeLabel]
               , [(Node, Symbol)]
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
        t'' = formGraph expr t'
    in t''

  formGraph (LLVM.SetRegStmt reg expr) t =
    let (label, nodes, edges, mappings, constraints) = formGraph reg t
        (id1, (NodeLabel id2 (NTRegister _) l str)) = last nodes
        reg_node_set_as_output = (id1, (NodeLabel id2 (NTRegister True) l str))
        nodes' = init nodes ++ [reg_node_set_as_output]
        t'' = formGraph expr (label, nodes', edges, mappings, constraints)
    in t''

  formGraph (LLVM.StoreStmt addr_expr str ressize value_expr) t =
    let t' = formGraph addr_expr t
        addr_node = last $ getNodeList t'
        t'' = formGraph value_expr t'
        value_node = last $ getNodeList t''
        store_node_int = nextNodeInt $ getNodeList t''
        store_node = ( store_node_int
                     , NodeLabel (toNatural $ toInteger store_node_int)
                       NTMemoryStore (getCurrentLabel t) "store"
                     )
        t''' = addNode store_node t''
        e1 = createNewEdge addr_node store_node (getEdgeList t''')
        t'''' = addEdge e1 t'''
        e2 = createNewEdge value_node store_node (getEdgeList t'''')
        t''''' = addEdge e2 t''''
    in t'''''

  formGraph (LLVM.UncondBranchStmt (LLVM.Label l)) t =
    let br_node_int = nextNodeInt $ getNodeList t
        br_node = ( br_node_int
                  , NodeLabel (toNatural $ toInteger br_node_int)
                    (NTUncondBranch (BBLabel l)) (getCurrentLabel t) "br"
                  )
    in addNode br_node t

  formGraph (LLVM.CondBranchStmt reg (LLVM.Label true_l) (LLVM.Label false_l))
            t =
    let t' = formGraph reg t
        reg_node = last $ getNodeList t'
        br_node_int = nextNodeInt $ getNodeList t'
        br_node = ( br_node_int
                  , NodeLabel (toNatural $ toInteger br_node_int)
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
        temp_node = ( temp_node_int
                    , NodeLabel (toNatural $ toInteger temp_node_int)
                      (NTRegister False) (getCurrentLabel t)
                      ("t" ++ show temp_int)
                    )
        mapping = (temp_node_int, TemporarySymbol temp_int)
    in addNode temp_node $ addMapping mapping t

instance GraphFormable LLVM.Register where
  formGraph (LLVM.RegByTemporary temp) t = formGraph temp t

  formGraph (LLVM.RegBySymbol (LLVM.RegisterSymbol reg_sym)) t =
    let reg_node_int = nextNodeInt $ getNodeList t
        reg_node = ( reg_node_int
                    , NodeLabel (toNatural $ toInteger reg_node_int)
                      (NTRegister False) (getCurrentLabel t)
                      (show reg_sym)
                    )
        mapping = (reg_node_int, RegisterSymbol reg_sym)
        -- TODO: add register constraint
    in addNode reg_node $ addMapping mapping t

instance GraphFormable LLVM.StmtExpression where
  formGraph (LLVM.BinaryOpStmtExpr op _ lhs rhs) t =
    let t' = formGraph lhs t
        lhs_node = last $ getNodeList t'
        t'' = formGraph rhs t'
        rhs_node = last $ getNodeList t''
        op_node_int = nextNodeInt $ getNodeList t''
        op_node = ( op_node_int
                  , NodeLabel (toNatural $ toInteger op_node_int)
                    (NTBinaryOp op) (getCurrentLabel t) (show op)
                  )
        t''' = addNode op_node t''
        e1 = createNewEdge lhs_node op_node (getEdgeList t''')
        t'''' = addEdge e1 t'''
        e2 = createNewEdge rhs_node op_node (getEdgeList t'''')
        t''''' = addEdge e2 t''''
    in t'''''

  formGraph (LLVM.UnaryOpStmtExpr op _ expr) t =
    let t' = formGraph expr t
        expr_node = last $ getNodeList t'
        op_node_int = nextNodeInt $ getNodeList t'
        op_node = ( op_node_int
                  , NodeLabel (toNatural $ toInteger op_node_int)
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
        load_node = ( load_node_int
                    , NodeLabel (toNatural $ toInteger load_node_int)
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
        phi_node = ( phi_node_int
                   , NodeLabel (toNatural $ toInteger phi_node_int)
                     NTPhi (getCurrentLabel t) "phi"
                   )
        t'' = addNode phi_node t'
        g t data_node =
          let e = createNewEdge data_node phi_node (getEdgeList t)
          in addEdge e t
        t''' = foldl g t'' data_nodes
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
        const_node = ( const_node_int
                     , NodeLabel (toNatural $ toInteger const_node_int)
                       NTConstant (getCurrentLabel t) (show val)
                     )
        -- TODO: add constant value constraint
    in addNode const_node t

instance GraphFormable LLVM.ImmediateSymbol where
  formGraph (LLVM.ImmediateSymbol imm_sym) t =
    let imm_node_int = nextNodeInt $ getNodeList t
        imm_node = ( imm_node_int
                   , NodeLabel (toNatural $ toInteger imm_node_int)
                     NTConstant (getCurrentLabel t) imm_sym
                   )
    in addNode imm_node t

getCurrentLabel (l, _, _, _, _) = l
changeCurrentLabel l (_, ns, es, ms, cs) = (l, ns, es, ms, cs)
getNodeList (_, ns, _, _, _) = ns
getEdgeList (_, _, es, _, _) = es
getMappingList (_, _, _, ms, _) = ms
getNodeInt :: LNode NodeLabel -> Node
getNodeInt (int, _) = int
nextNodeInt :: [LNode NodeLabel] -> Node
nextNodeInt ns = (getNodeInt $ last ns) + 1
addNode n (l, ns, es, ms, cs) = (l, ns ++ [n], es, ms, cs)
addNodes more_ns (l, ns, es, ms, cs) = (l, ns ++ more_ns, es, ms, cs)
addEdge e (l, ns, es, ms, cs) = (l, ns, es ++ [e], ms, cs)
addEdges more_es (l, ns, es, ms, cs) = (l, ns, es ++ more_es, ms, cs)
addMapping m (l, ns, es, ms, cs) = (l, ns, es, ms ++ [m], cs)
addMappings more_ms (l, ns, es, ms, cs) = (l, ns, es, ms ++ more_ms, cs)
addConstraint c (l, ns, es, ms, cs) = (l, ns, es, ms, cs ++ [c])
addConstraints more_cs (l, ns, es, ms, cs) = (l, ns, es, ms, cs ++ more_cs)

createNewEdge :: LNode NodeLabel
              -> LNode NodeLabel
              -> [LEdge EdgeLabel]
              -> LEdge EdgeLabel
createNewEdge src dst es = (0, 0, EdgeLabel 0 0)
  -- TODO: implement

mergeIdenticalNodes :: General.Pattern -> General.Pattern
mergeIdenticalNodes g = g
-- TODO: implement

data Symbol
    = RegisterSymbol String
    | TemporarySymbol Integer
    | ImmediateSymbol String
    | ConstantSymbol Integer
    deriving (Show, Eq)

convertConstraints :: [LLVM.Constraint]
                   -> [(Node, Symbol)]
                   -> [General.Constraint]
convertConstraints cons mappings = []
-- TODO: implement