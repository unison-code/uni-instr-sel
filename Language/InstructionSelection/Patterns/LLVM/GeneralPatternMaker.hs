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
-- TODO: Refactor so that the graph is created incrementally instead of
-- maintaining a list of nodes and edges.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Patterns.LLVM.GeneralPatternMaker (
  toGeneralPattern
) where

import qualified Language.InstructionSelection.Patterns as General
import qualified Language.InstructionSelection.Patterns.LLVM as LLVM
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Utils
import Data.List
import Data.Maybe
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
  let (_, g, _, cons) = formGraph llvm (BBLabel "", empty, [], [])
  in General.Pattern g cons

class GraphFormable a where

  -- | Forms a graph from a given LLVM tree node. It is expected that this
  -- process will be done bottom-up and that the node to be used by a parent
  -- always appears at the end in the list of nodes which simplifies the
  -- process. Hence we can also assume that the node IDs will be in strictly
  -- increasing order as they appear in the node list. Each declaration node and
  -- use node will be kept separate and must be merged by a following stage.

  formGraph :: a                      -- ^ LLVM tree node to form graph from.
            -> ( BBLabel              -- ^ Current label that the process is in.
               , Graph                -- ^ The graph so far.
               , [(NodeId, Symbol)]   -- ^ List of node-to-symbol mappings.
               , [General.Constraint]
               )
            -> ( BBLabel
               , Graph
               , [(NodeId, Symbol)]
               , [General.Constraint]
               )

instance GraphFormable LLVM.Pattern where

  -- | First all LLVM statements are converted into a list of nodes and edges,
  -- while maintaining a mapping from symbol to node ID, and then the
  -- constraints are converted. However, the alias constraints are handled a bit
  -- differently. After the list of nodes, edges, and mappings have been
  -- formulated, but before converting the constraints, additional mappings are
  -- created from the alias constraints where there exist symbols which have no
  -- corresponding node. Since a converted constraints always refer to a
  -- particular node, this allows us to handle the situations when the
  -- referenced symbol does not have a node.

  formGraph (LLVM.Pattern stmts cons) t =
    let f t stmt = formGraph stmt t
        t' = foldl f t stmts
        t'' = makeMappingsForNoNodeSymbols cons t'
        t''' = addConstraints (convertConstraints cons (getMappingList t'')) t''
    in t'''

makeMappingsForNoNodeSymbols cons t =
  foldr makeMappingsForNoNodeSymbols' t (filter LLVM.isAliasesConstraint cons)
makeMappingsForNoNodeSymbols' (LLVM.AliasesConstraint list_of_aliases) t =
  let new_maps = concatMap (makeMappingsForNoNodeSymbols'' (getMappingList t))
                 list_of_aliases
  in addMappings new_maps t
makeMappingsForNoNodeSymbols'' _ [] = []
makeMappingsForNoNodeSymbols'' _ (_:[]) = []
makeMappingsForNoNodeSymbols'' maps aliases =
  let (Just node_of_some_sym) = head $ filter isJust
                                $ map ((maybeGetNodeIdFromSym maps) . toSymbol)
                                      aliases
      syms_with_no_node = filter (not . isJust . (maybeGetNodeIdFromSym maps))
                          $ map toSymbol aliases
  in zip (repeat node_of_some_sym) syms_with_no_node

instance GraphFormable LLVM.Statement where
  formGraph (LLVM.AssignmentStmt tmp expr) t =
    let t' = formGraph tmp t
        (Just dst_node) = lastAddedNode $ getGraph t'
        t'' = formGraph expr t'
        (Just expr_node) = lastAddedNode $ getGraph t''
        t''' = updateGraph (addNewEdge expr_node dst_node $ getGraph t'') t''
    in t'''

  formGraph (LLVM.SetRegStmt dst expr) t =
    let t' = formGraph dst t
        (Just dst_node) = lastAddedNode $ getGraph t'
        t'' = formGraph expr t'
        (Just expr_node) = lastAddedNode $ getGraph t''
        t''' = updateGraph (addNewEdge expr_node dst_node $ getGraph t'') t''
    in t'''

  formGraph (LLVM.StoreStmt addr_expr _ _ value_expr) t =
    let t' = formGraph addr_expr t
        (Just addr_node) = lastAddedNode $ getGraph t'
        t'' = formGraph value_expr t'
        g = getGraph t''
        (Just value_node) = lastAddedNode g
        (store_node, g') =
          makeAndAddNewNode (NodeInfo NTMemoryStore (getCurrentLabel t'')
                             "store") g
        g'' = addNewEdgesManySources [addr_node, value_node] store_node g'
        t''' = updateGraph g'' t''
    in t'''

  formGraph (LLVM.UncondBranchStmt (LLVM.Label l)) t =
    let (_, g) = makeAndAddNewNode (NodeInfo (NTUncondBranch (BBLabel l))
                                    (getCurrentLabel t) "br") (getGraph t)
    in updateGraph g t

  formGraph (LLVM.CondBranchStmt reg (LLVM.Label true_l) (LLVM.Label false_l))
            t =
    let t' = formGraph reg t
        g = getGraph t'
        (Just reg_node) = lastAddedNode g
        (br_node, g') =
          makeAndAddNewNode (NodeInfo (NTCondBranch (BBLabel true_l)
                                       (BBLabel false_l)) (getCurrentLabel t)
                             "br") g
        g'' = addNewEdge reg_node br_node g'
        t'' = updateGraph g'' t'
    in t''

  formGraph (LLVM.LabelStmt (LLVM.Label label)) t =
    changeCurrentLabel (BBLabel label) t

instance GraphFormable LLVM.SetRegDestination where
  formGraph (LLVM.SRDRegister reg) t = formGraph reg t
  formGraph (LLVM.SRDRegisterSymbol reg) t = formGraph reg t
  formGraph (LLVM.SRDRegisterFlag flag) t = formGraph flag t
  formGraph (LLVM.SRDTemporary temp) t = formGraph temp t

instance GraphFormable LLVM.Temporary where
  formGraph (LLVM.Temporary temp) t =
    let temp_sym = TemporarySymbol temp
        maybe_id = (maybeGetNodeIdFromSym (getMappingList t) temp_sym)
        g = getGraph t
        node_id = maybe (newNodeId g) id maybe_id
        node_label = NodeLabel node_id (NodeInfo NTRegister (getCurrentLabel t)
                                        ("t" ++ show temp))
        t' = updateGraph (addNewNode node_label g) t
        t'' = maybe t' (\_ -> addMapping (node_id, temp_sym) t') maybe_id
    in t''

instance GraphFormable LLVM.Register where
  formGraph (LLVM.Register reg) t =
    let (node, g) = makeAndAddNewNode (NodeInfo NTRegister
                                       (getCurrentLabel t) "") $ getGraph t
        constraint = General.AllocateInRegisterConstraint (nodeId node)
                     [(General.Register reg)]
    in addConstraint constraint $ updateGraph g t

instance GraphFormable LLVM.RegisterFlag where
  formGraph (LLVM.RegisterFlag str reg) t = t
  -- TODO: implement

instance GraphFormable LLVM.RegisterSymbol where
  formGraph (LLVM.RegisterSymbol sym) t =
    let reg_sym = RegisterSymbol sym
        maybe_id = maybeGetNodeIdFromSym (getMappingList t) reg_sym
        g = getGraph t
        node_id = maybe (newNodeId g) id maybe_id
        node_label = NodeLabel node_id (NodeInfo NTRegister (getCurrentLabel t)
                                        sym)
        t' = updateGraph (addNewNode node_label g) t
        t'' = maybe t' (\_ -> addMapping (node_id, reg_sym) t') maybe_id
    in t''

instance GraphFormable LLVM.StmtExpression where
  formGraph (LLVM.BinaryOpStmtExpr op _ lhs rhs) t =
    let t' = formGraph lhs t
        (Just lhs_node) = lastAddedNode $ getGraph t'
        t'' = formGraph rhs t'
        g = getGraph t''
        (Just rhs_node) = lastAddedNode g
        (op_node, g') = makeAndAddNewNode (NodeInfo (NTBinaryOp op)
                                           (getCurrentLabel t) (show op)) g
        g'' = addNewEdgesManySources [lhs_node, rhs_node] op_node g'
        t''' = updateGraph g'' t''
    in t'''

  formGraph (LLVM.UnaryOpStmtExpr op _ expr) t =
    let t' = formGraph expr t
        g = getGraph t'
        (Just expr_node) = lastAddedNode g
        (op_node, g') = makeAndAddNewNode (NodeInfo (NTUnaryOp op)
                                           (getCurrentLabel t) (show op)) g
        t'' = updateGraph (addNewEdge expr_node op_node g) t'
    in t''

  formGraph (LLVM.LoadStmtExpr _ _ expr) t =
    let t' = formGraph expr t
        g = getGraph t'
        (Just expr_node) = lastAddedNode g
        (op_node, g') = makeAndAddNewNode (NodeInfo NTMemoryLoad
                                           (getCurrentLabel t) "load") g
        t'' = updateGraph (addNewEdge expr_node op_node g) t'
    in t''

  formGraph (LLVM.PhiStmtExpr phis) t =
    let f (ns, t) elem =
          let t' = formGraph elem t
              (Just data_node) = lastAddedNode $ getGraph t'
          in (ns ++ [data_node], t')
        (data_nodes, t') = foldl f ([], t) phis
        (phi_node, g) = makeAndAddNewNode (NodeInfo NTPhi (getCurrentLabel t')
                                           "phi") $ getGraph t'
        g' = addNewEdgesManySources data_nodes phi_node g
        t'' = updateGraph g' t'
    in t''

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
    let (n, g) = makeAndAddNewNode (NodeInfo NTConstant (getCurrentLabel t)
                                    (show val)) $ getGraph t
        constraint = General.ConstantValueConstraint (nodeId n)
                     [Range (General.IntConstant val) (General.IntConstant val)]
    in addConstraint constraint $ updateGraph g t

instance GraphFormable LLVM.ImmediateSymbol where
  formGraph (LLVM.ImmediateSymbol sym) t =
    let imm_sym = ImmediateSymbol sym
        maybe_id = maybeGetNodeIdFromSym (getMappingList t) imm_sym
        g = getGraph t
        id = case maybe_id of (Just id) -> id
                              otherwise -> newNodeId g
        node_label = NodeLabel id (NodeInfo NTConstant (getCurrentLabel t) sym)
        t' = updateGraph (addNewNode node_label g) t
        t'' = if isJust maybe_id
                 then t'
                 else addMapping (id, imm_sym) t'
    in t''

instance GraphFormable (Either LLVM.Register LLVM.Temporary) where
  formGraph (Left reg) t = formGraph reg t
  formGraph (Right tmp) t = formGraph tmp t

getCurrentLabel (l, _, _, _) = l
changeCurrentLabel l (_, g, ms, cs) = (l, g, ms, cs)
getGraph (_, g, _, _) = g
updateGraph g (l, _, ms, cs) = (l, g, ms, cs)
getMappingList (_, _, ms, _) = ms
addMapping m (l, g, ms, cs) = (l, g, ms ++ [m], cs)
addMappings more_ms (l, g, ms, cs) = (l, g, ms ++ more_ms, cs)
addConstraint c (l, g, ms, cs) = (l, g, ms, cs ++ [c])
addConstraints more_cs (l, g, ms, cs) = (l, g, ms, cs ++ more_cs)

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
instance SymbolFormable (Either LLVM.Temporary LLVM.RegisterSymbol) where
  toSymbol (Left temp) = toSymbol temp
  toSymbol (Right reg) = toSymbol reg
instance SymbolFormable LLVM.ImmediateSymbol where
  toSymbol (LLVM.ImmediateSymbol str) = ImmediateSymbol str
instance SymbolFormable LLVM.AliasValue where
  toSymbol (LLVM.AVTemporary temp) = toSymbol temp
  toSymbol (LLVM.AVRegisterSymbol reg) = toSymbol reg

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

class ConstRangeFormable a where
  toConstRange :: a -> (Range General.Constant)
instance ConstRangeFormable (Range Integer) where
  toConstRange (Range lower upper) = Range (General.IntConstant lower)
                                     (General.IntConstant upper)

convertConstraints cons maps = concatMap (convertConstraint maps) cons
convertConstraint maps (LLVM.AllocateInConstraint store space) =
  let node_id = getNodeIdFromSym maps (toSymbol store)
      regs = toRegisters space
  in [General.AllocateInRegisterConstraint node_id regs]
convertConstraint maps (LLVM.RegFlagConstraint flag ranges) =
  [General.RegFlagConstraint (convertRegFlag flag) (map toConstRange ranges)]
convertConstraint maps (LLVM.ImmediateConstraint imm ranges) =
  let node_id = getNodeIdFromSym maps (toSymbol imm)
      const_ranges = map toConstRange ranges
  in [General.ConstantValueConstraint node_id const_ranges]
convertConstraint maps (LLVM.AliasesConstraint list_of_aliases) =
  map (convertAliases maps) list_of_aliases
-- TODO: handle address constraints

convertRegFlag (LLVM.RegisterFlag flag reg) =
  General.RegisterFlag flag (head $ toRegisters reg)
convertAliases maps aliases =
  let maybe_nodes = map ((maybeGetNodeIdFromSym maps) . toSymbol) aliases
      only_actual_nodes = filter isJust maybe_nodes
      nodes = map (\(Just n) -> n) only_actual_nodes
  in General.AliasConstraint nodes

getNodeIdFromSym maps sym =
  let (Just node_id) = maybeGetNodeIdFromSym maps sym
  in node_id
maybeGetNodeIdFromSym maps sym =
  let node_list = filter (\(n, s) -> s == sym) maps
  in if not $ null node_list
        then let (node_id, _) = last node_list
             in Just node_id
        else Nothing

resolveAliases (General.Pattern g cons) =
  let alias_cons = filter General.isAliasConstraint cons
      all_cons_but_alias = filter (not . General.isAliasConstraint) cons
      (new_g, new_cons) = foldr resolveAliases' (g, all_cons_but_alias)
                                alias_cons
  in General.Pattern new_g new_cons

resolveAliases' (General.AliasConstraint []) t = t
resolveAliases' (General.AliasConstraint (_:[])) t = t
resolveAliases' (General.AliasConstraint nodes) t =
  let node_to_replace_with = head nodes
      nodes_to_replace = tail nodes
      combinations = zip (repeat node_to_replace_with) nodes_to_replace
  in foldr resolveAliases'' t combinations
resolveAliases'' (n1, n2) (g, cons) =
  let new_g = copyNodeLabel n1 n2 g
      new_cons = map (updateNodeInConstraint n1 n2) cons
  in (new_g, new_cons)

updateNodeInConstraint n1 n2 con@(General.AllocateInRegisterConstraint id regs)
  | n2 == id = General.AllocateInRegisterConstraint n1 regs
  | otherwise = con
updateNodeInConstraint n1 n2 con@(General.ConstantValueConstraint id ranges)
  | n2 == id = General.ConstantValueConstraint n1 ranges
  | otherwise = con
updateNodeInConstraint _ _ con = con

mergeIdenticalNodes :: General.Pattern -> General.Pattern
mergeIdenticalNodes (General.Pattern g cons) =
  let isSame n1 n2 = haveSameNodeIds n1 n2 && haveSameLabels n1 n2
      unique_nodes = nubBy isSame (nodes g)
  in (General.Pattern (foldr (mergeNodes isSame) g unique_nodes) cons)

mergeAdjacentDataNodes :: General.Pattern -> General.Pattern
mergeAdjacentDataNodes g = g
-- TODO: implement

makeAndAddNewNode node_info g =
  let new_g = addNewNode (NodeLabel (newNodeId g) node_info) g
      (Just node) = lastAddedNode new_g
  in (node, new_g)
