--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ProgramModules.LLVM.FunctionMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts and LLVM IR module into the internal function format.
--
-- Since only the function name is retained, the names of overloaded functions
-- must have been resolved such that each is given a unique name.
--
-- TODO: update handling of phi nodes
--------------------------------------------------------------------------------

module Language.InstSel.ProgramModules.LLVM.FunctionMaker
  ( mkFunctionsFromLlvmModule
  , mkFunction
  )
where

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.IntegerPredicate as LLVMI
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified Language.InstSel.Constraints as C
import qualified Language.InstSel.DataTypes as D
import qualified Language.InstSel.Graphs as G
import qualified Language.InstSel.OpStructures as OS
import qualified Language.InstSel.OpTypes as Op
import qualified Language.InstSel.ProgramModules.Base as PM
import Language.InstSel.Utils
  ( computePosMapsOfPerm
  , toNatural
  )
import Data.Maybe



--------------
-- Data types
--------------

-- | Represents a mapping from a symbol to a node currently in the operation
-- structure.
type SymToNodeMapping = (G.Node, Symbol)

-- | Represents a mapping from constant symbol to a node currently in the
-- operation structure.
type ConstToNodeMapping = (G.Node, Constant)

-- | Represents auxiliary phi node data which will be needed in order to correct
-- the edge ordering. This is because the ordering is difficult to achieve as
-- the operation structure is being built, which is why it is fixed as a
-- post-step. The adjoining list of labels represents the labels of the basic
-- blocks from which the data values originate, and the order of the list is
-- exactly that of the edge ordering for the phi node just after it has been
-- built.
type AuxPhiNodeData = (G.Node, [G.BBLabelID])

-- | Represents the intermediate data as the function is being processed.
data ProcessState =
    ProcessState
    { theOS :: OS.OpStructure
      -- ^ The operation structure.

    , lastTouchedNode :: Maybe G.Node
      -- ^ The last node (if any) that was touched. This is used to simplifying
      -- edge insertion. This value is ignored during post-processing.

    , theLabelNode :: Maybe G.Node
      -- ^ The label node which represents the basic block currently being
      -- processed.

    , theSymMaps :: [SymToNodeMapping]
      -- ^ List of symbol-to-node mappings. If there are more than one mapping
      -- using the same symbol, then the last one occuring in the list should be
      -- picked.

    , theConstMaps :: [ConstToNodeMapping]
      -- ^ List of constant-to-node mappings. If there are more than one mapping
      -- using the same symbol, then the last one occuring in the list should be
      -- picked.

    , thePhiData :: [AuxPhiNodeData]
      -- ^ Auxiliary phi node data (see description of the type for more
      -- information).

    , theFuncInputs :: [G.NodeID]
      -- ^ The IDs of the nodes representing the function input arguments.

    , theFuncRets :: [G.NodeID]
      -- ^ The IDs of the nodes representing the function return statements.
    }
  deriving (Show)

-- | Retains various symbol names.
data Symbol =
    LocalStringSymbol String
  | GlobalStringSymbol String
  | TemporarySymbol Integer
  deriving (Eq)

instance Show Symbol where
  show (LocalStringSymbol str) = "%" ++ str
  show (GlobalStringSymbol str) = "@" ++ str
  show (TemporarySymbol int) = "t" ++ show int

-- | Retains various constant values.
data Constant =
    IntConstant
    { bitWidth :: Integer
    , intValue :: Integer
    }

  | FloatConstant Float
  deriving (Eq)

instance Show Constant where
  show (IntConstant _ v) = show v
  show (FloatConstant f) = show f



-----------
-- Classes
-----------

-- | Class for converting an LLVM symbol entity into a `Symbol`.
class SymbolFormable a where
  toSymbol :: a -> Symbol

instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = LocalStringSymbol str
  toSymbol (LLVM.UnName int) = TemporarySymbol $ toInteger int

-- | Class for converting an LLVM constant entity into a `Constant`.
class ConstantFormable a where
  toConstant :: a -> Constant

instance ConstantFormable LLVMC.Constant where
  toConstant (LLVMC.Int b v) = IntConstant (fromIntegral b) v
  toConstant l = error $ "'toConstant' not implemented for " ++ show l

-- | Class for processing an LLVM AST element.
class Processable a where

  -- | Processes an LLVM element, which builds the coresponding operation
  -- structure.
  process ::
      ProcessState
      -- ^ The current state.
    -> a
       -- ^ The LLVM element to process.
    -> ProcessState
       -- ^ The new state.



-------------
-- Functions
-------------

-- | Creates an initial state.
initialState :: ProcessState
initialState =
  ProcessState
  { theOS = OS.mkEmpty
  , lastTouchedNode = Nothing
  , theLabelNode = Nothing
  , theSymMaps = []
  , theConstMaps = []
  , thePhiData = []
  , theFuncInputs = []
  , theFuncRets = []
  }

-- | Builds a list of functions from an LLVM module. If the module does not
-- contain any globally defined functions, an empty list is returned.
mkFunctionsFromLlvmModule :: LLVM.Module -> [PM.Function]
mkFunctionsFromLlvmModule m =
  mapMaybe mkFunctionFromGlobalDef (LLVM.moduleDefinitions m)

-- | Builds a function from an LLVM AST definition. If the definition is not
-- global, `Nothing` is returned.
mkFunctionFromGlobalDef :: LLVM.Definition -> Maybe PM.Function
mkFunctionFromGlobalDef (LLVM.GlobalDefinition g) = mkFunction g
mkFunctionFromGlobalDef _ = Nothing

-- | Builds a function from a global LLVM AST definition. If the definition is
-- not a function, `Nothing` is returned.
mkFunction :: LLVM.Global -> Maybe PM.Function
mkFunction (LLVM.Function _ _ _ _ _ (LLVM.Name fname) (params, _) _ _ _ _ bbs) =
  let st1 = initialState
      st2 = process st1 params
      st3 = process st2 bbs
      st4 = fixPhiNodeEdgeOrderings st3
      st5 = addMissingInEdgesToDataNodes st4
      st6 = insertCopyNodes st5
  in Just (PM.Function
           { PM.functionName = fname
           , PM.functionOS = theOS st6
           , PM.functionInputs = theFuncInputs st6
           , PM.functionReturns = theFuncRets st6
           })
mkFunction _ = Nothing

-- | Gets the OS graph contained by the operation structure in a given state.
theOSGraph :: ProcessState -> G.Graph
theOSGraph = OS.osGraph . theOS

-- | Updates the OS graph contained by the operation structure in a given state.
updateOSGraph :: ProcessState -> G.Graph -> ProcessState
updateOSGraph st g =
  let os = theOS st
  in st { theOS = os { OS.osGraph = g } }

-- | Updates the last touched node information.
touchNode :: ProcessState -> G.Node -> ProcessState
touchNode st n = st { lastTouchedNode = Just n }

-- | Adds a new node into a given state.
addNewNode :: ProcessState -> G.NodeType -> ProcessState
addNewNode st0 nt =
  let (new_g, new_n) = G.addNewNode nt (theOSGraph st0)
      st1 = updateOSGraph st0 new_g
      st2 = touchNode st1 new_n
  in st2

-- | Adds a new edge into a given state.
addNewEdge ::
     ProcessState
     -- ^ The current state.
  -> G.EdgeType
  -> G.Node
     -- ^ The source node.
  -> G.Node
     -- ^ The destination node.
  -> ProcessState
     -- ^ The new state.
addNewEdge st et src dst =
  let (new_g, _) = G.addNewEdge et (src, dst) (theOSGraph st)
  in updateOSGraph st new_g

-- | Adds many new edges of the same type into a given state.
addNewEdgesManySources ::
     ProcessState
     -- ^ The current state.
  -> G.EdgeType
  -> [G.Node]
     -- ^ The source nodes.
  -> G.Node
     -- ^ The destination node.
  -> ProcessState
     -- ^ The new state.
addNewEdgesManySources st et srcs dst =
  let es = zip srcs (repeat dst)
      f g e = fst $ G.addNewEdge et e g
  in updateOSGraph st $ foldl f (theOSGraph st) es

-- | Adds many new edges of the same type into a given state.
addNewEdgesManyDests ::
     ProcessState
     -- ^ The current state.
  -> G.EdgeType
  -> G.Node
     -- ^ The source node.
  -> [G.Node]
     -- ^ The destination nodes.
  -> ProcessState
     -- ^ The new state.
addNewEdgesManyDests st et src dsts =
  let es = zip (repeat src) dsts
      f g e = fst $ G.addNewEdge et e g
  in updateOSGraph st $ foldl f (theOSGraph st) es

-- | Adds a new constraint into a given state.
addOSConstraint :: ProcessState -> C.Constraint -> ProcessState
addOSConstraint st c = st { theOS = OS.addConstraint (theOS st) c }

-- | Adds a list of new constraints into a given state.
addOSConstraints :: ProcessState -> [C.Constraint] -> ProcessState
addOSConstraints st cs = foldl addOSConstraint st cs

-- | Adds a new symbol-to-node mapping to a given state.
addSymMap :: ProcessState -> SymToNodeMapping -> ProcessState
addSymMap st sm = st { theSymMaps = theSymMaps st ++ [sm] }

-- | Adds a new constant-to-node mapping to a given state.
addConstMap :: ProcessState -> ConstToNodeMapping -> ProcessState
addConstMap st cm = st { theConstMaps = theConstMaps st ++ [cm] }

-- | Adds additional auxiliary phi node data to a given state.
addAuxPhiNodeData :: ProcessState -> AuxPhiNodeData -> ProcessState
addAuxPhiNodeData st ad = st { thePhiData = thePhiData st ++ [ad] }

-- | Adds a data node representing a function argument to a given state.
addFuncInput :: ProcessState -> G.Node -> ProcessState
addFuncInput st n = st { theFuncInputs = theFuncInputs st ++ [G.getNodeID n] }

-- | Adds a data node representing a return value to a given state.
addFuncRet :: ProcessState -> G.Node -> ProcessState
addFuncRet st n = st { theFuncRets = theFuncRets st ++ [G.getNodeID n] }

-- | Gets the node ID (if any) to which a symbol is mapped to.
mappedNodeFromSym :: [SymToNodeMapping] -> Symbol -> Maybe G.Node
mappedNodeFromSym ms sym =
  let ns = filter (\m -> snd m == sym) ms
  in if not $ null ns
     then Just $ fst $ last ns
     else Nothing

-- | Gets the node ID (if any) to which a constant is mapped to.
mappedNodeFromConst :: [ConstToNodeMapping] -> Constant -> Maybe G.Node
mappedNodeFromConst ms c =
  let ns = filter (\m -> snd m == c) ms
  in if not $ null ns
     then Just $ fst $ last ns
     else Nothing

-- | Processes a symbol. If a node mapping for that symbol already exists, then
-- the last touched node is updated to reflect that node. If a mapping does not
-- exist, then a new data node is added.
processSym :: ProcessState -> Symbol -> ProcessState
processSym st0 sym =
    let node_for_sym = mappedNodeFromSym (theSymMaps st0) sym
    in if isJust node_for_sym
       then touchNode st0 (fromJust node_for_sym)
       else let st1 = addNewNode st0 (G.DataNode D.AnyType (Just $ show sym))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addSymMap st1 (d_node, sym)
            in st2

-- | Processes a constant. If a node mapping for that constant already exists,
-- then the last touched node is updated to reflect that node. If a mapping does
-- not exist, then a new data node is added.
processConst :: ProcessState -> Constant -> ProcessState
processConst st0 c =
    let node_for_c = mappedNodeFromConst (theConstMaps st0) c
    in if isJust node_for_c
       then touchNode st0 (fromJust node_for_c)
       else let st1 = addNewNode
                      st0
                      (G.DataNode (toDataType c) (Just $ show c))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addConstMap st1 (d_node, c)
                st3 = addOSConstraints st2 (mkConstConstraints c d_node)
            in st3

mkConstConstraints :: Constant -> G.Node -> [C.Constraint]
mkConstConstraints (IntConstant _ v) n =
  [ C.BoolExprConstraint $
    C.DataNodeIsAnIntConstantExpr $ C.ANodeIDExpr $ G.getNodeID n
  , C.BoolExprConstraint $
    C.EqExpr
    ( C.Int2NumExpr $
      C.IntConstValueOfDataNodeExpr $
      C.ANodeIDExpr $ G.getNodeID n
    )
    ( C.Int2NumExpr $
      C.AnIntegerExpr v
    )
  ]

-- | Inserts a new node representing a computational operation, and adds edges
-- to that node from the given operands (which will also be processed).
processCompOp ::
  (Processable o)
  => ProcessState
  -> Op.CompOp
     -- ^ The computational operation.
  -> [o]
     -- ^ The operands.
  -> ProcessState
processCompOp st0 op operands =
  let sts = scanl process st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ComputationNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
  in st3

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current label node and operands (which will also be processed).
processControlOp ::
  (Processable o)
  => ProcessState
  -> Op.ControlOp
     -- ^ The control operation.
  -> [o]
     -- ^ The operands.
  -> ProcessState
processControlOp st0 op operands =
  let sts = scanl process st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ControlNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdge
            st2
            G.ControlFlowEdge
            (fromJust $ theLabelNode st2)
            op_node
      st4 = addNewEdgesManySources
            st3
            G.ControlFlowEdge
            operand_ns
            op_node
  in st4

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

-- | Gets the corresponding DataType for a constant value.
toDataType :: Constant -> D.DataType
toDataType (IntConstant b _) = D.IntType (toNatural b)
toDataType c = error $ "'toDataType' not implemented for " ++ show c

-- | Gets the label node with a particular name in the graph of the given state.
-- If no such node exists, `Nothing` is returned.
getLabelNode :: ProcessState -> G.BBLabelID -> Maybe G.Node
getLabelNode st l =
  let label_nodes = filter G.isLabelNode $ G.getAllNodes $ theOSGraph st
      nodes_w_matching_labels =
        filter (\n -> (G.bbLabel $ G.getNodeType n) == l) label_nodes
  in if length nodes_w_matching_labels > 0
     then Just (head nodes_w_matching_labels)
     else Nothing

-- | Checks that a label node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated to reflect the
-- label node in question. If not then a new label node is added.
ensureLabelNodeExists :: ProcessState -> G.BBLabelID -> ProcessState
ensureLabelNodeExists st l =
  let label_node = getLabelNode st l
  in if isJust label_node
     then touchNode st (fromJust label_node)
     else addNewNode st (G.LabelNode l)


-- | Corrects the edge ordering of the phi nodes.
fixPhiNodeEdgeOrderings :: ProcessState -> ProcessState
fixPhiNodeEdgeOrderings st = foldl fixPhiNodeEdgeOrdering st (thePhiData st)

-- | Corrects the edge ordering for a single phi node.
fixPhiNodeEdgeOrdering :: ProcessState -> AuxPhiNodeData -> ProcessState
fixPhiNodeEdgeOrdering st (phi_node, phi_labels) =
  let g = theOSGraph st
      in_edges_of_phi_node = G.sortByEdgeNr
                             G.getInEdgeNr
                             (G.getInEdges g phi_node)
      -- The in-edge from the label node to the phi node is always first
      pred_l_node_of_phi = G.getSourceNode g (head in_edges_of_phi_node)
      pred_labels = getPredLabelsOfLabelNode g pred_l_node_of_phi
      pos_maps = computePosMapsOfPerm phi_labels pred_labels
      edge_nr_maps = zip
                     (tail in_edges_of_phi_node)
                     (map (+1) pos_maps)
  in foldl
     ( \st' (e, new_in_nr) ->
       let g' = theOSGraph st'
           new_e_label = G.EdgeLabel { G.edgeType = G.DataFlowEdge
                                     , G.outEdgeNr = (G.getOutEdgeNr e)
                                     , G.inEdgeNr = (G.toEdgeNr new_in_nr)
                                     }
           new_g = G.updateEdgeLabel new_e_label e g'
       in updateOSGraph st' new_g
     )
     st
     edge_nr_maps

-- | Gets a list of labels of the label nodes which are the predecessors of
-- another label node. The list is in the same order as the ordering of the
-- in-edges.
getPredLabelsOfLabelNode :: G.Graph -> G.Node -> [G.BBLabelID]
getPredLabelsOfLabelNode g l_node =
  -- The in-edges to the label node are always from control nodes, and for these
  -- their first in-edge is always from the label node to which they belong
  let in_edges = G.sortByEdgeNr G.getInEdgeNr $ G.getInEdges g l_node
      preds = map (G.getSourceNode g) in_edges
      sought_l_nodes =
        map
        ( G.getSourceNode g
          . head
          . G.sortByEdgeNr G.getInEdgeNr
          . G.getInEdges g
        )
        preds
  in map (G.bbLabel . G.getNodeType) sought_l_nodes

-- | Adds an edge from the root label node to all data edges which currently
-- have no in-bound edges (such data nodes represent either constants or input
-- arguments).
addMissingInEdgesToDataNodes :: ProcessState -> ProcessState
addMissingInEdgesToDataNodes st =
  let g = theOSGraph st
      all_d_nodes = filter G.isDataNode (G.getAllNodes g)
      d_nodes_with_no_in_edges =
        filter (\n -> (length $ G.getInEdges g n) == 0) all_d_nodes
      root_l_node = fromJust $ G.rootInCFG $ G.extractCFG g
      new_g = foldl
              (\g' e -> fst $ G.addNewEdge G.DataFlowEdge e g')
              g
              (zip (repeat root_l_node) d_nodes_with_no_in_edges)
  in updateOSGraph st new_g

-- | Inserts a copy node between between each use of a data node.
insertCopyNodes :: ProcessState -> ProcessState
insertCopyNodes st =
  let g = theOSGraph st
      all_d_nodes = filter G.isDataNode (G.getAllNodes g)
      all_out_edges_from_d_nodes = concatMap (G.getOutEdges g) all_d_nodes
      new_st = foldl insertCopy st all_out_edges_from_d_nodes
  in new_st

-- | Inserts a copy and data node along a given edge.
insertCopy :: ProcessState -> G.Edge -> ProcessState
insertCopy st0 e =
  let g0 = theOSGraph st0
      orig_src_n = G.getSourceNode g0 e
      orig_dst_n = G.getTargetNode g0 e
      -- Modify OS graph
      (g1, new_d_node) = G.insertNewNodeAlongEdge G.CopyNode e g0
      new_e = head $ G.getOutEdges g1 new_d_node
      (g2, _) = G.insertNewNodeAlongEdge (G.DataNode D.AnyType Nothing) new_e g1
      st1 = updateOSGraph st0 g2
  in if G.isRetControlNode orig_dst_n
     -- Correct function return data node entries
     then let rets = [ nid | nid <- theFuncRets st1
                           , nid /= G.getNodeID orig_src_n
                     ]
          in st1 { theFuncRets = (G.getNodeID new_d_node):rets }
     else st1



---------------------------------
-- 'Processable' class instances
---------------------------------

instance (Processable a) => Processable [a] where
  process = foldl process

instance (Processable n) => Processable (LLVM.Named n) where
  process st0 (name LLVM.:= expr) =
    let st1 = process st0 expr
        expr_node = fromJust $ lastTouchedNode st1
        st2 = process st1 name
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 G.DataFlowEdge expr_node dst_node
    in st3
  process st (LLVM.Do expr) = process st expr

instance Processable LLVM.BasicBlock where
  process st0 (LLVM.BasicBlock (LLVM.Name str) insts term_inst) =
    let st1 = ensureLabelNodeExists st0 (G.BBLabelID str)
        st2 = st1 { theLabelNode = lastTouchedNode st1 }
        st3 = foldl process st2 insts
        st4 = process st3 term_inst
    in st4
  process _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "'process' not supported for un-named basic blocks"

instance Processable LLVM.Name where
  process st name@(LLVM.Name _) = processSym st (toSymbol name)
  process st name@(LLVM.UnName _) = processSym st (toSymbol name)

instance Processable LLVM.Instruction where
  process st (LLVM.Add  _ _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.Add) [op1, op2]
  process st (LLVM.FAdd op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.FloatOp Op.Add) [op1, op2]
  process st (LLVM.Sub  _ _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.Sub) [op1, op2]
  process st (LLVM.FSub op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.FloatOp Op.Sub) [op1, op2]
  process st (LLVM.Mul _ _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.Mul) [op1, op2]
  process st (LLVM.FMul op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.FloatOp Op.Mul) [op1, op2]
  process st (LLVM.UDiv _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.UIntOp Op.Div) [op1, op2]
  process st (LLVM.SDiv _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.SIntOp Op.Div) [op1, op2]
  process st (LLVM.FDiv op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.FloatOp Op.Div) [op1, op2]
  process st (LLVM.URem op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.UIntOp Op.Rem) [op1, op2]
  process st (LLVM.SRem op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.SIntOp Op.Rem) [op1, op2]
  process st (LLVM.FRem op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.FloatOp Op.Rem) [op1, op2]
  process st (LLVM.Shl _ _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.Shl) [op1, op2]
  process st (LLVM.LShr _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.LShr) [op1, op2]
  process st (LLVM.AShr _ op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.AShr) [op1, op2]
  process st (LLVM.And op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.And) [op1, op2]
  process st (LLVM.Or op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.Or) [op1, op2]
  process st (LLVM.Xor op1 op2 _) =
    processCompOp st (Op.CompArithOp $ Op.IntOp Op.XOr) [op1, op2]
  process st (LLVM.ICmp p op1 op2 _) =
    processCompOp st (fromLlvmIPred p) [op1, op2]
  process st (LLVM.FCmp p op1 op2 _) =
    processCompOp st (fromLlvmFPred p) [op1, op2]
  process st0 (LLVM.Phi _ operands _) =
    let (ops, l_names) = unzip operands
        sts = scanl process st0 ops
        operand_ns = map (fromJust . lastTouchedNode) (tail sts)
        st1 = last sts
        st2 = addNewNode st1 G.PhiNode
        op_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge
              st2
              G.DataFlowEdge
              (fromJust $ theLabelNode st1)
              op_node
        -- Here we simply ignore the edge order, and then run a second pass to
        -- correct the order afterwards after all edges from the branches have
        -- been added
        st4 = addNewEdgesManySources st3 G.DataFlowEdge operand_ns op_node
        l_strs = map (\(LLVM.Name str) -> str) l_names
        labels = map G.BBLabelID l_strs
        st5 = addAuxPhiNodeData st4 (op_node, labels)
    in st5
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Terminator where
  process st0 (LLVM.Ret op _) =
    let st1 = processControlOp st0 Op.Ret (maybeToList op)
    in if isJust op
       then let ret_n = fromJust $ lastTouchedNode st1
                g = theOSGraph st1
                preds = map
                        (G.getSourceNode g)
                        (G.sortByEdgeNr G.getInEdgeNr $ G.getInEdges g ret_n)
                -- For return nodes, the edge from the value node always
                -- appears last when ordered by in-edge numbers
                d_node = last preds
            in addFuncRet st1 d_node
       else st1
  process st0 (LLVM.Br (LLVM.Name dst) _) =
    let st1 = processControlOp
              st0
              Op.Branch
              ([] :: [LLVM.Name]) -- The type signature is needed to please GHC
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (G.BBLabelID dst)
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 G.ControlFlowEdge br_node dst_node
    in st3
  process st0 (LLVM.CondBr op (LLVM.Name t_dst) (LLVM.Name f_dst) _) =
    let st1 = processControlOp st0 Op.CondBranch [op]
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (G.BBLabelID t_dst)
        t_dst_node = fromJust $ lastTouchedNode st2
        st3 = ensureLabelNodeExists st2 (G.BBLabelID f_dst)
        f_dst_node = fromJust $ lastTouchedNode st3
        st4 = addNewEdgesManyDests
              st3
              G.ControlFlowEdge
              br_node
              [t_dst_node, f_dst_node]
    in st4
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Operand where
  process st (LLVM.LocalReference name) = process st name
  process st (LLVM.ConstantOperand c) = processConst st (toConstant c)
  process _ o = error $ "'process' not supported for " ++ show o

instance Processable LLVM.Parameter where
  process st0 (LLVM.Parameter _ pname _) =
    let st1 = process st0 pname
        n = fromJust $ lastTouchedNode st1
        st2 = addFuncInput st1 n
    in st2
