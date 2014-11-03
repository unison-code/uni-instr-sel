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
--------------------------------------------------------------------------------

module Language.InstSel.ProgramModules.LLVM.FunctionMaker
  ( mkFunctionsFromLlvmModule
  , mkFunction
  )
where

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified LLVM.General.AST.Global as LLVMG
import qualified LLVM.General.AST.IntegerPredicate as LLVMI
import qualified Language.InstSel.Constraints as C
import qualified Language.InstSel.DataTypes as D
import qualified Language.InstSel.Graphs as G
import qualified Language.InstSel.OpStructures as OS
import qualified Language.InstSel.OpTypes as Op
import qualified Language.InstSel.ProgramModules.Base as PM
import Language.InstSel.Utils
  ( toNatural )
import Data.Maybe



--------------
-- Data types
--------------

-- | Represents a mapping from a symbol to a data node currently in the graph.
type SymToDataNodeMapping = (G.Node, Symbol)

-- | Represents a mapping from constant symbol to a data node currently in the
-- graph.
type ConstToDataNodeMapping = (G.Node, Constant)

-- | Represents a data flow that goes from the label node, identified by a given
-- ID, to some given node. This is needed to draw the missing data flow edges
-- after both the data flow graph and the control flow graph have been built.
type LabelToNodeDF = (G.BBLabelID, G.Node)

-- | Represents a definition placement condition, stating that the given node
-- must be defined in the basic block identified by the given ID. This is needed
-- to draw the missing definition placement edges after both the data flow graph
-- and the control flow graph have been built.
type DefPlaceCond = (G.Node, G.BBLabelID)

-- | Represents the intermediate build data.
data BuildState =
    BuildState { osGraph :: OS.OpStructure
                 -- ^ The current operation structure.

               , lastTouchedNode :: Maybe G.Node
                 -- ^ The last node (if any) that was touched. This is used to
                 -- simplifying edge insertion.

               , currentLabelNode :: Maybe G.Node
                 -- ^ The label node which represents the basic block currently
                 -- being processed.

               , symMaps :: [SymToDataNodeMapping]
                 -- ^ List of symbol-to-node mappings. If there are more than
                 -- one mapping using the same symbol, then the last one
                 -- occuring in the list should be picked.

               , constMaps :: [ConstToDataNodeMapping]
                 -- ^ List of constant-to-node mappings. If there are more than
                 -- one mapping using the same symbol, then the last one
                 -- occuring in the list should be picked.

               , labelToNodeDFs :: [LabelToNodeDF]
                 -- ^ List of label-to-node data flow dependencies which are yet
                 -- to be converted into edges.

               , defPlaceConds :: [DefPlaceCond]
                 -- ^ List of definition placement conditions which are yet to
                 -- be converted into edges.

               , funcInputValues :: [G.NodeID]
                 -- ^ The IDs of the nodes representing the function input
                 -- arguments.
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
    IntConstant { bitWidth :: Integer
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

-- | Class for building the data flow graph.
class DfgBuildable a where
  -- | Builds the corresponding data flow graph from a given LLVM element.
  buildDfg ::
      BuildState
      -- ^ The current build state.
    -> a
       -- ^ The LLVM element to process.
    -> BuildState
       -- ^ The new build state.

-- | Class for building the control flow graph.
class CfgBuildable a where
  -- | Builds the corresponding control flow graph from a given LLVM element.
  buildCfg ::
      BuildState
      -- ^ The current build state.
    -> a
       -- ^ The LLVM element to process.
    -> BuildState
       -- ^ The new build state.



-------------
-- Functions
-------------

-- | Creates an initial state.
mkInitBuildState :: BuildState
mkInitBuildState =
  BuildState { osGraph = OS.mkEmpty
             , lastTouchedNode = Nothing
             , currentLabelNode = Nothing
             , symMaps = []
             , constMaps = []
             , labelToNodeDFs = []
             , defPlaceConds = []
             , funcInputValues = []
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
mkFunction f@(LLVM.Function {}) =
  let st0 = mkInitBuildState
      st1 = buildDfg st0 f
      st2 = buildCfg st1 f
      st3 = addMissingLabelToNodeDataFlowEdges st2
      st4 = addMissingDefPlaceEdges st3
  in Just ( PM.Function { PM.functionName = toFunctionName $ LLVMG.name f
                        , PM.functionOS = osGraph st4
                        , PM.functionInputs = funcInputValues st4
                        }
          )
mkFunction _ = Nothing

toFunctionName :: LLVM.Name -> Maybe String
toFunctionName (LLVM.Name str) = Just str
toFunctionName (LLVM.UnName _) = Nothing

-- | Gets the OS graph contained by the operation structure in a given state.
getOSGraph :: BuildState -> G.Graph
getOSGraph = OS.osGraph . osGraph

-- | Updates the OS graph contained by the operation structure in a given state.
updateOSGraph :: BuildState -> G.Graph -> BuildState
updateOSGraph st g =
  let os = osGraph st
  in st { osGraph = os { OS.osGraph = g } }

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

-- | Adds a new edge into a given state.
addNewEdge ::
     BuildState
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
addNewEdgesManySources ::
     BuildState
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
addNewEdgesManyDests ::
     BuildState
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

-- | Adds a new constraint into a given state.
addOSConstraint :: BuildState -> C.Constraint -> BuildState
addOSConstraint st c = st { osGraph = OS.addConstraint (osGraph st) c }

-- | Adds a list of new constraints into a given state.
addOSConstraints :: BuildState -> [C.Constraint] -> BuildState
addOSConstraints st cs = foldl addOSConstraint st cs

-- | Adds a new symbol-to-node mapping to a given state.
addSymMap :: BuildState -> SymToDataNodeMapping -> BuildState
addSymMap st sm = st { symMaps = symMaps st ++ [sm] }

-- | Adds a new constant-to-node mapping to a given state.
addConstMap :: BuildState -> ConstToDataNodeMapping -> BuildState
addConstMap st cm = st { constMaps = constMaps st ++ [cm] }

-- | Adds a list of definition placement conditions to a given state.
addDefPlaceConds :: BuildState -> [DefPlaceCond] -> BuildState
addDefPlaceConds st dps = st { defPlaceConds = defPlaceConds st ++ dps }

-- | Adds a data node representing a function argument to a given state.
addFuncInputValue :: BuildState -> G.Node -> BuildState
addFuncInputValue st n =
  st { funcInputValues = funcInputValues st ++ [G.getNodeID n] }

-- | Gets the node ID (if any) of the data node to which a symbol is mapped to.
mappedDataNodeFromSym :: [SymToDataNodeMapping] -> Symbol -> Maybe G.Node
mappedDataNodeFromSym ms sym =
  let ns = filter (\m -> snd m == sym) ms
  in if not $ null ns
     then Just $ fst $ last ns
     else Nothing

-- | Gets the node ID (if any) of the data node to which a constant is mapped
-- to.
mappedDataNodeFromConst :: [ConstToDataNodeMapping] -> Constant -> Maybe G.Node
mappedDataNodeFromConst ms c =
  let ns = filter (\m -> snd m == c) ms
  in if not $ null ns
     then Just $ fst $ last ns
     else Nothing

-- | Builds the corresponding operation structure from a symbol. If a node
-- mapping for that symbol already exists, then the last touched node is updated
-- to reflect that node. If a mapping does not exist, then a new data node is
-- added.
buildOSFromSym :: BuildState -> Symbol -> BuildState
buildOSFromSym st0 sym =
    let node_for_sym = mappedDataNodeFromSym (symMaps st0) sym
    in if isJust node_for_sym
       then touchNode st0 (fromJust node_for_sym)
       else let st1 = addNewNode st0 (G.DataNode D.AnyType (Just $ show sym))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addSymMap st1 (d_node, sym)
            in st2

-- | Builds the corresponding operation structure from a constant value. If a
-- node mapping for that constant already exists, then the last touched node is
-- updated to reflect that node. If a mapping does not exist, then a new data
-- node is added.
buildOSFromConst :: BuildState -> Constant -> BuildState
buildOSFromConst st0 c =
    let node_for_c = mappedDataNodeFromConst (constMaps st0) c
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
buildDfgFromCompOp ::
  (DfgBuildable o)
  => BuildState
  -> Op.CompOp
     -- ^ The computational operation.
  -> [o]
     -- ^ The operands.
  -> BuildState
buildDfgFromCompOp st0 op operands =
  let sts = scanl buildDfg st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ComputationNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
  in st3

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current label node and operands (which will also be processed).
buildCfgFromControlOp ::
  (CfgBuildable o)
  => BuildState
  -> Op.ControlOp
     -- ^ The control operation.
  -> [o]
     -- ^ The operands.
  -> BuildState
buildCfgFromControlOp st0 op operands =
  let sts = scanl buildCfg st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ControlNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdge
            st2
            G.ControlFlowEdge
            (fromJust $ currentLabelNode st2)
            op_node
      st4 = addNewEdgesManySources
            st3
            G.DataFlowEdge
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
findLabelNodeWithID :: BuildState -> G.BBLabelID -> Maybe G.Node
findLabelNodeWithID st l =
  let label_nodes = filter G.isLabelNode $ G.getAllNodes $ getOSGraph st
      nodes_w_matching_labels =
        filter (\n -> (G.bbLabel $ G.getNodeType n) == l) label_nodes
  in if length nodes_w_matching_labels > 0
     then Just (head nodes_w_matching_labels)
     else Nothing

-- | Checks that a label node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated to reflect the
-- label node in question. If not then a new label node is added.
ensureLabelNodeExists :: BuildState -> G.BBLabelID -> BuildState
ensureLabelNodeExists st l =
  let label_node = findLabelNodeWithID st l
  in if isJust label_node
     then touchNode st (fromJust label_node)
     else addNewNode st (G.LabelNode l)

-- | Adds the missing data or state flow edges from label nodes to data or state
-- nodes, as described in the given build state.
addMissingLabelToNodeDataFlowEdges :: BuildState -> BuildState
addMissingLabelToNodeDataFlowEdges st =
  let g0 = getOSGraph st
      deps = labelToNodeDFs st
      -- Add data flow edges
      df_deps = filter (\(_, n) -> G.isDataNode n) deps
      df_pairs = map
                 (\(bb_id, n) -> (fromJust $ findLabelNodeWithID st bb_id, n))
                 df_deps
      g1 = foldr (\p g -> fst $ G.addNewDFEdge p g) g0 df_pairs
      -- Add state flow edges
      sf_deps = filter (\(_, n) -> G.isStateNode n) deps
      sf_pairs = map
                 (\(bb_id, n) -> (fromJust $ findLabelNodeWithID st bb_id, n))
                 sf_deps
      g2 = foldr (\p g -> fst $ G.addNewSFEdge p g) g1 sf_pairs
  in updateOSGraph st g2

-- | Adds the missing definition placement edges, as described in the given
-- build state.
addMissingDefPlaceEdges :: BuildState -> BuildState
addMissingDefPlaceEdges st =
  let g0 = getOSGraph st
      conds = defPlaceConds st
      dp_pairs = map
                 (\(n, bb_id) -> (n, fromJust $ findLabelNodeWithID st bb_id))
                 conds
      g1 = foldr (\p g -> fst $ G.addNewDPEdge p g) g0 dp_pairs
  in updateOSGraph st g1



---------------------------------
-- 'DfgBuildable' class instances
---------------------------------

instance (DfgBuildable a) => DfgBuildable [a] where
  buildDfg = foldl buildDfg

instance (DfgBuildable n) => DfgBuildable (LLVM.Named n) where
  buildDfg st0 (name LLVM.:= expr) =
    let st1 = buildDfg st0 expr
        expr_node = fromJust $ lastTouchedNode st1
        st2 = buildDfg st1 name
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 G.DataFlowEdge expr_node dst_node
    in st3
  buildDfg st (LLVM.Do expr) = buildDfg st expr

instance DfgBuildable LLVM.Global where
  buildDfg st0 f@(LLVM.Function {}) =
    let (params, _) = LLVMG.parameters f
        st1 = buildDfg st0 params
        st2 = buildDfg st1 $ LLVMG.basicBlocks f
    in st2
  buildDfg _ l = error $ "'buildDfg' not supported for " ++ show l

instance DfgBuildable LLVM.BasicBlock where
  buildDfg st0 (LLVM.BasicBlock (LLVM.Name str) insts _) =
    let st1 = ensureLabelNodeExists st0 (G.BBLabelID str)
        st2 = st1 { currentLabelNode = lastTouchedNode st1 }
        st3 = foldl buildDfg st2 insts
    in st3
  buildDfg _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "'buildDfg' not supported for un-named basic blocks"

instance DfgBuildable LLVM.Name where
  buildDfg st name@(LLVM.Name _) = buildOSFromSym st (toSymbol name)
  buildDfg st name@(LLVM.UnName _) = buildOSFromSym st (toSymbol name)

instance DfgBuildable LLVM.Instruction where
  buildDfg st (LLVM.Add  _ _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.Add) [op1, op2]
  buildDfg st (LLVM.FAdd op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.FloatOp Op.Add) [op1, op2]
  buildDfg st (LLVM.Sub  _ _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.Sub) [op1, op2]
  buildDfg st (LLVM.FSub op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.FloatOp Op.Sub) [op1, op2]
  buildDfg st (LLVM.Mul _ _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.Mul) [op1, op2]
  buildDfg st (LLVM.FMul op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.FloatOp Op.Mul) [op1, op2]
  buildDfg st (LLVM.UDiv _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.UIntOp Op.Div) [op1, op2]
  buildDfg st (LLVM.SDiv _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.SIntOp Op.Div) [op1, op2]
  buildDfg st (LLVM.FDiv op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.FloatOp Op.Div) [op1, op2]
  buildDfg st (LLVM.URem op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.UIntOp Op.Rem) [op1, op2]
  buildDfg st (LLVM.SRem op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.SIntOp Op.Rem) [op1, op2]
  buildDfg st (LLVM.FRem op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.FloatOp Op.Rem) [op1, op2]
  buildDfg st (LLVM.Shl _ _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.Shl) [op1, op2]
  buildDfg st (LLVM.LShr _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.LShr) [op1, op2]
  buildDfg st (LLVM.AShr _ op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.AShr) [op1, op2]
  buildDfg st (LLVM.And op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.And) [op1, op2]
  buildDfg st (LLVM.Or op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.Or) [op1, op2]
  buildDfg st (LLVM.Xor op1 op2 _) =
    buildDfgFromCompOp st (Op.CompArithOp $ Op.IntOp Op.XOr) [op1, op2]
  buildDfg st (LLVM.ICmp p op1 op2 _) =
    buildDfgFromCompOp st (fromLlvmIPred p) [op1, op2]
  buildDfg st (LLVM.FCmp p op1 op2 _) =
    buildDfgFromCompOp st (fromLlvmFPred p) [op1, op2]
  buildDfg st0 (LLVM.Phi _ phi_operands _) =
    let (operands, label_names) = unzip phi_operands
        bb_labels = map (\(LLVM.Name str) -> G.BBLabelID str) label_names
        operand_node_sts = scanl buildDfg st0 operands
        operand_ns = map (fromJust . lastTouchedNode) (tail operand_node_sts)
        st1 = last operand_node_sts
        st2 = addDefPlaceConds st1 (zip operand_ns bb_labels)
        st3 = addNewNode st2 G.PhiNode
        op_node = fromJust $ lastTouchedNode st3
        st4 = addNewEdgesManySources st3 G.DataFlowEdge operand_ns op_node
    in st4
  buildDfg _ l = error $ "'buildDfg' not implemented for " ++ show l

instance DfgBuildable LLVM.Operand where
  buildDfg st (LLVM.LocalReference name) = buildDfg st name
  buildDfg st (LLVM.ConstantOperand c) = buildOSFromConst st (toConstant c)
  buildDfg _ o = error $ "'buildDfg' not supported for " ++ show o

instance DfgBuildable LLVM.Parameter where
  buildDfg st0 (LLVM.Parameter _ pname _) =
    let st1 = buildDfg st0 pname
        n = fromJust $ lastTouchedNode st1
        st2 = addFuncInputValue st1 n
    in st2



---------------------------------
-- 'CfgBuildable' class instances
---------------------------------

instance (CfgBuildable a) => CfgBuildable [a] where
  buildCfg = foldl buildCfg

instance CfgBuildable LLVM.BasicBlock where
  buildCfg st0 (LLVM.BasicBlock (LLVM.Name str) _ term_inst) =
    let st1 = ensureLabelNodeExists st0 (G.BBLabelID str)
        st2 = st1 { currentLabelNode = lastTouchedNode st1 }
        st3 = buildCfg st2 term_inst
    in st3
  buildCfg _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "'buildCfg' not supported for un-named basic blocks"

instance (CfgBuildable n) => CfgBuildable (LLVM.Named n) where
  buildCfg st (_ LLVM.:= expr) = buildCfg st expr
  buildCfg st (LLVM.Do expr) = buildCfg st expr

instance CfgBuildable LLVM.Global where
  buildCfg st f@(LLVM.Function {}) =
    buildCfg st $ LLVMG.basicBlocks f
  buildCfg _ l = error $ "'buildCfg' not supported for " ++ show l

instance CfgBuildable LLVM.Terminator where
  buildCfg st (LLVM.Ret op _) =
    buildCfgFromControlOp st Op.Ret (maybeToList op)
  buildCfg st0 (LLVM.Br (LLVM.Name dst) _) =
    let st1 = buildCfgFromControlOp
              st0
              Op.Branch
              ([] :: [LLVM.Name]) -- The type signature is needed to please GHC
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (G.BBLabelID dst)
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 G.ControlFlowEdge br_node dst_node
    in st3
  buildCfg st0 (LLVM.CondBr op (LLVM.Name t_dst) (LLVM.Name f_dst) _) =
    let st1 = buildCfgFromControlOp st0 Op.CondBranch [op]
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
  buildCfg _ l = error $ "'buildCfg' not implemented for " ++ show l

instance CfgBuildable LLVM.Operand where
  buildCfg st (LLVM.LocalReference name) = buildCfg st name
  buildCfg st (LLVM.ConstantOperand c) = buildOSFromConst st (toConstant c)
  buildCfg _ o = error $ "'buildCfg' not supported for " ++ show o

instance CfgBuildable LLVM.Name where
  buildCfg st name@(LLVM.Name _) = buildOSFromSym st (toSymbol name)
  buildCfg st name@(LLVM.UnName _) = buildOSFromSym st (toSymbol name)
