--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.ProgramModules.LLVM.FunctionMaker
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

module Language.InstructionSelection.ProgramModules.LLVM.FunctionMaker (
  mkFunctionsFromLlvmModule
, mkFunction
) where

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.IntegerPredicate as LLVMI
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified Language.InstructionSelection.Constraints as C
import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OpStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.ProgramModules.Base as PM
import Language.InstructionSelection.DataTypes as D
import Language.InstructionSelection.Utils ( computePosMapsOfPerm
                                           , toNatural
                                           )
import Data.Maybe



---------
-- Types
---------

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

type AuxPhiNodeData = (G.Node, [G.BBLabel])

-- | A tuple containing the intermediate data as the function is being
-- processed.

type ProcessState =
  ( OS.OpStructure       -- ^ The operation structure.
  , Maybe G.Node         -- ^ The last node (if any) that was touched. This is
                         -- used to simplifying edge insertion. This value is
                         -- ignored during post-processing.
  , Maybe G.Node         -- ^ The label node which represents the basic block
                         -- currently being processed.
  , [SymToNodeMapping]   -- ^ List of symbol-to-node mappings. If there are more
                         -- than one mapping using the same symbol, then the
                         -- last one occuring in the list should be picked.
  , [ConstToNodeMapping] -- ^ List of constant-to-node mappings. If there are
                         -- more than one mapping using the same symbol, then
                         -- the last one occuring in the list should be picked.
  , [AuxPhiNodeData]     -- ^ Auxiliary phi node data (see description of the
                         -- type for more information).
  )



--------------
-- Data types
--------------

-- | Data type for retaining various symbol names.

data Symbol
    = LocalStringSymbol String
    | GlobalStringSymbol String
    | TemporarySymbol Integer
    deriving (Eq)

instance Show Symbol where
  show (LocalStringSymbol str) = "%" ++ str
  show (GlobalStringSymbol str) = "@" ++ str
  show (TemporarySymbol int) = "t" ++ show int

-- | Data type for retaining various constant values.

data Constant
    = IntConstant

          -- | Bit width.

          Integer

          -- | Integer value.

          Integer

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

  process :: ProcessState    -- ^ The current state.
             -> a            -- ^ The LLVM element to process.
             -> ProcessState -- ^ The new state.



-------------
-- Functions
-------------

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
mkFunction (LLVM.Function _ _ _ _ _ (LLVM.Name name) _ _ _ _ _ bbs) =
  let st1 = process (OS.mkEmpty, Nothing, Nothing, [], [], []) bbs
      st2 = fixAllPhiNodeEdgeOrderings st1
      os = currentOS st2
  in Just (PM.Function name os)
mkFunction _ = Nothing

-- | Gets the operation structure from the state tuple.

currentOS :: ProcessState -> OS.OpStructure
currentOS (os, _, _, _, _, _) = os

-- | Updates the state with a new operation structure.

updateOS :: ProcessState -> OS.OpStructure -> ProcessState
updateOS (_, n, l, sms, cms, ads) os = (os, n, l, sms, cms, ads)

-- | Gets the graph contained by the operation structure in a given state.

currentGraph :: ProcessState -> G.Graph
currentGraph = OS.osGraph . currentOS

-- | Updates the graph contained by the operation structure in a given state.

updateGraph :: ProcessState -> G.Graph -> ProcessState
updateGraph st g = updateOS st $ OS.updateGraph (currentOS st) g

-- | Adds a new node into a given state.

addNewNode :: ProcessState -> G.NodeType -> ProcessState
addNewNode st0 nt =
  let (new_g, new_n) = G.addNewNode nt (currentGraph st0)
      st1 = updateGraph st0 new_g
      st2 = updateLastTouchedNode st1 new_n
  in st2

-- | Gets the last touched node in a given state. If the graph contained by the
-- operation structure in the state is empty, `Nothing` is returned.

lastTouchedNode :: ProcessState -> Maybe G.Node
lastTouchedNode (_, n, _, _, _, _) = n

-- | Updates the last touched node of a given state.

updateLastTouchedNode :: ProcessState -> G.Node -> ProcessState
updateLastTouchedNode (os, _, l, sms, cms, ads) n =
  (os, Just n, l, sms, cms, ads)

-- | Adds a new edge into a given state.

addNewEdge :: ProcessState    -- ^ The current state.
              -> G.Node       -- ^ The source node.
              -> G.Node       -- ^ The destination node.
              -> ProcessState -- ^ The new state.
addNewEdge st src dst =
  let (new_g, _) = G.addNewEdge (src, dst) (currentGraph st)
  in updateGraph st new_g

-- | Adds many new edges into a given state.

addNewEdgesManySources :: ProcessState    -- ^ The current state.
                          -> [G.Node]     -- ^ The source nodes.
                          -> G.Node       -- ^ The destination node.
                          -> ProcessState -- ^ The new state.
addNewEdgesManySources st srcs dst =
  let es = zip srcs (repeat dst)
      f g e = fst $ G.addNewEdge e g
  in updateGraph st $ foldl f (currentGraph st) es

-- | Adds many new edges into a given state.

addNewEdgesManyDests :: ProcessState    -- ^ The current state.
                        -> G.Node       -- ^ The source node.
                        -> [G.Node]     -- ^ The destination nodes.
                        -> ProcessState -- ^ The new state.
addNewEdgesManyDests st src dsts =
  let es = zip (repeat src) dsts
      f g e = fst $ G.addNewEdge e g
  in updateGraph st $ foldl f (currentGraph st) es

-- | Adds a new constraint into a given state.

addConstraint :: ProcessState -> C.Constraint -> ProcessState
addConstraint st c = updateOS st $ OS.addConstraint (currentOS st) c

-- | Gets the label node of a given state.

currentLabelNode :: ProcessState -> Maybe G.Node
currentLabelNode (_, _, l, _, _, _) = l

-- | Updates the label node of a given state.

updateLabelNode :: ProcessState -> G.Node -> ProcessState
updateLabelNode (os, n, _, sms, cms, ads) l = (os, n, Just l, sms, cms, ads)

-- | Gets the list of symbol-to-node mappings of a given state.

currentSymMaps :: ProcessState -> [SymToNodeMapping]
currentSymMaps (_, _, _, sms, _, _) = sms

-- | Adds a new symbol-to-node mapping to a given state.

addSymMap :: ProcessState -> SymToNodeMapping -> ProcessState
addSymMap (os, n, l, sms, cms, ads) sm = (os, n, l, sms ++ [sm], cms, ads)

-- | Gets the list of constant-to-node mappings of a given state.

currentConstMaps :: ProcessState -> [ConstToNodeMapping]
currentConstMaps (_, _, _, _, cms, _) = cms

-- | Adds a new constant-to-node mapping to a given state.

addConstMap :: ProcessState -> ConstToNodeMapping -> ProcessState
addConstMap (os, n, l, sms, cms, ads) cm = (os, n, l, sms, cms ++ [cm], ads)

-- | Gets the list of auxiliary phi node data of a given state.

currentAuxPhiNodeData :: ProcessState -> [AuxPhiNodeData]
currentAuxPhiNodeData (_, _, _, _, _, as) = as

-- | Adds additional auxiliary phi node data to a given state.

addAuxPhiNodeData :: ProcessState -> AuxPhiNodeData -> ProcessState
addAuxPhiNodeData (os, n, l, sms, cms, ads) ad =
  (os, n, l, sms, cms, ads ++ [ad])

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

processSym :: ProcessState    -- ^ The current state.
              -> Symbol       -- ^ The symbol.
              -> ProcessState -- ^ The new state.
processSym st0 sym =
    let node_for_sym = mappedNodeFromSym (currentSymMaps st0) sym
    in if isJust node_for_sym
       then updateLastTouchedNode st0 (fromJust node_for_sym)
       else let st1 = addNewNode st0 (G.DataNode D.AnyType (Just $ show sym))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addSymMap st1 (d_node, sym)
            in st2

-- | Processes a constant. If a node mapping for that constant already exists,
-- then the last touched node is updated to reflect that node. If a mapping does
-- not exist, then a new data node is added.

processConst :: ProcessState    -- ^ The current state.
                -> Constant     -- ^ The constant.
                -> ProcessState -- ^ The new state.
processConst st0 c =
    let node_for_c = mappedNodeFromConst (currentConstMaps st0) c
    in if isJust node_for_c
       then updateLastTouchedNode st0 (fromJust node_for_c)
       else let st1 = addNewNode st0 (G.DataNode (toDataType c)
                                                 (Just $ show c))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addConstMap st1 (d_node, c)
                -- TODO: add constant constraint
            in st2

-- | Inserts a new node representing a computational operation, and adds edges
-- to that node from the given operands (which will also be processed).

processCompOp :: (Processable o)
                 => ProcessState -- ^ The current state.
                 -> Op.CompOp    -- ^ The computational operation.
                 -> [o]          -- ^ The operands.
                 -> ProcessState -- ^ The new state.
processCompOp st0 op operands =
  let sts = scanl process st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ComputationNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 operand_ns op_node
  in st3

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current label node and operands (which will also be processed).

processControlOp :: (Processable o)
                    => ProcessState -- ^ The current state.
                    -> Op.ControlOp -- ^ The control operation.
                    -> [o]          -- ^ The operands.
                    -> ProcessState -- ^ The new state.
processControlOp st0 op operands =
  let sts = scanl process st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ControlNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdge st2 (fromJust $ currentLabelNode st2) op_node
      st4 = addNewEdgesManySources st3 operand_ns op_node
  in st4

-- | Converts an LLVM integer comparison op into an equivalent op of our own
-- data type.

fromLlvmIPred :: LLVMI.IntegerPredicate -> Op.CompOp
fromLlvmIPred LLVMI.EQ  =  Op.IntOp Op.Eq
fromLlvmIPred LLVMI.NE  =  Op.IntOp Op.NEq
fromLlvmIPred LLVMI.UGT = Op.UIntOp Op.GT
fromLlvmIPred LLVMI.ULT = Op.UIntOp Op.LT
fromLlvmIPred LLVMI.UGE = Op.UIntOp Op.GE
fromLlvmIPred LLVMI.ULE = Op.UIntOp Op.LE
fromLlvmIPred LLVMI.SGT = Op.SIntOp Op.GT
fromLlvmIPred LLVMI.SLT = Op.SIntOp Op.LT
fromLlvmIPred LLVMI.SGE = Op.SIntOp Op.GE
fromLlvmIPred LLVMI.SLE = Op.SIntOp Op.LE

-- | Converts an LLVM floating point comparison op into an equivalent op of our
-- own data type.

fromLlvmFPred :: LLVMF.FloatingPointPredicate -> Op.CompOp
fromLlvmFPred LLVMF.OEQ = Op.OFloatOp Op.Eq
fromLlvmFPred LLVMF.ONE = Op.OFloatOp Op.NEq
fromLlvmFPred LLVMF.OGT = Op.OFloatOp Op.GT
fromLlvmFPred LLVMF.OGE = Op.OFloatOp Op.GE
fromLlvmFPred LLVMF.OLT = Op.OFloatOp Op.LT
fromLlvmFPred LLVMF.OLE = Op.OFloatOp Op.LE
fromLlvmFPred LLVMF.ORD =  Op.FloatOp Op.Ordered
fromLlvmFPred LLVMF.UNO =  Op.FloatOp Op.Unordered
fromLlvmFPred LLVMF.UEQ = Op.UFloatOp Op.Eq
fromLlvmFPred LLVMF.UGT = Op.UFloatOp Op.GT
fromLlvmFPred LLVMF.UGE = Op.UFloatOp Op.GE
fromLlvmFPred LLVMF.ULT = Op.UFloatOp Op.LT
fromLlvmFPred LLVMF.ULE = Op.UFloatOp Op.LE
fromLlvmFPred LLVMF.UNE = Op.UFloatOp Op.NEq
fromLlvmFPred op = error $ "'fromLlvmFPred' not implemented for " ++ show op

-- | Gets the corresponding DataType for a constant value.

toDataType :: Constant -> D.DataType
toDataType (IntConstant b _) = D.IntType (toNatural b)
toDataType c = error $ "'toDataType' not implemented for " ++ show c

-- | Gets the label node with a particular name in the graph of the given state.
-- If no such node exists, `Nothing` is returned.

getLabelNode :: ProcessState -> G.BBLabel -> Maybe G.Node
getLabelNode st l =
  let label_nodes = filter G.isLabelNode $ G.allNodes $ currentGraph st
      nodes_w_matching_labels = filter (\n -> (G.bbLabel $ G.nodeType n) == l)
                                          label_nodes
  in if length nodes_w_matching_labels > 0
        then Just (head nodes_w_matching_labels)
        else Nothing

-- | Checks that a label node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated to reflect the
-- label node in question. If not then a new label node is added.

ensureLabelNodeExists :: ProcessState -> G.BBLabel -> ProcessState
ensureLabelNodeExists st l =
  let label_node = getLabelNode st l
  in if isJust label_node
        then updateLastTouchedNode st (fromJust label_node)
        else addNewNode st (G.LabelNode l)


-- | Corrects the edge ordering of the phi nodes.

fixAllPhiNodeEdgeOrderings :: ProcessState -> ProcessState
fixAllPhiNodeEdgeOrderings st =
  foldl fixPhiNodeEdgeOrdering st (currentAuxPhiNodeData st)

-- | Corrects the edge ordering for a single phi node.

fixPhiNodeEdgeOrdering :: ProcessState -> AuxPhiNodeData -> ProcessState
fixPhiNodeEdgeOrdering st (phi_node, phi_labels) =
  let g = currentGraph st
      in_edges_of_phi_node = G.sortEdgesByInNumbers $ G.inEdges g phi_node
      -- The in-edge from the label node to the phi node is always first
      pred_l_node_of_phi = G.sourceOfEdge g (head in_edges_of_phi_node)
      pred_labels = getPredLabelsOfLabelNode g pred_l_node_of_phi
      pos_maps = computePosMapsOfPerm phi_labels pred_labels
      edge_nr_maps = zip (tail in_edges_of_phi_node)
                         (map (+1) pos_maps)
  in foldl (\st'
            -> \(e, new_in_nr)
            -> let g' = currentGraph st'
                   new_e_label = G.EdgeLabel (G.outEdgeNr e)
                                             (G.toEdgeNr new_in_nr)
                   new_g = G.updateEdgeLabel new_e_label e g'
               in updateGraph st' new_g
           )
           st
           edge_nr_maps

-- | Gets a list of labels of the label nodes which are the predecessors of
-- another label node. The list is in the same order as the ordering of the
-- in-edges.

getPredLabelsOfLabelNode :: G.Graph -> G.Node -> [G.BBLabel]
getPredLabelsOfLabelNode g l_node =
  -- The in-edges to the label node are always from control nodes, and for these
  -- their first in-edge is always from the label node to which they belong
  let preds = G.predecessors g l_node
      l_preds_of_l_node =
        map (G.sourceOfEdge g . head . G.sortEdgesByInNumbers . G.inEdges g)
            preds
  in map (G.bbLabel . G.nodeType) l_preds_of_l_node



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
        st3 = addNewEdge st2 expr_node dst_node
    in st3
  process st (LLVM.Do expr) = process st expr

instance Processable LLVM.BasicBlock where
  process st0 (LLVM.BasicBlock (LLVM.Name str) insts term_inst) =
    let st1 = ensureLabelNodeExists st0 (G.BBLabel str)
        st2 = updateLabelNode st1 (fromJust $ lastTouchedNode st1)
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
    processCompOp st (Op.IntOp   Op.Add) [op1, op2]
  process st (LLVM.FAdd op1 op2 _) =
    processCompOp st (Op.FloatOp Op.Add) [op1, op2]
  process st (LLVM.Sub  _ _ op1 op2 _) =
    processCompOp st (Op.IntOp   Op.Sub) [op1, op2]
  process st (LLVM.FSub op1 op2 _) =
    processCompOp st (Op.FloatOp Op.Sub) [op1, op2]
  process st (LLVM.Mul _ _ op1 op2 _) =
    processCompOp st (Op.IntOp   Op.Mul) [op1, op2]
  process st (LLVM.FMul op1 op2 _) =
    processCompOp st (Op.FloatOp Op.Mul) [op1, op2]
  process st (LLVM.UDiv _ op1 op2 _) =
    processCompOp st (Op.UIntOp  Op.Div) [op1, op2]
  process st (LLVM.SDiv _ op1 op2 _) =
    processCompOp st (Op.SIntOp  Op.Div) [op1, op2]
  process st (LLVM.FDiv op1 op2 _) =
    processCompOp st (Op.FloatOp Op.Div) [op1, op2]
  process st (LLVM.URem op1 op2 _) =
    processCompOp st (Op.UIntOp  Op.Rem) [op1, op2]
  process st (LLVM.SRem op1 op2 _) =
    processCompOp st (Op.SIntOp  Op.Rem) [op1, op2]
  process st (LLVM.FRem op1 op2 _) =
    processCompOp st (Op.FloatOp Op.Rem) [op1, op2]
  process st (LLVM.Shl _ _ op1 op2 _) =
    processCompOp st (Op.IntOp   Op.Shl) [op1, op2]
  process st (LLVM.LShr _ op1 op2 _) =
    processCompOp st (Op.IntOp  Op.LShr) [op1, op2]
  process st (LLVM.AShr _ op1 op2 _) =
    processCompOp st (Op.IntOp  Op.AShr) [op1, op2]
  process st (LLVM.And op1 op2 _) =
    processCompOp st (Op.IntOp   Op.And) [op1, op2]
  process st (LLVM.Or op1 op2 _) =
    processCompOp st (Op.IntOp    Op.Or) [op1, op2]
  process st (LLVM.Xor op1 op2 _) =
    processCompOp st (Op.IntOp   Op.XOr) [op1, op2]
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
        st3 = addNewEdge st2 (fromJust $ currentLabelNode st1) op_node
        -- Here we simply ignore the edge order, and then run a second pass to
        -- correct the order afterwards after all edges from the branches have
        -- been added
        st4 = addNewEdgesManySources st3 operand_ns op_node
        l_strs = map (\(LLVM.Name str) -> str) l_names
        labels = map G.BBLabel l_strs
        st5 = addAuxPhiNodeData st4 (op_node, labels)
    in st5
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Terminator where
  process st (LLVM.Ret op _) = processControlOp st Op.Ret (maybeToList op)
  process st0 (LLVM.Br (LLVM.Name dst) _) =
    let st1 = processControlOp st0 Op.UncondBranch
              ([] :: [LLVM.Name]) -- The type signature is needed to please GHC
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (G.BBLabel dst)
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 br_node dst_node
    in st3
  process st0 (LLVM.CondBr op (LLVM.Name t_dst) (LLVM.Name f_dst) _) =
    let st1 = processControlOp st0 Op.CondBranch [op]
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (G.BBLabel t_dst)
        t_dst_node = fromJust $ lastTouchedNode st2
        st3 = ensureLabelNodeExists st2 (G.BBLabel f_dst)
        f_dst_node = fromJust $ lastTouchedNode st3
        st4 = addNewEdgesManyDests st3 br_node [t_dst_node, f_dst_node]
    in st4
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Operand where
  process st (LLVM.LocalReference name) = process st name
  process st (LLVM.ConstantOperand c) = processConst st (toConstant c)
  process _ o = error $ "'process' not supported for " ++ show o
