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
import Language.InstructionSelection.PrettyPrint
import Language.InstructionSelection.Utils
import Data.Maybe



---------
-- Types
---------

-- | Represents a mapping from a symbol to a node currently in the operation
-- structure.

type SymToNodeMapping = (G.Node, Symbol)

-- | A tuple containing the intermediate data as the function is being
-- processed. This is also used during post-processing.

type ProcessState =
  ( OS.OpStructure     -- ^ The operation structure.
  , Maybe G.Node       -- ^ The last node (if any) that was touched. This is
                       -- used to simplifying edge insertion. This value is
                       -- ignored during post-processing.
  , Maybe G.Node       -- ^ The label node which represents the basic block
                       -- currently being processed.
  , [SymToNodeMapping] -- ^ List of node-to-symbol mappings. If there are more
                       -- than one mapping using the same symbol, then the last
                       -- one occuring in the list should be picked.
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



-----------
-- Classes
-----------

-- | Class for converting an entity into a symbol.

class SymbolFormable a where
  toSymbol :: a -> Symbol

instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = LocalStringSymbol str
  toSymbol (LLVM.UnName int) = TemporarySymbol $ toInteger int



-- | Class for processing an LLVM AST element.

class Processable a where

  -- | Processes an LLVM element, which builds the coresponding operation
  -- structure.

  process :: ProcessState    -- ^ The current state.
             -> a            -- ^ The LLVM element to process.
             -> ProcessState -- ^ The new state.

-- | Class for post-processing an LLVM AST element.

class PostProcessable a where

  -- | Post-processes an LLVM element. This is done to correct the edge ordering
  -- of the phi nodes, which is difficult to get right when the function is
  -- processed the first time.

  postProcess :: ProcessState    -- ^ The current state.
                 -> a            -- ^ The LLVM element to post-process.
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
  let st1 = process (OS.mkEmpty, Nothing, Nothing, []) bbs
      st2 = postProcess st1 bbs
      os = currentOS st2
  in Just (PM.Function name os)
mkFunction _ = Nothing

-- | Gets the operation structure from the state tuple.

currentOS :: ProcessState -> OS.OpStructure
currentOS (os, _, _, _) = os

-- | Updates the state with a new operation structure.

updateOS :: ProcessState -> OS.OpStructure -> ProcessState
updateOS (_, n, l, ms) os = (os, n, l, ms)

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
lastTouchedNode (_, n, _, _) = n

-- | Updates the last touched node of a given state.

updateLastTouchedNode :: ProcessState -> G.Node -> ProcessState
updateLastTouchedNode (os, _, l, ms) n = (os, Just n, l, ms)

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
currentLabelNode (_, _, l, _) = l

-- | Updates the label node of a given state.

updateLabelNode :: ProcessState -> G.Node -> ProcessState
updateLabelNode (os, n, _, ms) l = (os, n, Just l, ms)

-- | Gets the list of mappings of a given state.

currentMappings :: ProcessState -> [SymToNodeMapping]
currentMappings (_, _, _, ms) = ms

-- | Adds a new mapping to a given state.

addMapping :: ProcessState -> SymToNodeMapping -> ProcessState
addMapping (os, n, l, ms) m = (os, n, l, ms ++ [m])

-- | Gets the node ID (if any) to which a symbol is mapped to.

mappedNodeFromSym :: [SymToNodeMapping] -> Symbol -> Maybe G.Node
mappedNodeFromSym ms sym =
  let ns = filter (\m -> snd m == sym) ms
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
    let node_for_sym = mappedNodeFromSym (currentMappings st0) sym
    in if isJust node_for_sym
       then updateLastTouchedNode st0 (fromJust node_for_sym)
       else let st1 = addNewNode st0 (G.DataNode D.AnyType (Just $ show sym))
                d_node = fromJust $ lastTouchedNode st1
                st2 = addMapping st1 (d_node, sym)
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
    let processPhiElem st' (op, _) = process st' op
        sts = scanl processPhiElem st0 operands
        operand_ns = map (fromJust . lastTouchedNode) (tail sts)
        st1 = last sts
        st2 = addNewNode st1 G.PhiNode
        op_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 (fromJust $ currentLabelNode st1) op_node
        -- Here we simply ignore the edge order, and then run a second pass to
        -- correct the order afterwards after all edges from the branches have
        -- been added
        st4 = addNewEdgesManySources st3 operand_ns op_node
    in st4
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
  process st (LLVM.ConstantOperand c) = process st c

instance Processable LLVMC.Constant where
  process st0 (LLVMC.Int b v) =
    let st1 = addNewNode st0 (G.DataNode (fromIWidth b) (Just $ show v))
        n = fromJust $ lastTouchedNode st1
        -- TODO: add constant constraint
        -- st2 = addConstraint st1 (C.EqExpr (C.AnIntegerExpr v) ())
        st2 = st1
    in st2
  process _ l = error $ "'process' not implemented for " ++ show l



-------------------------------------
-- 'PostProcessable' class instances
-------------------------------------

instance (PostProcessable a) => PostProcessable [a] where
  postProcess = foldl postProcess

instance (PostProcessable n) => PostProcessable (LLVM.Named n) where
  postProcess st (_ LLVM.:= expr) = postProcess st expr
  postProcess st (LLVM.Do expr) = postProcess st expr

instance PostProcessable LLVM.BasicBlock where
  postProcess st0 (LLVM.BasicBlock (LLVM.Name str) insts _) =
    let st1 = updateLabelNode st0 (fromJust $ getLabelNode st0 (G.BBLabel str))
        st2 = foldl postProcess st1 insts
    in st2

instance PostProcessable LLVM.Instruction where
  postProcess st0 (LLVM.Phi _ operands _) =
    -- TODO: implement
    st0
  postProcess st _ = st
