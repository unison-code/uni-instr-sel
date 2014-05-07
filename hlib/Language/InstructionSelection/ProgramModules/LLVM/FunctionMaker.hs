--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM.FunctionMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts and LLVM IR module into the internal function format.
--
-- Since only the function name is retained, the names of overloaded functions
-- must have been resolved such that each is given a unique name.
--
-- TODO: add data type information to data nodes
-- TODO: fix edge order for phi nodes
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

-- | Represents a mapping from a symbol to a node ID currently in the operation
-- structure.

type Mapping = (G.NodeId, Symbol)

-- | A tuple containing the intermediate data as the function is built.

type State = ( OS.OpStructure -- ^ The corresponding operation structure.
             , Maybe G.Node   -- ^ The last node (if any) that was touched. This
                              -- is used to simplifying edge insertion.
             , Maybe G.Node   -- ^ The label node which represents the basic
                              -- block currently being processed.
             , [Mapping]      -- ^ List of node-to-symbol mappings. If
                              -- there are more than one mapping using the
                              -- same symbol, then the last one occuring
                              -- in the list should be picked.
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

  -- | Extends a given 'OpStructure' with the information given by an LLVM data
  -- type, and by doing so the function is built. The first is given via a tuple
  -- which contains other auxiliary information needed for this process. This is
  -- called the 'state'.
  --
  -- NOTE: Each declaration node and use node will be kept separate to be merged
  -- as a post-step.

  process :: State    -- ^ Current state.
             -> a     -- ^ The LLVM data type to process.
             -> State -- ^ The new, extended state.



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
  let (os, _, _, _) = (process (OS.mkEmpty, Nothing, Nothing, []) bbs)
  in Just $ PM.Function name os
mkFunction _ = Nothing

-- | Gets the operation structure from the state tuple.

currentOS :: State -> OS.OpStructure
currentOS (os, _, _, _) = os

-- | Updates the state with a new operation structure.

updateOS :: State -> OS.OpStructure -> State
updateOS (_, n, l, ms) os = (os, n, l, ms)

-- | Gets the graph contained by the operation structure in a given state.

currentGraph :: State -> G.Graph
currentGraph = OS.osGraph . currentOS

-- | Updates the graph contained by the operation structure in a given state.

updateGraph :: State -> G.Graph -> State
updateGraph t g = updateOS t $ OS.updateGraph (currentOS t) g

-- | Adds a new node into a given state.

addNewNode :: State -> G.NodeInfo -> State
addNewNode t ni =
  let (new_g, new_n) = G.addNewNode ni (currentGraph t)
      t1 = updateGraph t new_g
      t2 = updateLastTouchedNode t1 new_n
  in t2

-- | Gets the last touched node in a given state. If the graph contained by the
-- operation structure in the state is empty, `Nothing` is returned.

lastTouchedNode :: State -> Maybe G.Node
lastTouchedNode (_, n, _, _) = n

-- | Updates the last touched node of a given state.

updateLastTouchedNode :: State -> G.Node -> State
updateLastTouchedNode (os, _, l, ms) n = (os, Just n, l, ms)

-- | Adds a new edge into a given state.

addNewEdge :: State
              -> G.Node -- ^ Source node.
              -> G.Node -- ^ Destination node.
              -> State
addNewEdge t src dst =
  let (new_g, _) = G.addNewEdge (src, dst) (currentGraph t)
  in updateGraph t new_g

-- | Adds many new edges into a given state.

addNewEdgesManySources :: State
                          -> [G.Node] -- ^ Source nodes.
                          -> G.Node   -- ^ Destination node.
                          -> State
addNewEdgesManySources t srcs dst =
  let es = zip srcs (repeat dst)
      f g e = fst $ G.addNewEdge e g
  in updateGraph t $ foldl f (currentGraph t) es

-- | Adds many new edges into a given state.

addNewEdgesManyDests :: State
                        -> G.Node   -- ^ Source node.
                        -> [G.Node] -- ^ Destination nodes.
                        -> State
addNewEdgesManyDests t src dsts =
  let es = zip (repeat src) dsts
      f g e = fst $ G.addNewEdge e g
  in updateGraph t $ foldl f (currentGraph t) es

-- | Adds a new constraint into a given state.

addConstraint :: State -> C.Constraint -> State
addConstraint t c = updateOS t $ OS.addConstraint (currentOS t) c

-- | Gets the label node of a given state.

currentLabel :: State -> Maybe G.Node
currentLabel (_, _, l, _) = l

-- | Updates the label node of a given state.

updateLabel :: State -> G.Node -> State
updateLabel (os, n, _, ms) l = (os, n, Just l, ms)

-- | Gets the list of mappings of a given state.

currentMappings :: State -> [Mapping]
currentMappings (_, _, _, ms) = ms

-- | Adds a new mapping to a given state.

addMapping :: State -> Mapping -> State
addMapping (os, n, l, ms) m = (os, n, l, ms ++ [m])

-- | Gets the node ID (if any) to which a symbol is mapped to.

nodeIdFromSym :: [Mapping] -> Symbol -> Maybe G.NodeId
nodeIdFromSym ms sym =
  let ns = filter (\t -> snd t == sym) ms
  in if not $ null ns
        then Just $ fst $ last ns
        else Nothing

-- | Processes a symbol. If a node mapping for that symbol already exists, then
-- the last touched node is updated to reflect that node. If a mapping does not
-- exist, then a new data node is added.

processSym :: State     -- ^ The current state.
              -> Symbol -- ^ The symbol
              -> State  -- ^ The new state.
processSym t sym =
    let node_id_of_sym = nodeIdFromSym (currentMappings t) sym
    in if isJust node_id_of_sym
       then let n_of_sym = head $ G.nodeId2Node (currentGraph t)
                                                (fromJust node_id_of_sym)
            in updateLastTouchedNode t n_of_sym
       else addNewNode t (G.NodeInfo (G.DataNode D.AnyType) (show sym))

-- | Inserts a new node representing a computational operation, and adds edges
-- to that node from the given operands (which will also be processed).

processCompOp :: (Processable o)
                 => State     -- ^ The current state.
                 -> Op.CompOp -- ^ The computational operation.
                 -> [o]       -- ^ The operands.
                 -> State     -- ^ The new state.
processCompOp t op operands =
  let ts = scanl process t operands
      t1 = last ts
      t2 = addNewNode t1 (G.NodeInfo (G.ComputationNode op) (prettyShow op))
      op_node = fromJust $ lastTouchedNode t2
      operand_ns = map (fromJust . lastTouchedNode) (tail ts)
      t3 = addNewEdgesManySources t2 operand_ns op_node
  in t3

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current label node and operands (which will also be processed).

processControlOp :: (Processable o)
                    => State        -- ^ The current state.
                    -> Op.ControlOp -- ^ The control operation.
                    -> [o]          -- ^ The operands.
                    -> State        -- ^ The new state.
processControlOp t op operands =
  let ts = scanl process t operands
      t1 = last ts
      t2 = addNewNode t1 (G.NodeInfo (G.ControlNode op) (prettyShow op))
      op_node = fromJust $ lastTouchedNode t2
      t3 = addNewEdge t2 (fromJust $ currentLabel t2) op_node
      operand_ns = map (fromJust . lastTouchedNode) (tail ts)
      t4 = addNewEdgesManySources t3 operand_ns op_node
  in t4

-- | Inserts a new data node and adds an edge to it from the node which was last
-- added into the state.

insertAndConnectDataNode :: State -> State
insertAndConnectDataNode t =
  let op_node = fromJust $ lastTouchedNode t
      t1 = addNewNode t (G.NodeInfo (G.DataNode D.AnyType) "")
      d_node = fromJust $ lastTouchedNode t1
      t2 = addNewEdge t1 op_node d_node
  in t2

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

-- | Checks that a label node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated to reflect the
-- label node in question. If not then a new label node is added.

ensureLabelNodeExists :: State -> G.BBLabel -> State
ensureLabelNodeExists t l =
  let label_nodes = filter G.isLabelNode $ G.allNodes $ currentGraph t
      nodes_w_matching_labels = filter (\n -> (G.bbLabel $ G.nodeType n) == l)
                                          label_nodes
  in if length nodes_w_matching_labels > 0
     then updateLastTouchedNode t (head nodes_w_matching_labels)
     else addNewNode t (G.NodeInfo (G.LabelNode l) (show l))



------------------------
-- 'Processable' instances
------------------------

instance (Processable a) => Processable [a] where
  process = foldl process

instance (Processable n) => Processable (LLVM.Named n) where
  process t (name LLVM.:= expr) =
    let t1 = process t name
        dst_node = fromJust $ lastTouchedNode t1
        t2 = process t1 expr
        expr_node = fromJust $ lastTouchedNode t2
        t3 = addNewEdge t2 expr_node dst_node
    in t3
  process t (LLVM.Do expr) = process t expr

instance Processable LLVM.BasicBlock where
  process t (LLVM.BasicBlock (LLVM.Name str) insts term_inst) =
    let t1 = ensureLabelNodeExists t1 (G.BBLabel str)
        t2 = updateLabel t1 (fromJust $ lastTouchedNode t1)
        t3 = foldl process t2 insts
        t4 = process t3 term_inst
    in t4

instance Processable LLVM.Name where
  process t name@(LLVM.Name _) = processSym t (toSymbol name)
  process t name@(LLVM.UnName _) = processSym t (toSymbol name)

instance Processable LLVM.Instruction where
  process t (LLVM.Add  _ _ op1 op2 _) =
    processCompOp t (Op.IntOp   Op.Add) [op1, op2]
  process t (LLVM.FAdd op1 op2 _) =
    processCompOp t (Op.FloatOp Op.Add) [op1, op2]
  process t (LLVM.Sub  _ _ op1 op2 _) =
    processCompOp t (Op.IntOp   Op.Sub) [op1, op2]
  process t (LLVM.FSub op1 op2 _) =
    processCompOp t (Op.FloatOp Op.Sub) [op1, op2]
  process t (LLVM.Mul _ _ op1 op2 _) =
    processCompOp t (Op.IntOp   Op.Mul) [op1, op2]
  process t (LLVM.FMul op1 op2 _) =
    processCompOp t (Op.FloatOp Op.Mul) [op1, op2]
  process t (LLVM.UDiv _ op1 op2 _) =
    processCompOp t (Op.UIntOp  Op.Div) [op1, op2]
  process t (LLVM.SDiv _ op1 op2 _) =
    processCompOp t (Op.SIntOp  Op.Div) [op1, op2]
  process t (LLVM.FDiv op1 op2 _) =
    processCompOp t (Op.FloatOp Op.Div) [op1, op2]
  process t (LLVM.URem op1 op2 _) =
    processCompOp t (Op.UIntOp  Op.Rem) [op1, op2]
  process t (LLVM.SRem op1 op2 _) =
    processCompOp t (Op.SIntOp  Op.Rem) [op1, op2]
  process t (LLVM.FRem op1 op2 _) =
    processCompOp t (Op.FloatOp Op.Rem) [op1, op2]
  process t (LLVM.Shl _ _ op1 op2 _) =
    processCompOp t (Op.IntOp   Op.Shl) [op1, op2]
  process t (LLVM.LShr _ op1 op2 _) =
    processCompOp t (Op.IntOp  Op.LShr) [op1, op2]
  process t (LLVM.AShr _ op1 op2 _) =
    processCompOp t (Op.IntOp  Op.AShr) [op1, op2]
  process t (LLVM.And op1 op2 _) =
    processCompOp t (Op.IntOp   Op.And) [op1, op2]
  process t (LLVM.Or op1 op2 _) =
    processCompOp t (Op.IntOp    Op.Or) [op1, op2]
  process t (LLVM.Xor op1 op2 _) =
    processCompOp t (Op.IntOp   Op.XOr) [op1, op2]
  process t (LLVM.ICmp p op1 op2 _) =
    processCompOp t (fromLlvmIPred p) [op1, op2]
  process t (LLVM.FCmp p op1 op2 _) =
    processCompOp t (fromLlvmFPred p) [op1, op2]
  process t (LLVM.Phi _ operands _) =
    let processPhiElem t' (op, _) = process t' op
        ts = scanl processPhiElem t operands
        t1 = last ts
        t2 = addNewNode t1 (G.NodeInfo G.PhiNode "phi")
        op_node = fromJust $ lastTouchedNode t2
        operand_ns = map (fromJust . lastTouchedNode) (tail ts)
        -- Here we simply ignore the edge order, and then run a second pass to
        -- fix it afterwards after all edges from the branches have been added
        t3 = addNewEdgesManySources t2 operand_ns op_node
        t4 = insertAndConnectDataNode t3
    in t4
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Terminator where
  process t (LLVM.Ret op _) = processControlOp t Op.Ret (maybeToList op)
  process t (LLVM.Br (LLVM.Name dst) _) =
    let t1 = processControlOp t Op.UncondBranch
             ([] :: [LLVM.Name]) -- The type signature is needed to please GHC
        br_node = fromJust $ lastTouchedNode t1
        t2 = ensureLabelNodeExists t1 (G.BBLabel dst)
        dst_node = fromJust $ lastTouchedNode t2
        t3 = addNewEdge t2 br_node dst_node
    in t3
  process t (LLVM.CondBr op (LLVM.Name t_dst) (LLVM.Name f_dst) _) =
    let t1 = processControlOp t Op.CondBranch [op]
        br_node = fromJust $ lastTouchedNode t1
        t2 = ensureLabelNodeExists t1 (G.BBLabel t_dst)
        t_dst_node = fromJust $ lastTouchedNode t2
        t3 = ensureLabelNodeExists t2 (G.BBLabel f_dst)
        f_dst_node = fromJust $ lastTouchedNode t3
        t4 = addNewEdgesManyDests t3 br_node [t_dst_node, f_dst_node]
    in t4
  process _ l = error $ "'process' not implemented for " ++ show l

instance Processable LLVM.Operand where
  process t (LLVM.LocalReference name) = process t name
  process t (LLVM.ConstantOperand c) = process t c

instance Processable LLVMC.Constant where
  process t (LLVMC.Int b v) =
    let t1 = addNewNode t (G.NodeInfo (G.DataNode $ fromIWidth b) (show v))
        n = fromJust $ lastTouchedNode t1
        -- TODO: add constant constraint
        -- t2 = addConstraint t1 (C.EqExpr (C.AnIntegerExpr v) ())
        t2 = t1
    in t2
  process _ l = error $ "'process' not implemented for " ++ show l
