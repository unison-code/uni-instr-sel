--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM.PMMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Converts and LLVM IR module into the internal program format.
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.LLVM.PMMaker (
  mkProgramModule
) where

import qualified LLVM.General.AST as LLVM
import qualified Language.InstructionSelection.Graphs as G
import qualified Language.InstructionSelection.OperationStructures as OS
import qualified Language.InstructionSelection.OpTypes as Op
import qualified Language.InstructionSelection.ProgramModules.Base as PM
import Language.InstructionSelection.Utils
import Data.Maybe



mkProgramModule :: LLVM.Module -> PM.Module
mkProgramModule m =
  let funcs = concatMap mkFunction $ LLVM.moduleDefinitions m
  in PM.Module funcs

mkFunction :: LLVM.Definition -> [PM.Function]
mkFunction (LLVM.GlobalDefinition g) = fromGlobalDef g
mkFunction _ = []

fromGlobalDef :: LLVM.Global -> [PM.Function]
fromGlobalDef (LLVM.Function _ _ _ _ _ (LLVM.Name name) _ _ _ _ _ bbs) =
  let (os, _, _) = (extend (OS.empty, G.BBLabel "", []) bbs)
  in [PM.Function name os]
fromGlobalDef _ = []

data Symbol
    = StringSymbol String
    | TemporarySymbol Integer
    deriving (Show, Eq)

class SymbolFormable a where
  toSymbol :: a -> Symbol
instance SymbolFormable LLVM.Name where
  toSymbol (LLVM.Name str) = StringSymbol str
  toSymbol (LLVM.UnName int) = TemporarySymbol $ toInteger int

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

instance (LlvmToOS a) => LlvmToOS [a] where
  extend = foldl extend

instance (LlvmToOS n) => LlvmToOS (LLVM.Named n) where
  extend t (name LLVM.:= expr) =
    let t' = extend t name
        dst_node = fromJust $ lastAddedNode t'
        t'' = extend t' expr
        expr_node = fromJust $ lastAddedNode t''
        t''' = addNewEdge t'' expr_node dst_node
    in t'''
  extend t (LLVM.Do expr) = extend t expr

instance LlvmToOS LLVM.BasicBlock where
  extend t (LLVM.BasicBlock (LLVM.Name l) insts term_inst) =
    let t' = updateLabel t (G.BBLabel l)
        t'' = foldl extend t' insts
        t''' = extend t'' term_inst
    in t'''

instance LlvmToOS LLVM.Name where
  extend t name@(LLVM.Name str) =
    let sym = toSymbol name
        existing_nid = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id existing_nid
        t' = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t)
                                                  ("%" ++ str)))
        t'' = maybe (addMapping t' (nid, sym)) (\_ -> t') existing_nid
    in t''

  extend t name@(LLVM.UnName int) =
    let sym = toSymbol name
        existing_nid = nodeIdFromSym (currentMappings t) sym
        nid = maybe (newNodeId t) id existing_nid
        t' = addNewNodeWithNL t (G.NodeLabel nid (G.NodeInfo G.NTData
                                                  (currentLabel t)
                                                  ("t" ++ show int)))
        t'' = maybe (addMapping t' (nid, sym)) (\_ -> t') existing_nid
    in t''

instance LlvmToOS LLVM.Instruction where
  extend t (LLVM.Add nsw nuw op1 op2 _) = undefined
  extend t (LLVM.FAdd op1 op2 _) = undefined
  extend t (LLVM.Sub nsw nuw op1 op2 _) = undefined
  extend t (LLVM.FSub op1 op2 _) = undefined
  extend t (LLVM.Mul nsw nuw op1 op2 _) = undefined
  extend t (LLVM.FMul op1 op2 _) = undefined
  extend t (LLVM.UDiv exact op1 op2 _) = undefined
  extend t (LLVM.SDiv exact op1 op2 _) = undefined
  extend t (LLVM.FDiv op1 op2 _) = undefined
  extend t (LLVM.URem op1 op2 _) = undefined
  extend t (LLVM.SRem op1 op2 _) = undefined
  extend t (LLVM.FRem op1 op2 _) = undefined
  extend t (LLVM.Shl nsw nuw op1 op2 _) = undefined
  extend t (LLVM.LShr exact op1 op2 _) = undefined
  extend t (LLVM.AShr exact op1 op2 _) = undefined
  extend t (LLVM.And op1 op2 _) = undefined
  extend t (LLVM.Or op1 op2 _) = undefined
  extend t (LLVM.Xor op1 op2 _) = undefined
  extend t (LLVM.Alloca typ numE align _) = undefined
  extend t (LLVM.Load vol addr atom align _) = undefined
  extend t (LLVM.Store vol addr val atom align _) = undefined
  extend t (LLVM.GetElementPtr inb addr is _) = undefined
  extend t (LLVM.Fence atom _) = undefined
  extend t (LLVM.CmpXchg vol addr expect repl atom _) = undefined
  extend t (LLVM.AtomicRMW vol rmwOp addr val atom _) = undefined
  extend t (LLVM.Trunc op typ _) = undefined
  extend t (LLVM.ZExt op typ _) = undefined
  extend t (LLVM.SExt op typ _) = undefined
  extend t (LLVM.FPToUI op typ _) = undefined
  extend t (LLVM.FPToSI op typ _) = undefined
  extend t (LLVM.UIToFP op typ _) = undefined
  extend t (LLVM.SIToFP op typ _) = undefined
  extend t (LLVM.FPTrunc op typ _) = undefined
  extend t (LLVM.FPExt op typ _) = undefined
  extend t (LLVM.FPExt op typ _) = undefined
  extend t (LLVM.PtrToInt op typ _) = undefined
  extend t (LLVM.IntToPtr op typ _) = undefined
  extend t (LLVM.BitCast op typ _) = undefined
  extend t (LLVM.ICmp iPred op1 op2 _) = undefined
  extend t (LLVM.FCmp fPred op1 op2 _) = undefined
  extend t (LLVM.Phi typ ins _) = undefined
  extend t (LLVM.Call isTail cc retAttr funOp args funAttrs _) = undefined
  extend t (LLVM.Select cond trueV falseV _) = undefined
  extend t (LLVM.VAArg arg typ _) = undefined
  extend t (LLVM.ExtractElement vect i _) = undefined
  extend t (LLVM.InsertElement vect elem i _) = undefined
  extend t (LLVM.ShuffleVector op1 op2 mask _) = undefined
  extend t (LLVM.ExtractValue aggr is _) = undefined
  extend t (LLVM.InsertValue aggr elem is _) = undefined
  extend t (LLVM.LandingPad typ perFun clean clauses _) = undefined
  -- TODO: implement these functions

instance LlvmToOS LLVM.Terminator where
  extend t (LLVM.Ret op _) = undefined
  extend t (LLVM.Br dst _) = undefined
  extend t (LLVM.CondBr op trueDst falseDst _) = undefined
  extend t (LLVM.IndirectBr op dsts _) = undefined
  extend t (LLVM.Switch op defDst dsts _) = undefined
  extend t (LLVM.IndirectBr op dsts _) = undefined
  extend t (LLVM.Invoke cc retAttr funOp args funAttr retDst expDst _) =
    undefined
  extend t (LLVM.Resume op _) = undefined
  extend t (LLVM.Unreachable _) = undefined
  -- TODO: implement these functions
