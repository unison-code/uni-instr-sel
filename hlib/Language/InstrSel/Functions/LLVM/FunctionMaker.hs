--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Functions.LLVM.FunctionMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
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

module Language.InstrSel.Functions.LLVM.FunctionMaker
  ( mkFunctionsFromLlvmModule
  , mkFunction
  )
where

import qualified Language.InstrSel.DataTypes as D
import qualified Language.InstrSel.Graphs as G
import qualified Language.InstrSel.OpStructures as OS
import qualified Language.InstrSel.OpTypes as Op
import qualified Language.InstrSel.Functions as PM
import Language.InstrSel.Utils
  ( rangeFromSingleton
  , toNatural
  )

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.FloatingPointPredicate as LLVMF
import qualified LLVM.General.AST.Global as LLVMG
import qualified LLVM.General.AST.IntegerPredicate as LLVMI

import Data.Maybe

-- TODO: remove
import Debug.Trace



--------------
-- Data types
--------------

-- | Represents a mapping from a symbol to a data node currently in the graph.
type SymToDataNodeMapping = (Symbol, G.Node)

-- | Represents a data flow that goes from a label node, identified by the given
-- ID, to an entity node. This is needed to draw the missing flow edges after
-- both the data flow graph and the control flow graph have been built.
type LabelToEntityFlow = (PM.BasicBlockLabel, G.Node)

-- | Represents a definition that goes from a label node, identified by the
-- given ID, an entity node. This is needed to draw the missing definition edges
-- after both the data flow graph and the control flow graph have been
-- built. Since the in-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the in-edge number of the data-flow edge is
-- also included in the tuple.
type LabelToEntityDef = (PM.BasicBlockLabel, G.Node, G.EdgeNr)

-- | Represents a definition that goes from an entity node to a label node,
-- identified by the given ID. This is needed to draw the missing definition
-- edges after both the data flow graph and the control flow graph have been
-- built. Since the out-edge number of an data-flow edge must match that of the
-- corresponding definition edge, the out-edge number of the data-flow edge is
-- also included in the tuple.
type EntityToLabelDef = (G.Node, PM.BasicBlockLabel, G.EdgeNr)

-- | Represents the intermediate build data.
data BuildState
  = BuildState
      { llvmModule :: LLVM.Module
        -- ^ The original LLVM module.
      , opStruct :: OS.OpStructure
        -- ^ The current operation structure.
      , lastTouchedNode :: Maybe G.Node
        -- ^ The last node (if any) that was touched. This is used to
        -- simplifying edge insertion.
      , entryLabel :: Maybe PM.BasicBlockLabel
        -- ^ The label of the function entry point. A 'Nothing' value means that
        -- this value has not yet been assigned.
      , currentLabel :: Maybe PM.BasicBlockLabel
        -- ^ The label of the basic block currently being processed. A 'Nothing'
        -- value means that no basic block has yet been processed.
      , funcBBExecFreqs :: [(PM.BasicBlockLabel, PM.ExecFreq)]
        -- ^ The execution frequencies of the respective basic blocks.
      , symMaps :: [SymToDataNodeMapping]
        -- ^ List of symbol-to-node mappings. If there are more than one mapping
        -- using the same symbol, then the last one occuring in the list should
        -- be picked.
      , labelToEntityFlows :: [LabelToEntityFlow]
        -- ^ List of label-to-entity flow dependencies that are yet to be
        -- converted into edges.
      , labelToEntityDefs :: [LabelToEntityDef]
        -- ^ List of label-to-entity definitions that are yet to be converted
        -- into edges.
      , entityToLabelDefs :: [EntityToLabelDef]
        -- ^ List of entity-to-label definitions that are yet to be converted
        -- into edges.
      , funcInputValues :: [G.Node]
        -- ^ The data nodes representing the function input arguments.
      }
  deriving (Show)

-- | Retains various symbol names.
data Symbol
  = LocalStringSymbol String
  | GlobalStringSymbol String
  | TemporarySymbol Integer
  deriving (Eq)

instance Show Symbol where
  show (LocalStringSymbol str) = "%" ++ str
  show (GlobalStringSymbol str) = "@" ++ str
  show (TemporarySymbol int) = "t" ++ show int

-- | Retains various constant values.
data Constant
  = IntConstant
      { intBitWidth :: Integer
        -- ^ Number of bits that represents the integer value.
      , signedIntValue :: Integer
        -- ^ The integer value. Note that this value is the signed-interpreted
        -- value of the value provided in the LLVM AST (see
        -- `LLVMC.signedIntegerValue`).
      }

  | FloatConstant { floatValue :: Float }
  | GlobalReferenceConstant { globalRefType :: D.DataType
                            , globalRefName :: Symbol
                            }
  deriving (Eq)

instance Show Constant where
  show IntConstant { signedIntValue = v } = show v
  show FloatConstant { floatValue = v } = show v
  show GlobalReferenceConstant { globalRefName = s } = show s


----------------
-- Type classes
----------------

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
  toConstant i@(LLVMC.Int b _) =
    IntConstant { intBitWidth = fromIntegral b
                , signedIntValue = LLVMC.signedIntegerValue i
                }
  toConstant a@(LLVMC.GlobalReference t n) =
    GlobalReferenceConstant { globalRefType = toDataType t
                            , globalRefName = toSymbol n
                            }
  toConstant l = error $ "'toConstant' not implemented for " ++ show l

-- | Class for converting an LLVM entity into a 'DataType'.
class DataTypeFormable a where
  toDataType :: a -> D.DataType

instance DataTypeFormable Constant where
  toDataType IntConstant { intBitWidth = w, signedIntValue = v } =
    D.IntConstType { D.intConstValue = rangeFromSingleton v
                   , D.intConstNumBits = Just $ toNatural w
                   }
  toDataType GlobalReferenceConstant {} =
    -- TODO: fix so that the correct data type is applied
    D.AnyType
  toDataType c = error $ "'toDataType' not implemented for " ++ show c

instance DataTypeFormable LLVM.Type where
  toDataType (LLVM.IntegerType bits) =
    D.IntTempType { D.intTempNumBits = toNatural bits }
  toDataType (LLVM.PointerType _ _) =
    -- TODO: fix so that the correct data type is applied
    D.AnyType
  toDataType t = error $ "'toDataType' not implemented for " ++ show t

instance DataTypeFormable LLVM.Operand where
  toDataType (LLVM.LocalReference t _) = toDataType t
  toDataType (LLVM.ConstantOperand c) = toDataType (toConstant c)
  toDataType o = error $ "'toDataType' not implemented for " ++ show o

-- | Class for building the data flow graph.
class DfgBuildable a where
  -- | Builds the corresponding data flow graph from a given LLVM element.
  buildDfg
    :: BuildState
      -- ^ The current build state.
    -> a
       -- ^ The LLVM element to process.
    -> BuildState
       -- ^ The new build state.

-- | Class for building the control flow graph.
class CfgBuildable a where
  -- | Builds the corresponding control flow graph from a given LLVM element.
  -- It is assumed that all data entities referred to in the resulting CFG are
  -- already available in the current build state.
  buildCfg
    :: BuildState
      -- ^ The current build state.
    -> a
       -- ^ The LLVM element to process.
    -> BuildState
       -- ^ The new build state.



-------------
-- Functions
-------------

-- | Converts an argument into a temporary-oriented data type.
toTempDataType :: (DataTypeFormable t) => t -> D.DataType
toTempDataType a =
  conv $ toDataType a
  where conv d@(D.IntTempType {}) = d
        conv (D.IntConstType { D.intConstNumBits = b }) =
          if isJust b
          then D.IntTempType { D.intTempNumBits = fromJust b }
          else error $ "toTempDataType: IntConstType has 'intTempNumBits' "
                       ++ "set to 'Nothing'"
        conv d = error $ "toTempDataType: unexpected data type " ++ show d

-- | Creates an initial state.
mkInitBuildState :: LLVM.Module -> BuildState
mkInitBuildState m =
  BuildState { llvmModule = m
             , opStruct = OS.mkEmpty
             , lastTouchedNode = Nothing
             , entryLabel = Nothing
             , currentLabel = Nothing
             , funcBBExecFreqs = []
             , symMaps = []
             , labelToEntityFlows = []
             , labelToEntityDefs = []
             , entityToLabelDefs = []
             , funcInputValues = []
             }

-- | Builds a list of functions from an LLVM module. If the module does not
-- contain any globally defined functions, an empty list is returned.
mkFunctionsFromLlvmModule :: LLVM.Module -> [PM.Function]
mkFunctionsFromLlvmModule m =
  mapMaybe (mkFunctionFromGlobalDef m) (LLVM.moduleDefinitions m)

-- | Builds a function from an LLVM AST definition. If the definition is not
-- global, `Nothing` is returned.
mkFunctionFromGlobalDef :: LLVM.Module -> LLVM.Definition -> Maybe PM.Function
mkFunctionFromGlobalDef m (LLVM.GlobalDefinition g) = mkFunction m g
mkFunctionFromGlobalDef _ _ = Nothing

-- | Builds a function from a global LLVM AST definition. If the definition is
-- not a function, `Nothing` is returned.
mkFunction :: LLVM.Module -> LLVM.Global -> Maybe PM.Function
mkFunction m f@(LLVM.Function {}) =
  let st0 = mkInitBuildState m
      st1 = buildDfg st0 f
      st2 = buildCfg st1 f
      st3 = updateOSEntryLabelNode
              st2
              (fromJust $ findLabelNodeWithID st2 (fromJust $ entryLabel st2))
      st4 = addMissingLabelToEntityFlowEdges st3
      st5 = addMissingLabelToEntityDefEdges st4
      st6 = addMissingEntityToLabelDefEdges st5
  in Just ( PM.Function
              { PM.functionName = toFunctionName $ LLVMG.name f
              , PM.functionOS = opStruct st6
              , PM.functionInputs = map G.getNodeID (funcInputValues st6)
              , PM.functionBBExecFreq = funcBBExecFreqs st6
              }
          )
mkFunction _ _ = Nothing

toFunctionName :: LLVM.Name -> Maybe String
toFunctionName (LLVM.Name str) = Just str
toFunctionName (LLVM.UnName _) = Nothing

-- | Gets the OS graph contained by the operation structure in a given state.
getOSGraph :: BuildState -> G.Graph
getOSGraph = OS.osGraph . opStruct

-- | Updates the OS graph contained by the operation structure in a given state.
updateOSGraph :: BuildState -> G.Graph -> BuildState
updateOSGraph st g =
  let os = opStruct st
  in st { opStruct = os { OS.osGraph = g } }

-- | Updates the OS entry label node contained by the operation structure in a
-- given state.
updateOSEntryLabelNode :: BuildState -> G.Node -> BuildState
updateOSEntryLabelNode st n =
  let os = opStruct st
  in st { opStruct = os { OS.osEntryLabelNode = Just (G.getNodeID n) } }

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

-- | Adds a new data node representing a particular constant to a given state.
addNewDataNodeWithConstant :: BuildState -> Constant -> BuildState
addNewDataNodeWithConstant st0 c =
  let st1 = addNewNode st0 (G.DataNode (toDataType c) Nothing)
      new_n = fromJust $ lastTouchedNode st1
      st2 = addLabelToEntityFlow st1 (fromJust $ entryLabel st1, new_n)
  in st2

-- | Adds a new edge into a given state.
addNewEdge
  :: BuildState
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
addNewEdgesManySources
  :: BuildState
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
addNewEdgesManyDests
  :: BuildState
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

-- | Adds a new symbol-to-node mapping to a given state.
addSymMap :: BuildState -> SymToDataNodeMapping -> BuildState
addSymMap st sm = st { symMaps = sm:(symMaps st) }

-- | Adds label-to-entity flow to a given state.
addLabelToEntityFlow :: BuildState -> LabelToEntityFlow -> BuildState
addLabelToEntityFlow st flow =
  st { labelToEntityFlows = flow:(labelToEntityFlows st) }

-- | Adds label-to-entity definition to a given state.
addLabelToEntityDef :: BuildState -> LabelToEntityDef -> BuildState
addLabelToEntityDef st def =
  st { labelToEntityDefs = def:(labelToEntityDefs st) }

-- | Adds entity-to-label definition to a given state.
addEntityToLabelDef :: BuildState -> EntityToLabelDef -> BuildState
addEntityToLabelDef st def =
  st { entityToLabelDefs = def:(entityToLabelDefs st) }

-- | Adds a data node representing a function argument to a given state.
addFuncInputValue :: BuildState -> G.Node -> BuildState
addFuncInputValue st n =
  st { funcInputValues = n:(funcInputValues st) }

-- | Finds the node ID (if any) of the data node to which a symbol is mapped.
findDataNodeWithSym :: BuildState -> Symbol -> Maybe G.Node
findDataNodeWithSym st sym = lookup sym (symMaps st)

-- | Gets the label node with a particular name in the graph of the given state.
-- If no such node exists, `Nothing` is returned.
findLabelNodeWithID :: BuildState -> PM.BasicBlockLabel -> Maybe G.Node
findLabelNodeWithID st l =
  let label_nodes = filter G.isLabelNode $ G.getAllNodes $ getOSGraph st
      nodes_w_matching_labels =
        filter (\n -> (G.bbLabel $ G.getNodeType n) == l) label_nodes
  in if length nodes_w_matching_labels > 0
     then Just (head nodes_w_matching_labels)
     else Nothing

-- | Checks that a data node with a particular symbol exists in the graph of the
-- given state. If it does then the last touched node is updated accordingly,
-- otherwise a new data node with the symbol is added. A corresponding mapping
-- is also added.
ensureDataNodeWithSymExists
  :: BuildState
  -> Symbol
  -> D.DataType
     -- ^ Data type to use upon creation if such a data node does not exist.
  -> BuildState
ensureDataNodeWithSymExists st0 sym dt =
  let n = findDataNodeWithSym st0 sym
  in if isJust n
     then touchNode st0 (fromJust n)
     else let st1 = addNewNode st0 (G.DataNode dt (Just $ show sym))
              new_n = fromJust $ lastTouchedNode st1
              st2 = addSymMap st1 (sym, new_n)
          in st2

-- | Checks that a label node with a particular name exists in the graph of the
-- given state. If it does then the last touched node is updated accordingly,
-- otherwise then a new label node is added.
ensureLabelNodeExists :: BuildState -> PM.BasicBlockLabel -> BuildState
ensureLabelNodeExists st l =
  let label_node = findLabelNodeWithID st l
  in if isJust label_node
     then touchNode st (fromJust label_node)
     else addNewNode st (G.LabelNode l)

-- | Inserts a new computation node representing the operation along with edges
-- to that computation node from the given operands (which will also be
-- processed). Lastly, a new data node representing the result will be added
-- along with an edge to that data node from the computation node.
buildDfgFromCompOp
  :: (DfgBuildable o)
  => BuildState
  -> D.DataType
     -- ^ The data type of the result.
  -> Op.CompOp
     -- ^ The computational operation.
  -> [o]
     -- ^ The operands.
  -> BuildState
buildDfgFromCompOp st0 dt op operands =
  let sts = scanl buildDfg st0 operands
      operand_ns = map (fromJust . lastTouchedNode) (tail sts)
      st1 = last sts
      st2 = addNewNode st1 (G.ComputationNode op)
      op_node = fromJust $ lastTouchedNode st2
      st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns op_node
      st4 = addNewNode st3 (G.DataNode dt Nothing)
      d_node = fromJust $ lastTouchedNode st4
      st5 = addNewEdge st4 G.DataFlowEdge op_node d_node
  in st5

-- | Inserts a new node representing a control operation, and adds edges to that
-- node from the current label node and operands (which will also be processed).
buildCfgFromControlOp
  :: (CfgBuildable o)
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
      st3 = addNewEdge st2
                       G.ControlFlowEdge
                       ( fromJust $
                           findLabelNodeWithID st2
                                               (fromJust $ currentLabel st2)
                       )
              op_node
      st4 = addNewEdgesManySources st3 G.DataFlowEdge operand_ns op_node
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

-- | Adds the missing data or state flow edges from label nodes to data or state
-- nodes, as described in the given build state.
addMissingLabelToEntityFlowEdges :: BuildState -> BuildState
addMissingLabelToEntityFlowEdges st =
  let g0 = getOSGraph st
      deps = map ( \(l, n) ->
                   if G.isDataNode n
                   then (l, n, G.DataFlowEdge)
                   else if G.isStateNode n
                        then (l, n, G.StateFlowEdge)
                        else error ( "addMissingLabelToEntityFlowEdges: "
                                     ++ "This should never happen"
                                   )
                 )
                 (labelToEntityFlows st)
      g1 =
        foldr ( \(l, n, et) g ->
                let pair = (fromJust $ findLabelNodeWithID st l, n)
                in fst $ G.addNewEdge et pair g
              )
              g0
              deps
  in updateOSGraph st g1

-- | Adds the missing label-to-entity definition edges, as described in the
-- given build state.
addMissingLabelToEntityDefEdges :: BuildState -> BuildState
addMissingLabelToEntityDefEdges st =
  let g0 = getOSGraph st
      defs = labelToEntityDefs st
      g1 = foldr ( \(bb_id, dn, nr) g ->
                   let ln = fromJust $ findLabelNodeWithID st bb_id
                       (g', new_e) = G.addNewDefEdge (ln, dn) g
                       new_el = (G.getEdgeLabel new_e) { G.inEdgeNr = nr }
                       g'' = G.updateEdgeLabel new_el new_e g'
                   in g''
                 )
                 g0
                 defs
  in updateOSGraph st g1

-- | Adds the missing entity-to-label definition edges, as described in the
-- given build state.
addMissingEntityToLabelDefEdges :: BuildState -> BuildState
addMissingEntityToLabelDefEdges st =
  let g0 = getOSGraph st
      defs = entityToLabelDefs st
      g1 = foldr ( \(dn, bb_id, nr) g ->
                   let ln = fromJust $ findLabelNodeWithID st bb_id
                       (g', new_e) = G.addNewDefEdge (dn, ln) g
                       new_el = (G.getEdgeLabel new_e) { G.outEdgeNr = nr }
                       g'' = G.updateEdgeLabel new_el new_e g'
                   in g''
                 )
                 g0
                 defs
  in updateOSGraph st g1

-- | Extracts the block execution frequency from the metadata (which should be
-- attached to the terminator instruction of the corresponding basic block).
extractExecFreq :: LLVM.Module -> LLVM.InstructionMetadata -> PM.ExecFreq
extractExecFreq m im =
  mkExecFreq $ head $ checkNumOps $ getOps $ head $ checkNumNodes $ findNodes im
  where soughtMetaName = "exec_freq"
        findNodes = map snd . filter (\m' -> fst m' == soughtMetaName)
        checkNumNodes ms | length ms == 0 =
                             error $
                               "No metadata entry with name '" ++
                               soughtMetaName ++ "'!"
                         | length ms > 1 =
                             error $
                               "Multiple metadata entries with name '" ++
                               soughtMetaName ++ "'!"
                         | otherwise = ms

        getOps = catMaybes . (retrieveMetadataOps m)
        checkNumOps ops | length ops == 0 = error "No operands in metadata!"
                        | length ops > 1 =
                            error "Multiple operands in metadata!"
                        | otherwise = ops
        mkExecFreq (LLVM.ConstantOperand (LLVMC.Int _ freq)) =
          PM.toExecFreq freq
        mkExecFreq _ = error "Invalid execution frequency value!"

-- | Gets the metadata of a given LLVM terminator instruction.
getTermMetadata :: LLVM.Terminator -> LLVM.InstructionMetadata
getTermMetadata t = LLVM.metadata' t

-- | Gets the LLVM instruction from a named expression.
fromNamed :: LLVM.Named i -> i
fromNamed (_ LLVM.:= i) = i
fromNamed (LLVM.Do i) = i

-- | Retrieves the list of operands attached to a metadata node. If the node is
-- a metanode ID, then the operands of that metanode ID will be retrieved.
retrieveMetadataOps :: LLVM.Module -> LLVM.MetadataNode -> [Maybe LLVM.Operand]
retrieveMetadataOps _ (LLVM.MetadataNode ops) = ops
retrieveMetadataOps m (LLVM.MetadataNodeReference mid) =
  let module_defs = LLVM.moduleDefinitions m
      isMetaDef (LLVM.MetadataNodeDefinition _ _) = True
      isMetaDef _ = False
      meta_defs = filter isMetaDef module_defs
      sought_ops = mapMaybe ( \(LLVM.MetadataNodeDefinition mid' ops) ->
                              if mid' == mid
                              then Just ops
                              else Nothing
                            )
                            meta_defs
  in if length sought_ops == 1
     then head sought_ops
     else let (LLVM.MetadataNodeID mid_value) = mid
          in error $ "No metadata with ID " ++ (show mid_value)



---------------------------------------------
-- DfgBuildable-related type class instances
---------------------------------------------

instance (DfgBuildable a) => DfgBuildable [a] where
  buildDfg = foldl buildDfg

instance (DfgBuildable n) => DfgBuildable (LLVM.Named n) where
  buildDfg st0 (name LLVM.:= expr) =
    let st1 = buildDfg st0 expr
        sym = toSymbol name
        res_n = fromJust $ lastTouchedNode st1
        res_dt = G.getDataTypeOfDataNode res_n
        st2 = ensureDataNodeWithSymExists st1 sym res_dt
        sym_n = fromJust $ lastTouchedNode st2
        st3 = updateOSGraph st2 (G.mergeNodes sym_n res_n (getOSGraph st2))
        replaceNodeInLEDef old_n new_n (l, n, nr) =
          if old_n == n then (l, new_n, nr) else (l, n, nr)
        st4 = st3 { labelToEntityDefs =
                       map (replaceNodeInLEDef res_n sym_n)
                           (labelToEntityDefs st3)
                  }
        replaceNodeInELDef old_n new_n (n, l, nr) =
          if old_n == n then (new_n, l, nr) else (n, l, nr)
        st5 = st4 { entityToLabelDefs =
                       map (replaceNodeInELDef res_n sym_n)
                           (entityToLabelDefs st4)
                  }
    in st5
  buildDfg st (LLVM.Do expr) = buildDfg st expr

instance DfgBuildable LLVM.Global where
  buildDfg st0 f@(LLVM.Function {}) =
    let (params, _) = LLVMG.parameters f
        st1 = buildDfg st0 params
        st2 = buildDfg st1 $ LLVMG.basicBlocks f
    in st2
  buildDfg _ l = error $ "'buildDfg' not implemented for " ++ show l

instance DfgBuildable LLVM.BasicBlock where
  buildDfg st0 (LLVM.BasicBlock (LLVM.Name str) insts _) =
    let bb_label = PM.BasicBlockLabel str
        st1 = if isNothing $ entryLabel st0
              then foldl (\st n -> addLabelToEntityFlow st (bb_label, n))
                         (st0 { entryLabel = Just bb_label })
                         (funcInputValues st0)
              else st0
        st2 = st1 { currentLabel = Just bb_label }
        st3 = foldl buildDfg st2 insts
    in st3
  buildDfg _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "'buildDfg' does not support unnamed basic blocks"

instance DfgBuildable LLVM.Instruction where
  buildDfg st (LLVM.Add  nsw nuw op1 op2 _) =
    -- TODO: make use of nsw and nuw?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.Add)
                       [op1, op2]
  buildDfg st (LLVM.FAdd _ op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.FloatOp Op.Add)
                       [op1, op2]
  buildDfg st (LLVM.Sub  nsw nuw op1 op2 _) =
    -- TODO: make use of nsw and nuw?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.Sub)
                       [op1, op2]
  buildDfg st (LLVM.FSub _ op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.FloatOp Op.Sub)
                       [op1, op2]
  buildDfg st (LLVM.Mul nsw nuw op1 op2 _) =
    -- TODO: make use of nsw and nuw?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.Mul)
                       [op1, op2]
  buildDfg st (LLVM.FMul _ op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.FloatOp Op.Mul)
                       [op1, op2]
  buildDfg st (LLVM.UDiv exact op1 op2 _) =
    -- TODO: make use of exact?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.UIntOp Op.Div)
                       [op1, op2]
  buildDfg st (LLVM.SDiv exact op1 op2 _) =
    -- TODO: make use of exact?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.SIntOp Op.Div)
                       [op1, op2]
  buildDfg st (LLVM.FDiv _ op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.FloatOp Op.Div)
                       [op1, op2]
  buildDfg st (LLVM.URem op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.UIntOp Op.Rem)
                       [op1, op2]
  buildDfg st (LLVM.SRem op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.SIntOp Op.Rem)
                       [op1, op2]
  buildDfg st (LLVM.FRem _ op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.FloatOp Op.Rem)
                       [op1, op2]
  buildDfg st (LLVM.Shl nsw nuw op1 op2 _) =
    -- TODO: make use of nsw and nuw?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.Shl)
                       [op1, op2]
  buildDfg st (LLVM.LShr exact op1 op2 _) =
    -- TODO: make use of exact?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.LShr)
                       [op1, op2]
  buildDfg st (LLVM.AShr exact op1 op2 _) =
    -- TODO: make use of exact?
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.AShr)
                       [op1, op2]
  buildDfg st (LLVM.And op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.And)
                       [op1, op2]
  buildDfg st (LLVM.Or op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.Or)
                       [op1, op2]
  buildDfg st (LLVM.Xor op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (Op.CompArithOp $ Op.IntOp Op.XOr)
                       [op1, op2]
  buildDfg st (LLVM.ICmp p op1 op2 _) =
    -- TODO: add support for vectorized icmp
    buildDfgFromCompOp st
                       (D.IntTempType { D.intTempNumBits = 1 })
                       (fromLlvmIPred p)
                       [op1, op2]
  buildDfg st (LLVM.FCmp p op1 op2 _) =
    buildDfgFromCompOp st
                       (toTempDataType op1)
                       (fromLlvmFPred p)
                       [op1, op2]
  buildDfg st (LLVM.Trunc op1 t1 _) =
    buildDfgFromCompOp st
                       (toDataType t1)
                       (Op.CompTypeConvOp Op.Trunc)
                       [op1]
  buildDfg st (LLVM.ZExt op1 t1 _) =
    buildDfgFromCompOp st
                       (toDataType t1)
                       (Op.CompTypeConvOp Op.ZExt)
                       [op1]
  buildDfg st (LLVM.SExt op1 t1 _) =
    buildDfgFromCompOp st
                       (toDataType t1)
                       (Op.CompTypeConvOp Op.SExt)
                       [op1]
  -- TODO: replace the 'addEntityToLabelDef' with proper dependencies from/to
  -- state nodes.
  buildDfg st0 (LLVM.Load _ op1 _ _ _) =
    let st1 = buildDfgFromCompOp st0
                        (toDataType op1)
                        (Op.CompMemoryOp Op.Load)
                        [op1]
        n   = fromJust $ lastTouchedNode st1
        bb  = fromJust $ currentLabel st1
        st2 = addEntityToLabelDef st1 (n, bb, 0)
    in st2
  -- TODO: replace the 'addEntityToLabelDef' with proper dependencies from/to
  -- state nodes.
  buildDfg st0 (LLVM.Store _ op1 op2 _ _ _) =
    let st1 = buildDfgFromCompOp st0
                        D.AnyType -- This doesn't matter since the result node
                                  -- will be removed directly afterwards
                        (Op.CompMemoryOp Op.Store)
                        [op1, op2] -- TODO: check the order that it's correct
        -- TODO: remove the node below
        n   = fromJust $ lastTouchedNode st1
    in st1
  buildDfg st0 (LLVM.Phi t phi_operands _) =
    let (operands, label_names) = unzip phi_operands
        bb_labels = map (\(LLVM.Name str) -> PM.BasicBlockLabel str) label_names
        operand_node_sts = scanl buildDfg st0 operands
        operand_ns = map (fromJust . lastTouchedNode) (tail operand_node_sts)
        st1 = last operand_node_sts
        st2 = addNewNode st1 G.PhiNode
        phi_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdgesManySources st2 G.DataFlowEdge operand_ns phi_node
        st4 = foldl ( \st (n, bb_id) ->
                      let g = getOSGraph st
                          dfe = head
                                $ filter G.isDataFlowEdge
                                $ G.getEdges g n phi_node
                      in addEntityToLabelDef st (n, bb_id, G.getOutEdgeNr dfe)
                    )
                    st3
                    (zip operand_ns bb_labels)
        st5 = addNewNode st4 (G.DataNode (toDataType t) Nothing)
        d_node = fromJust $ lastTouchedNode st5
        st6 = addNewEdge st5 G.DataFlowEdge phi_node d_node
        st7 = addLabelToEntityDef st6 (fromJust $ currentLabel st6, d_node, 0)
              -- ^ Since we've just created the data node and only added a
              -- single data-flow edge to it, we are guaranteed that the in-edge
              -- number of that data-flow edge is 0.
    in st7
  buildDfg _ l = error $ "'buildDfg' not implemented for " ++ show l

instance DfgBuildable LLVM.Operand where
  buildDfg st (LLVM.LocalReference t name) =
    ensureDataNodeWithSymExists st (toSymbol name) (toDataType t)
  buildDfg st (LLVM.ConstantOperand c) =
    addNewDataNodeWithConstant st (toConstant c)
  buildDfg _ o = error $ "'buildDfg' not implemented for " ++ show o

instance DfgBuildable LLVM.Parameter where
  buildDfg st0 (LLVM.Parameter t name _) =
    let st1 = ensureDataNodeWithSymExists st0 (toSymbol name) (toDataType t)
        n = fromJust $ lastTouchedNode st1
        st2 = addFuncInputValue st1 n
    in st2



---------------------------------------------
-- CfgBuildable-related type class instances
---------------------------------------------

instance (CfgBuildable a) => CfgBuildable [a] where
  buildCfg = foldl buildCfg

instance CfgBuildable LLVM.BasicBlock where
  buildCfg st0 (LLVM.BasicBlock (LLVM.Name str) _ named_term_inst) =
    let bb_label = PM.BasicBlockLabel str
        term_inst = fromNamed named_term_inst
        bb_exec_freq = ( bb_label
                       , extractExecFreq (llvmModule st0)
                                         (getTermMetadata term_inst)
                       )
        st1 = if isNothing $ entryLabel st0
              then st1 { entryLabel = Just bb_label }
              else st0
        st2 = ensureLabelNodeExists st1 bb_label
        st3 = st2 { currentLabel = Just bb_label }
        st4 = st3 { funcBBExecFreqs = bb_exec_freq:(funcBBExecFreqs st3) }
        st5 = buildCfg st4 term_inst
    in st5
  buildCfg _ (LLVM.BasicBlock (LLVM.UnName _) _ _) =
    error $ "'buildCfg' does not support unnamed basic blocks"

instance CfgBuildable LLVM.Global where
  buildCfg st f@(LLVM.Function {}) =
    buildCfg st $ LLVMG.basicBlocks f
  buildCfg _ l = error $ "'buildCfg' not implemented for " ++ show l

instance CfgBuildable LLVM.Terminator where
  buildCfg st (LLVM.Ret op _) =
    buildCfgFromControlOp st Op.Ret (maybeToList op)
  buildCfg st0 (LLVM.Br (LLVM.Name dst) _) =
    let st1 = buildCfgFromControlOp st0
                                    Op.Br
                                    ([] :: [LLVM.Operand])
                                    -- ^ Signature needed to please GHC...
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (PM.BasicBlockLabel dst)
        dst_node = fromJust $ lastTouchedNode st2
        st3 = addNewEdge st2 G.ControlFlowEdge br_node dst_node
    in st3
  buildCfg st0 (LLVM.CondBr op (LLVM.Name t_dst) (LLVM.Name f_dst) _) =
    let st1 = buildCfgFromControlOp st0 Op.CondBr [op]
        br_node = fromJust $ lastTouchedNode st1
        st2 = ensureLabelNodeExists st1 (PM.BasicBlockLabel t_dst)
        t_dst_node = fromJust $ lastTouchedNode st2
        st3 = ensureLabelNodeExists st2 (PM.BasicBlockLabel f_dst)
        f_dst_node = fromJust $ lastTouchedNode st3
        st4 = addNewEdgesManyDests st3
                                   G.ControlFlowEdge
                                   br_node
                                   [t_dst_node, f_dst_node]
    in st4
  buildCfg _ l = error $ "'buildCfg' not implemented for " ++ show l

instance CfgBuildable LLVM.Operand where
  buildCfg st (LLVM.LocalReference t name) =
    ensureDataNodeWithSymExists st (toSymbol name) (toDataType t)
  buildCfg st (LLVM.ConstantOperand c) =
    addNewDataNodeWithConstant st (toConstant c)
  buildCfg _ o = error $ "'buildCfg' not implemented for " ++ show o
