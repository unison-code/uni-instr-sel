{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Functions.Transformations
  ( branchExtend
  , copyExtend
  , combineConstants
  , alternativeExtend
  )
where

import Language.InstrSel.Functions.Base
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.OpTypes
import Language.InstrSel.Utils
  ( groupBy )
import Language.InstrSel.Utils.Range

import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  )
import Data.List
  ( partition )



-------------
-- Functions
-------------

-- | Copy-extends the given graph along every eligable data-flow edge.
copyExtend :: Function -> Function
copyExtend f = updateGraph (copyExtendGraph $ getGraph f) f

-- | Inserts a copy node along every data-flow edge that involves a use of a
-- value node. This also updates the definition edges to retain the same
-- semantics of the original graph. This means that if there is a definition
-- edge $e$ that involves a value node used by a phi node, then upon copy
-- extension $e$ will be moved to the new value node. Otherwise $e$ will remain
-- on the original value node. Note that definition edges where the target is a
-- value node are not affected.
copyExtendGraph :: Graph -> Graph
copyExtendGraph g =
  let nodes = filter isValueNode (getAllNodes g)
  in foldl insertCopies g nodes

-- | Inserts a new copy and value node along each outgoing data-flow edge from
-- the given value node.
insertCopies :: Graph -> Node -> Graph
insertCopies g0 n =
  let old_edges = getDtFlowOutEdges g0 n
      g1 = foldl insertCopyAlongEdge g0 old_edges
  in g1

-- | Inserts a new copy and value node along a given data-flow edge. If the
-- value node is used by a phi node, and there is a definition edge on that
-- value node, then the definition edge with matching out-edge number will be
-- moved to the new data node. Note that definition edges where the target is a
-- value node are not affected.
insertCopyAlongEdge :: Graph -> Edge -> Graph
insertCopyAlongEdge g0 df_edge =
  let mkNewDataType d@(IntTempType {}) = d
      mkNewDataType (IntConstType { intConstNumBits = Just b }) =
        IntTempType { intTempNumBits = b }
      mkNewDataType d@(PointerTempType {}) = d
      mkNewDataType d@(PointerNullType {}) = d
      mkNewDataType d = error $ "insertCopy: DataType '" ++ show d ++ "' " ++
                                "not " ++ "supported"
                        -- This happens if copy extension is attempted before
                        -- constants have been combined. In many cases, the
                        -- bit width for a constant is known somewhere in a
                        -- program but not everywhere. By combining all equal
                        -- constants afterwards, this information is propagated
                        -- such that no constant has unknown bit width.
      old_d_node = getSourceNode g0 df_edge
      old_d_origin = originOfValue $ getNodeType old_d_node
      old_op_n = getTargetNode g0 df_edge
      def_edge = if isPhiNode old_op_n
                 then let d_node_edges = getOutEdges g0 old_d_node
                          def_edges = filter isDefEdge d_node_edges
                      in Just $
                         head $
                         filter (\n -> getOutEdgeNr n == getOutEdgeNr df_edge)
                                def_edges
                 else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      new_dt = mkNewDataType $ getDataTypeOfValueNode old_d_node
      new_origin = Just $
                   let origins = map (fromJust . getOriginOfValueNode) $
                                 filter isValueNodeWithOrigin $
                                 getAllNodes g1
                       prefix = if isJust old_d_origin
                                then (fromJust old_d_origin) ++ ".copy."
                                else "%copy."
                   in head $ dropWhile (`elem` origins)
                                       ( map (\i -> prefix ++ show i)
                                             ([1..] :: [Integer])
                                             -- Cast is needed or GHC will
                                             -- complain
                                       )
      (g2, new_d_node) =
        insertNewNodeAlongEdge (ValueNode new_dt new_origin)
                               (head $ getOutEdges g1 new_cp_node)
                               g1
      g3 = if isJust def_edge
           then let e = fromJust def_edge
                in fst $ addNewDefEdge (new_d_node, getTargetNode g2 e)
                                       (delEdge e g2)
           else g2
  in g3

-- | Inserts a new block node and jump control node along each outbound control
-- edge from every conditional jump control node. This is used to handle special
-- cases where a conditional jump control node cannot be covered by patterns
-- that performs a fall-through to one of the destination blocks. These
-- situations often occur in loops where it is not possible to do a fall-through
-- out of the loop (and, naturally, it is never possible to do a fall-through to
-- the head of the loop).
--
-- After branch extension, there will be blocks which has no execution
-- frequency. These will be set to have the same frequency as its preceding
-- block in the CFG (at this point we know for sure that each new block has only
-- one preceding block).
branchExtend :: Function -> Function
branchExtend f =
  assignMissingBlockExecFreqs $
  discoverBEBlocks $
  assignMissingBlockNames $
  updateGraph (branchExtendGraph $ getGraph f) f

-- | Inserts a new block node and jump control node along each outbound control
-- edge from every conditional jump control node and passes the predicate
-- function. The new block nodes will all have empty block name.
--
-- TODO: update the def-placement edges when phi nodes are involved!
branchExtendGraph :: Graph -> Graph
branchExtendGraph g =
  let c_nodes = filter isControlNode (getAllNodes g)
      nodes = filter (\n -> (ctrlOp $ getNodeType n) == CondBr) c_nodes
      edges = concatMap (getCtrlFlowOutEdges g) nodes
  in foldl insertBranch g edges

-- | Inserts a new block node and jump control node along the given control-flow
-- edge.
insertBranch :: Graph -> Edge -> Graph
insertBranch g0 e =
  let (g1, l) = insertNewNodeAlongEdge (BlockNode mkEmptyBlockName) e g0
      new_e = head $ getCtrlFlowOutEdges g1 l
      (g2, _) = insertNewNodeAlongEdge (ControlNode Br) new_e g1
  in g2

-- | Assigns a unique block name to every block node that currently has an empty
-- block name (which will be the case after branch extension).
assignMissingBlockNames :: Function -> Function
assignMissingBlockNames f =
  let g = getGraph f
      nodes = filter isBlockNode (getAllNodes g)
      block_node_pairs = map (\n -> (getNameOfBlockNode n, n)) nodes
      no_block_nodes =
        map snd (filter (isBlockNameEmpty . fst) block_node_pairs)
      existing_names = map fst block_node_pairs
      ok_names = filter (`notElem` existing_names)
                         ( map (\i -> BlockName $ "bb" ++ show i)
                               ([0..] :: [Integer])
                                -- The type cast is to inhibit a compilation
                                -- warning
                         )
      new_g =
        foldl ( \g' (l, n) ->
                updateNodeType (BlockNode { nameOfBlock = l }) n g'
              )
              g $
        zip ok_names no_block_nodes
  in updateGraph new_g f

-- | Updates the 'functionBEBlocks' field in the 'Function' record. These blocks
-- are those which have yet to be assigned an execution frequency.
discoverBEBlocks :: Function -> Function
discoverBEBlocks f =
  let g = getGraph f
      cfg = extractCFG g
      be_blocks =
        map getNameOfBlockNode $
        filter ( \n -> let l = getNameOfBlockNode n
                           freqs = functionBBExecFreq f
                           l_freq = lookup l freqs
                       in isNothing l_freq
               ) $
        getAllNodes cfg
  in f { functionBEBlocks = be_blocks }

-- | Assigns an execution frequency to every block that currently does not have
-- one (which will be the case after branch extension). These blocks will be set
-- to have the same frequency as its preceding block in the CFG (we know at this
-- point that each new block has exactly one preceding block).
assignMissingBlockExecFreqs :: Function -> Function
assignMissingBlockExecFreqs f =
  let g = getGraph f
      cfg = extractCFG g
      new_freqs =
        map ( \n -> let l = getNameOfBlockNode n
                        freqs = functionBBExecFreq f
                        l_freq = lookup l freqs
                    in if isJust l_freq
                       then (l, fromJust l_freq)
                       else let prec_n = getSourceNode cfg $
                                         head $
                                         getCtrlFlowInEdges cfg n
                                prec_l = getNameOfBlockNode prec_n
                                prec_freq = fromJust $ lookup prec_l freqs
                            in (l, prec_freq)
            ) $
        getAllNodes cfg
  in (updateGraph g f) { functionBBExecFreq = new_freqs }

-- | Combines value nodes in the function graph that represent the same
-- constant.
combineConstants :: Function -> Function
combineConstants f =
  let g = getGraph f
      const_ns = filter isValueNodeWithConstValue (getAllNodes g)
      isSameConstant (IntConstType { intConstValue = r1 })
                     (IntConstType { intConstValue = r2 }) =
        isRangeSingleton r1 && isRangeSingleton r2 && r1 == r2
      isSameConstant _ _ = False
      haveSameConstants n1 n2 =
        (getDataTypeOfValueNode n1) `isSameConstant` (getDataTypeOfValueNode n2)
      partitionNodes [] = []
      partitionNodes [n] = [[n]]
      partitionNodes (n:ns) =
        let (eq_n, neq_n) = partition (haveSameConstants n) ns
        in [n:eq_n] ++ partitionNodes neq_n
      partitioned_ns = partitionNodes const_ns
  in foldl combineValueNodes f partitioned_ns

combineValueNodes :: Function -> [Node] -> Function
combineValueNodes f nodes =
  cc nodes
  where
  cc [] = f
  cc [n] =
    let dt = mkNewDataType $ getDataTypeOfValueNode n
             -- Note that the bitwidth is not copied
        g0 = getGraph f
        g1 = updateDataTypeOfValueNode dt n g0
    in updateGraph g1 f
  cc ns =
    let g0 = getGraph f
        nt = ValueNode (mkNewDataType $ getDataTypeOfValueNode $ head ns)
                       (getOriginOfValueNode $ head ns)
             -- Note that the bitwidth is not copied
        (g1, new_n) = addNewNode nt g0
        entry_node = head $
                     findNodesWithNodeID g1 $
                     fromJust $
                     osEntryBlockNode $
                     functionOS f
        (g2, _) = addNewEdge DataFlowEdge (entry_node, new_n) g1
    in foldr (replaceValueNode new_n) (updateGraph g2 f) ns
  mkNewDataType IntConstType { intConstValue = r } =
    IntConstType { intConstValue = r, intConstNumBits = Nothing }
  mkNewDataType d = error $ "combineValueNodes: unsupported data type " ++
                            show d

-- | Replaces a value node in the function graph with another value node, but
-- all inbound edges to the value node to be removed will be ignored and thus
-- disappear.
replaceValueNode
  :: Node
     -- ^ The new node.
  -> Node
     -- ^ The old node.
  -> Function
  -> Function
replaceValueNode new_n old_n f =
  let old_os = functionOS f
      old_g = osGraph old_os
      new_g = delNode old_n $ redirectOutEdges new_n old_n old_g
      def_recon = mkDefaultReconstructor
      mkNodeExpr _ expr@(ANodeIDExpr n) =
        if n == getNodeID old_n
        then ANodeIDExpr $ getNodeID new_n
        else expr
      mkNodeExpr r expr = (mkNodeExprF def_recon) r expr
      recon = mkDefaultReconstructor { mkNodeExprF = mkNodeExpr }
      new_cs = map (apply recon) (osConstraints old_os)
      new_os = old_os { osGraph = new_g, osConstraints = new_cs }
  in f { functionOS = new_os }

-- | For each value node that is copied multiple times, an additional data-flow
-- edge will be inserted for each operation that uses one of the copied values.
-- If the operation is a phi node, alternative definition edges will also be
-- inserted.
alternativeExtend :: Function -> Function
alternativeExtend f = updateGraph (alternativeExtendGraph $ getGraph f) f

alternativeExtendGraph :: Graph -> Graph
alternativeExtendGraph g =
  let v_ns = filter isValueNode $ getAllNodes g
      copy_related_vs =
        concat $
        map ( \n ->
              let es = getDtFlowOutEdges g n
                  copies = filter isCopyNode $ map (getTargetNode g) es
                  -- A copy node always have exactly one outgoing data flow
                  -- edge
                  cp_vs = map ( getTargetNode g .
                                head .
                                getDtFlowOutEdges g
                              ) $
                          copies
                  grouped_vs = filter ((> 1) . length) $
                               groupBy ( \v1 v2 ->
                                         getDataTypeOfValueNode v1
                                         ==
                                         getDataTypeOfValueNode v2
                                       ) $
                               cp_vs
              in grouped_vs
            ) $
        v_ns
  in foldr insertAlternativeEdges g copy_related_vs

insertAlternativeEdges :: [Node] -> Graph -> Graph
insertAlternativeEdges vs g0 =
  -- A this point, every copy-related value node has exactly one outgoing data
  -- flow edge as it is used by exactly one operation
  let processInput i g1 =
        let ops = map (getTargetNode g0) $
                  getDtFlowOutEdges g0 i
                  -- It's very important to get the edges from g0 and not g1, as
                  -- new data-flow edges will continuously be added to g1
            processOp o g2 =
              let alts = filter (/= i) vs
                  orig_e = head $ getEdgesBetween g0 i o
                           -- Here it is also important to get the edge from g0,
                           -- where there should be exactly one edge between
                           -- nodes i and o
                  orig_in_nr = inEdgeNr $ getEdgeLabel $ orig_e
                  insertEdge v g3 =
                    let (g4, new_e) = addNewDtFlowEdge (v, o) g3
                        label = getEdgeLabel new_e
                        new_label = label { inEdgeNr = orig_in_nr }
                        g5 = updateEdgeLabel new_label new_e g4
                    in if isPhiNode o
                       then let orig_out_nr = outEdgeNr $ getEdgeLabel $ orig_e
                                orig_d_e = head $
                                           filter ( (==) orig_out_nr .
                                                    getOutEdgeNr
                                                  ) $
                                           filter isDefEdge $
                                           getOutEdges g0 i
                                           -- Remember to get edges from g0
                                b_node = getTargetNode g0 orig_d_e
                                (g6, new_d_e) = addNewDefEdge (v, b_node) g5
                                d_label = getEdgeLabel new_d_e
                                new_out_nr = outEdgeNr $ getEdgeLabel $ new_e
                                new_d_label =
                                  d_label { outEdgeNr = new_out_nr }
                                g7 = updateEdgeLabel new_d_label new_d_e g6
                            in g7
                       else g5
              in foldr insertEdge g2 alts
        in foldr processOp g1 ops
  in foldr processInput g0 vs

-- | Applies a transformation on the graph in a given function.
getGraph :: Function -> Graph
getGraph = osGraph . functionOS

-- | Replaces the current graph in a function with a new graph.
updateGraph :: Graph -> Function -> Function
updateGraph new_g f =
  let os = functionOS f
      new_os = os { osGraph = new_g }
      new_f = f { functionOS = new_os }
  in new_f
