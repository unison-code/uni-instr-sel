{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Functions.Transformations
  ( copyExtend
  , combineConstants
  , enforcePhiNodeInvariants
  , lowerPointers
  , removeDeadCode
  , removePhiNodeRedundancies
  , removeRedundantConversions
  )
where

import Language.InstrSel.Functions.Base
import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import qualified Language.InstrSel.OpStructures.Transformations as OS
  ( enforcePhiNodeInvariants
  , lowerPointers
  , removeDeadCode
  , removePhiNodeRedundancies
  , removeRedundantConversions
  )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( TargetMachine (..) )
import Language.InstrSel.Utils
  ( groupBy )

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Copy-extends the given graph along every eligable data-flow edge.
copyExtend :: Function -> Function
copyExtend f = updateGraph (copyExtendGraph $ getGraph f) f

-- | Inserts a copy node along every data-flow edge that involves a use of a
-- value node (except for edges where the target is a copy node or the source
-- already has already been copy extended). This also updates the definition
-- edges to retain the same semantics of the original graph. This means that if
-- there is a definition edge $e$ that involves a value node used by a phi node,
-- then upon copy extension $e$ will be moved to the new value node. Otherwise
-- $e$ will remain on the original value node. Note that definition edges where
-- the target is a value node are not affected.
copyExtendGraph :: Graph -> Graph
copyExtendGraph g =
  let nodes = filter ( \n ->
                       let es = getDtFlowInEdges g n
                       in if length es > 0
                          then not $ isCopyNode $ getSourceNode g $ head es
                          else True
                     ) $
              filter isValueNode (getAllNodes g)
      edges = filter (not . isCopyNode . getTargetNode g) $
              concatMap (getDtFlowOutEdges g) $
              nodes
  in foldr insertCopyAlongEdge g edges

-- | Inserts a new copy and value node along a given data-flow edge. If the
-- value node is used by a phi node, and there is a definition edge on that
-- value node, then the definition edge with matching out-edge number will be
-- moved to the new data node. Note that definition edges where the target is a
-- value node are not affected.
insertCopyAlongEdge :: Edge -> Graph -> Graph
insertCopyAlongEdge df_edge g0 =
  let mkNewDataType d@(IntTempType {}) = d
      mkNewDataType (IntConstType { intConstNumBits = Just b }) =
        IntTempType { intTempNumBits = b }
      mkNewDataType d@(PointerTempType {}) = d
      mkNewDataType d@(PointerNullType {}) = d
      mkNewDataType d = error $ "insertCopyAlongEdge: " ++ show d ++ " not " ++
                                "supported"
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
                 then let es = filter ( \n -> getEdgeOutNr n
                                              ==
                                              getEdgeOutNr df_edge
                                      ) $
                               filter isDefEdge $
                               getOutEdges g0 old_d_node
                      in if length es == 1
                         then Just $ head es
                         else if length es == 0
                              then error $ "insertCopyAlongEdge: " ++
                                           show old_d_node ++ " has no " ++
                                           "outgoing definition edge with " ++
                                           "out edge number " ++
                                           pShow (getEdgeOutNr df_edge)
                              else error $ "insertCopyAlongEdge: " ++
                                           show old_d_node ++ " has " ++
                                           "multiple outgoing definition " ++
                                           "edges with out edge number " ++
                                           pShow (getEdgeOutNr df_edge)
                 else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      new_dt = mkNewDataType $ getDataTypeOfValueNode old_d_node
      new_origin = let origins = concatMap getOriginOfValueNode $
                                 filter isValueNodeWithOrigin $
                                 getAllNodes g1
                       fst_chr = head $ head old_d_origin
                       prefix = if length old_d_origin > 0
                                then (if fst_chr /= '%' then "%" else "")
                                     ++ head old_d_origin ++ ".copy."
                                else "%copy."
                   in head $ dropWhile (`elem` origins)
                                       ( map (\i -> prefix ++ show i)
                                             ([1..] :: [Integer])
                                       )
      new_e = let es = getOutEdges g1 new_cp_node
              in if length es == 1
                 then head es
                 else if length es == 0
                      then error $ "insertCopyAlongEdge: " ++
                                   show new_cp_node ++ " has no outgoing edge"
                      else error $ "insertCopyAlongEdge: " ++
                                   show new_cp_node ++ " has multiple " ++
                                   "outgoing edges"

      (g2, new_d_node) =
        insertNewNodeAlongEdge (ValueNode new_dt [new_origin]) new_e g1
      g3 = if isJust def_edge
           then let e = fromJust def_edge
                in fst $ addNewDefEdge (new_d_node, getTargetNode g2 e)
                                       (delEdge e g2)
           else g2
  in g3

-- | Combines value nodes in the function graph that represent the same
-- constant.
combineConstants :: Function -> Function
combineConstants f =
  let g = getGraph f
      const_ns = filter isValueNodeWithConstValue (getAllNodes g)
      areSameValues n1 n2 =
        let d1 = getDataTypeOfValueNode n1
            d2 = getDataTypeOfValueNode n2
            o1 = getOriginOfValueNode n1
            o2 = getOriginOfValueNode n2
        in d1 `areSameConstants` d2 || o1 == o2
      grouped_ns = groupBy areSameValues const_ns
  in foldl combineValueNodes f grouped_ns

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
        entry_node = -- We assume all constant value nodes have an inbound
                     -- data-flow edge from the entry block
                     let n = head ns
                         in_es = getDtFlowInEdges g0 n
                     in if length in_es == 1
                        then getSourceNode g0 (head in_es)
                        else if length in_es == 0
                             then error $ "combineValueNodes: constant " ++
                                          "value node " ++ pShow n ++ " has " ++
                                          "no inbound data-flow edges"
                             else error $ "combineValueNodes: constant " ++
                                          "value node " ++ pShow n ++ " has " ++
                                          "multiple inbound data-flow edges"
        (g2, _) = addNewEdge DataFlowEdge (entry_node, new_n) g1
    in foldr (replaceValueNode new_n) (updateGraph g2 f) ns
  mkNewDataType IntConstType { intConstValue = r } =
    IntConstType { intConstValue = r, intConstNumBits = Nothing }
  mkNewDataType d = error $ "combineValueNodes: unsupported data type " ++
                            show d
  replaceValueNode new_n old_n f' =
    let old_os = functionOS f'
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
    in f' { functionOS = new_os }

-- | Gets the graph of the given function.
getGraph :: Function -> Graph
getGraph = osGraph . functionOS

-- | Replaces the graph in the given function with a new graph.
updateGraph :: Graph -> Function -> Function
updateGraph new_g f =
  let os = functionOS f
      new_os = os { osGraph = new_g }
      new_f = f { functionOS = new_os }
  in new_f

-- | Lowers pointers in a given function graph into corresponding integer values
-- for the given target machine.
--
-- See also 'OS.lowerPointers'.
lowerPointers :: TargetMachine -> Function -> Function
lowerPointers tm f =
  let os0 = functionOS f
      os1 = OS.lowerPointers (tmPointerSize tm)
                             (tmNullPointerValue tm)
                             (tmPointerSymbolRange tm)
                             os0
  in f { functionOS = os1 }

-- | Fixes phi nodes in a given 'OpStructure' that do not abide by the graph
-- invariants:
--    - Phi nodes with multiple data-flow edges to the same value node are
--      transformed such that only one data-flow edge is kept. Concerned
--      definition edges are also replaced with a single definition edge to the
--      last block that dominates the blocks of the replaced definition edges.
--    - Phi nodes with multiple values that originate from the same block are
--      transformed such that only a single value is kept.
--
-- See also 'OS.enforcePhiNodeInvariants'.
enforcePhiNodeInvariants :: Function -> Function
enforcePhiNodeInvariants f =
  let os0 = functionOS f
      os1 = OS.enforcePhiNodeInvariants os0
  in f { functionOS = os1 }

-- | Replaces copies of the same value that act as input to a phi node with a
-- single value, and removes phi nodes that takes only one value.
--
-- See also 'OS.removePhiNodeRedundancies'.
removePhiNodeRedundancies :: Function -> Function
removePhiNodeRedundancies f =
  let os0 = functionOS f
      os1 = OS.removePhiNodeRedundancies os0
  in f { functionOS = os1 }

-- | Removes operation and value nodes whose result are not observable.
--
-- See also 'OS.removeDeadCode'.
removeDeadCode :: Function -> Function
removeDeadCode f =
  let os0 = functionOS f
      os1 = OS.removeDeadCode os0
      g1 = osGraph os1
      ns = map getNodeID $
           getAllNodes g1
      func_inputs = functionInputs f
      new_func_inputs = filter (`elem` ns) func_inputs
  in f { functionOS = os1
       , functionInputs = new_func_inputs
       }

-- | Removes redundant type conversions.
--
-- See also 'OS.removeRedundantConversions'.
removeRedundantConversions :: Function -> Function
removeRedundantConversions f =
  let os0 = functionOS f
      os1 = OS.removeRedundantConversions os0
  in f { functionOS = os1 }
