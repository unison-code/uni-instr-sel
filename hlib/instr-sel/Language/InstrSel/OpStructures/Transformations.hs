{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.OpStructures.Transformations
  ( canonicalizeCopies
  , lowerPointers
  , fixPhis
  )
where

import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.DataTypes
import Language.InstrSel.OpTypes
import Language.InstrSel.OpStructures.Base
import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils
  ( Natural
  , rangeFromSingleton
  , groupBy
  )

import Data.Maybe
  ( isJust
  , fromJust
  )

import Data.List
  ( intersect
  , nub
  )



-------------
-- Functions
-------------

-- | Finds computations that are synonymous with a copy operation, and replaces
-- such computation nodes with 'CopyNode's.
canonicalizeCopies :: OpStructure -> OpStructure
canonicalizeCopies os =
  let mkTempValueNode =
        ValueNode { typeOfValue = AnyType
                  , originOfValue = Nothing
                  }
      mkIntConstValueNode c_val =
        ValueNode { typeOfValue = IntConstType
                                    { intConstValue = rangeFromSingleton c_val
                                    , intConstNumBits = Nothing
                                    }
                  , originOfValue = Nothing
                  }
      mkCompNode op = ComputationNode { compOp = CompArithOp op }
      mkPat op c_val swap_ops =
        mkGraph ( map Node $
                  [ ( 0, NodeLabel 0 (mkCompNode op) )
                  , ( 1, NodeLabel 1 mkTempValueNode )
                  , ( 2, NodeLabel 2 (mkIntConstValueNode c_val) )
                  , ( 3, NodeLabel 3 mkTempValueNode )
                  ]
                )
                ( map Edge $
                  ( [ ( 0, 3, EdgeLabel DataFlowEdge 0 0 ) ]
                    ++
                    if not swap_ops
                    then [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                         , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                         ]
                    else [ ( 1, 0, EdgeLabel DataFlowEdge 0 1 )
                         , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                         ]
                  )
                )
      cp_patterns = concatMap (\(op, c) -> [mkPat op c False, mkPat op c True])
                              [ (IntOp Add,  0)
                              , (IntOp Mul,  1)
                              , (IntOp  Or,  0)
                              , (IntOp And, -1)
                              ]
      fg = osGraph os
      matches = concatMap (findMatches fg) cp_patterns
      process m os' =
        let g0 = osGraph os'
            f_ns = map fNode $ fromMatch m
            comp_n = head $ filter isComputationNode f_ns
            const_n = head $ filter isValueNodeWithConstValue f_ns
            -- There is only one value node with constant value in each pattern,
            -- so we expected there to be exactly one such node in the
            -- match. Same goes for the computation node
            g1 = foldr delEdge g0 $ getEdgesBetween g0 const_n comp_n
            g2 = updateNodeType CopyNode comp_n g1
            dt_es = getDtFlowOutEdges g2 const_n
            g3 = if length dt_es == 0 then delNode const_n g2 else g2
        in os' { osGraph = g3 }
  in foldr process os matches

-- | Lowers pointer value nodes into corresponding integer values. Computation
-- nodes that operate on pointers are also either removed (if they become
-- redundant) or converted into appropriate operations.
lowerPointers
  :: Natural
     -- ^ Size (in number of bits) of a pointer value.
  -> Integer
     -- ^ Value of a null-pointer reference.
  -> OpStructure
  -> OpStructure
lowerPointers ptr_size null_value os =
  -- TODO: check that these transformations do not cause inconsistencies,
  -- for example that something refers to the deleted value nodes
  let g0 = osGraph os
      g1 = transformPtrValueNodesInGraph ptr_size null_value g0
      g2 = removeRedundantPtrConversionsInGraph g1
      g3 = transformPtrConversionsInGraph g2
  in os { osGraph = g3 }

transformPtrValueNodesInGraph :: Natural -> Integer -> Graph -> Graph
transformPtrValueNodesInGraph ptr_size null_value g0 =
  let new_temp_dt = IntTempType { intTempNumBits = ptr_size }
      new_null_dt = IntConstType { intConstValue = rangeFromSingleton null_value
                                 , intConstNumBits = Just ptr_size
                                 }
      ns = filter isValueNodeWithPointerDataType $
           getAllNodes g0
      temp_ptr_ns = filter (isPointerTempType . getDataTypeOfValueNode) ns
      null_ptr_ns = filter (isPointerNullType . getDataTypeOfValueNode) ns
      g1 = foldr (updateDataTypeOfValueNode new_temp_dt) g0 temp_ptr_ns
      g2 = foldr (updateDataTypeOfValueNode new_null_dt) g1 null_ptr_ns
  in g2

removeRedundantPtrConversionsInGraph :: Graph -> Graph
removeRedundantPtrConversionsInGraph g0 =
  let ns = filter ( \n -> let op = getCompOpOfComputationNode n
                          in isCompTypeConvOp op || isCompTypeCastOp op
                  ) $
           filter isComputationNode $
           getAllNodes g0
      ps = map ( \n -> ( n
                       , head $ getPredecessors g0 n
                       , head $ getSuccessors g0 n
                       )
               )
               ns
      redundant_ps = filter ( \(_, v1, v2) ->
                              let d1 = getDataTypeOfValueNode v1
                                  d2 = getDataTypeOfValueNode v2
                              in d1 `hasSameBitWidth` d2
                            )
                            ps
      g1 = foldr (\(o, _, _) g -> delNode o g) g0 redundant_ps
      g2 = foldr (\(_, in_v, out_v) g -> mergeValueNodes in_v out_v g)
                 g1
                 redundant_ps
  in g2

mergeValueNodes :: Node -> Node -> Graph -> Graph
mergeValueNodes in_v out_v g0 =
  -- As a result of these transformations, we may need to fix the out numbers of
  -- the outgoing definition edges. We do that by first removing the definition
  -- edges, merge the nodes, and then re-add them with appropriate out numbers
  let old_def_es = getDefOutEdges g0 out_v
      old_df_es = getDtFlowOutEdges g0 out_v
      phi_b_ps = map ( \e ->
                       let b = getTargetNode g0 e
                           df_e = head $
                                  filter ( \e' -> getEdgeOutNr e' ==
                                                  getEdgeOutNr e
                                         ) $
                                  old_df_es
                           phi = getTargetNode g0 df_e
                       in (phi, b)
                     ) $
                 old_def_es
      g1 = foldr delEdge g0 old_def_es
      g2 = mergeNodes in_v out_v g1
      b_nr_ps = map ( \(phi, b) ->
                      let es = filter (\e -> getTargetNode g2 e == phi) $
                               getDtFlowOutEdges g2 in_v
                      in (b, getEdgeOutNr $ head es)
                    ) $
                phi_b_ps
      g3 = foldr ( \(b, nr) g ->
                   let (g', new_e) = addNewDefEdge (in_v, b) g
                   in fst $ updateEdgeOutNr nr new_e g'
                 )
                 g2
                 b_nr_ps
  in g3

transformPtrConversionsInGraph :: Graph -> Graph
transformPtrConversionsInGraph g =
  let ns = filter ( \n -> let op = getCompOpOfComputationNode n
                          in if isCompTypeConvOp op
                             then let (CompTypeConvOp op') = op
                                  in op' `elem` [IntToPtr, PtrToInt]
                             else False
                  ) $
           filter isComputationNode $
           getAllNodes g
      getBitWidthFromNode n = getTypeBitWidth $ getDataTypeOfValueNode n
      ps = map ( \n -> let v1 = head $ getPredecessors g n
                           v2 = head $ getSuccessors g n
                           w1 = getBitWidthFromNode v1
                           w2 = getBitWidthFromNode v2
                       in if isJust w1
                          then if isJust w2
                               then (n, fromJust w1, fromJust w2)
                               else error $ "transformPtrConversionsInGraph:" ++
                                            " data type of node has no bit " ++
                                            "width: " ++ show v2
                          else error $ "transformPtrConversionsInGraph:" ++
                                       " data type of node has no bit " ++
                                       "width: " ++ show v1
               )
               ns
  in foldr ( \(n, w1, w2) g' ->
             if w1 < w2
             then updateOpOfComputationNode (CompTypeConvOp ZExt) n g'
             else updateOpOfComputationNode (CompTypeConvOp Trunc) n g'
           )
           g
           ps

-- | Fixes phi nodes in a given 'OpStructure' that have multiple data-flow edges
-- to the same value node by only keeping one data-flow edge and replacing all
-- concerned definition edges with a single definition edge to the last block
-- that dominates the blocks of the replaced definition edges.
fixPhis :: OpStructure -> OpStructure
fixPhis os =
  let g = osGraph os
      entry = osEntryBlockNode os
  in if isJust entry
     then let entry_n = findNodesWithNodeID g (fromJust entry)
          in if length entry_n == 1
             then let g0 = fixPhisInGraph g (head entry_n)
                      g1 = removeRedundantPhisInGraph g0
                  in os { osGraph = g1 }
             else if length entry_n == 0
                  then error $ "fixPhis: op-structure has no block node " ++
                               "with ID " ++ pShow entry
                  else error $ "fixPhis: op-structure has multiple block " ++
                               "nodes with ID " ++ pShow entry
     else error $ "fixPhis: op-structure has no entry block"

fixPhisInGraph
  :: Graph
  -> Node
     -- ^ The root (entry) block of the given graph.
  -> Graph
fixPhisInGraph g root =
  let cfg = extractCFG g
      ns = filter ( \n ->
                    let es = getDtFlowInEdges g n
                        vs = nub $
                             map (getSourceNode g) $
                             es
                    in length es /= length vs
                  ) $
           filter isPhiNode $
           getAllNodes g
      fix phi_n g0 =
        let dt_es = getDtFlowInEdges g0 phi_n
            def_es = map ( \e ->
                           let v = getSourceNode g0 e
                               out_nr = getEdgeOutNr e
                               def_e = filter (\e' -> getEdgeOutNr e' == out_nr)
                                              (getDefOutEdges g0 v)
                           in if length def_e == 1
                              then head def_e
                              else error $ "fixPhisInGraph: " ++ show v ++
                                           " has no definition edge " ++
                                           "with out-edge number " ++
                                           pShow out_nr
                         )
                         dt_es
            vb_ps = map ( \(dt_e, def_e) ->
                          (getSourceNode g0 dt_e, getTargetNode g0 def_e)
                        ) $
                    zip dt_es def_es
            grouped_vb_ps = groupBy (\(v1, _) (v2, _) -> v1 == v2) vb_ps
            fixed_vb_ps = map ( \ps ->
                                if length ps == 1
                                then head ps
                                else ( fst $ head ps
                                     , getDomOf cfg root $ map snd ps
                                     )
                              ) $
                          grouped_vb_ps
            g1 = foldr delEdge g0 dt_es
            g2 = foldr delEdge g1 def_es
            g3 = foldr ( \(v, b) g' ->
                         let (g'', dt_e) = addNewDtFlowEdge (v, phi_n) g'
                             out_nr = getEdgeOutNr dt_e
                             (g''', def_e) = addNewDefEdge (v, b) g''
                             g'''' = fst $ updateEdgeOutNr out_nr def_e g'''
                         in g''''
                       )
                       g2
                       fixed_vb_ps
        in g3
  in foldr fix g ns

-- | From a given control-flow graph and list of block nodes, gets the block
-- that is the closest dominator of all given blocks.
getDomOf
  :: Graph
  -> Node
     -- ^ The root (entry) block of the given control-flow graph.
  -> [Node]
  -> Node
getDomOf g root bs =
  let domsets = computeDomSets g root
      bs_domsets = filter (\d -> (domNode d) `elem` bs) domsets
      cs = foldr intersect (domSet $ head bs_domsets) $
           map domSet bs_domsets
      cs_domsets = filter (\d -> (domNode d) `elem` cs) domsets
      pruned_cs_domsets = map (\d -> d { domSet = cs `intersect` domSet d}) $
                          cs_domsets
  in domNode $
     head $
     filter (\d -> length (domSet d) == 1) $
     pruned_cs_domsets

-- | Removes phi nodes that has only one input value.
removeRedundantPhisInGraph :: Graph -> Graph
removeRedundantPhisInGraph g =
  let ns = filter (\n -> length (getDtFlowInEdges g n) == 1) $
           filter isPhiNode $
           getAllNodes g
      remove phi_n g0 =
        let in_e = head $ getInEdges g0 phi_n
            out_e = head $ getOutEdges g0 phi_n
            in_v = getSourceNode g0 in_e
            out_v = getTargetNode g0 out_e
            in_v_def = head $
                       filter (\e -> getEdgeOutNr e == getEdgeOutNr in_e) $
                       getDefOutEdges g0 in_v
            out_v_def = head $
                        filter (\e -> getEdgeInNr e == getEdgeInNr out_e) $
                        getDefInEdges g0 out_v
            g1 = delNode phi_n g0
            g2 = delEdge in_v_def g1
            g3 = delEdge out_v_def g2
            g4 = mergeNodes in_v out_v g3
        in g4
  in foldr remove g ns
