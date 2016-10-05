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
  )
where

import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.DataTypes
import Language.InstrSel.OpTypes
import Language.InstrSel.OpStructures.Base
import Language.InstrSel.Utils
  ( Natural
  , rangeFromSingleton
  )

import Data.Maybe
  ( isJust
  , fromJust
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
      g2 = foldr (\(_, in_v, out_v) g -> mergeNodes in_v out_v g)
                 g1
                 redundant_ps
  in g2

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
