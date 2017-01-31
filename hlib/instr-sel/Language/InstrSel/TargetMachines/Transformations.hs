{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Transformations
  ( combineConstants
  , copyExtend
  , lowerPointers
  , alternativeExtend
  )
where

import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Functions
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import qualified Language.InstrSel.OpStructures.Transformations as OS
  ( lowerPointers )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.TargetMachines.PatternMatching
import Language.InstrSel.Utils
  ( groupBy )
import Language.InstrSel.Utils.Natural

import qualified Data.Map as M
import Data.List
  ( sortBy )
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  )



-------------
-- Functions
-------------

-- | Copy-extends every instruction in the given target machine.
copyExtend :: TargetMachine -> TargetMachine
copyExtend tm =
  let copyExtendOS os = os { osGraph = copyExtendGraph $ osGraph os }
      copyExtendPat p = p { patOS = copyExtendOS $ patOS p }
      copyExtendInstr i =
        let is_copy = isInstructionCopy i
        in if not is_copy
           then i { instrPatterns = map copyExtendPat (instrPatterns i) }
           else i
      new_instrs = M.map copyExtendInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

-- | Inserts a copy node along every data-flow edge that involves a use of a
-- value node except those edges where the value node has no definition (that
-- is, no parents) unless the value node represents a constant value. This also
-- updates the definition edges to retain the same semantics of the original
-- graph. This means that if there is a definition edge $e$ that involves a
-- value node used by a phi node, then upon copy extension $e$ will be moved to
-- the new value node. Otherwise $e$ will remain on the original value
-- node. Note that definition edges where the target is a value node are not
-- affected.
copyExtendGraph :: Graph -> Graph
copyExtendGraph g0 =
  let es = filter ( \e ->
                    let src = getSourceNode g0 e
                    in length (getDtFlowInEdges g0 src) > 0 ||
                       isValueNodeWithConstValue src ||
                       length (getDtFlowOutEdges g0 src) > 1
                  ) $
           concatMap (getDtFlowOutEdges g0) $
           filter isValueNode $
           getAllNodes g0
  in foldl insertCopy g0 es

-- | Inserts a new copy and value node along a given data-flow edge. If the
-- value node is used by a phi node, and there is a definition edge on that
-- value node, then the definition edge with matching out-edge number will be
-- moved to the new data node. Note that definition edges where the target is a
-- value node are not affected.
insertCopy :: Graph -> Edge -> Graph
insertCopy g0 df_edge =
  let old_d_node = getSourceNode g0 df_edge
      old_d_origin = originOfValue $ getNodeType old_d_node
      old_op_n = getTargetNode g0 df_edge
      def_edge = if isPhiNode old_op_n
                 then let d_node_edges = getOutEdges g0 old_d_node
                          def_edges = filter isDefEdge d_node_edges
                      in Just $
                         head $
                         filter (\n -> getEdgeOutNr n == getEdgeOutNr df_edge) $
                         def_edges
                 else Nothing
      (g1, new_cp_node) = insertNewNodeAlongEdge CopyNode df_edge g0
      new_dt = AnyType
      new_origin = Just $
                   let origins = map (fromJust . getOriginOfValueNode) $
                                 filter isValueNodeWithOrigin $
                                 getAllNodes g1
                       prefix = if isJust old_d_origin
                                then (fromJust old_d_origin) ++ ".copy."
                                else "%copy."
                   in head $
                      dropWhile (`elem` origins) $
                      map (\i -> prefix ++ show i) $
                      ([1..] :: [Integer])
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

-- | Lowers pointers in every instruction in the given target machine.
lowerPointers :: TargetMachine -> TargetMachine
lowerPointers tm =
  let lowerPat p = p { patOS = OS.lowerPointers (tmPointerSize tm)
                                                (tmNullPointerValue tm)
                                                (patOS p)
                     }
      lowerInstr i =
        i { instrPatterns = map lowerPat (instrPatterns i) }
      new_instrs = M.map lowerInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

-- | Combines value nodes in every pattern graph that represent the same
-- constant.
combineConstants :: TargetMachine -> TargetMachine
combineConstants tm =
  let processInstr i = i { instrPatterns = map combineConstantsInPattern
                                               (instrPatterns i)
                         }
      new_instrs = M.map processInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

combineConstantsInPattern :: InstrPattern -> InstrPattern
combineConstantsInPattern p =
  let g = getGraph p
      const_ns = filter isValueNodeWithConstValue (getAllNodes g)
      haveSameConstants n1 n2 =
        let d1 = getDataTypeOfValueNode n1
            d2 = getDataTypeOfValueNode n2
        in isSingletonConstant d1 &&
           isSingletonConstant d2 &&
           d1 `areSameConstants` d2
      grouped_ns = groupBy haveSameConstants const_ns
  in foldl combineValueNodes p grouped_ns

combineValueNodes :: InstrPattern -> [Node] -> InstrPattern
combineValueNodes p nodes =
  cc nodes
  where
  cc [] = p
  cc [n] =
    let g = getGraph p
        dt_wo_bitwidth = mkNewDataType $ getDataTypeOfValueNode n
        new_g = updateDataTypeOfValueNode dt_wo_bitwidth n g
    in updateGraph new_g p
  cc ns =
    let nt = ValueNode (mkNewDataType $ getDataTypeOfValueNode $ head ns)
                       (getOriginOfValueNode $ head ns)
             -- Note that the bitwidth is not copied
        g0 = getGraph p
        (g1, new_n) = addNewNode nt g0
        new_p = foldr (replaceValueNode new_n) (updateGraph g1 p) ns
    in new_p
  mkNewDataType IntConstType { intConstValue = r } =
    IntConstType { intConstValue = r, intConstNumBits = Nothing }
  mkNewDataType d = error $ "combineValueNodes: unsupported data type " ++
                            show d
  replaceValueNode new_n old_n p' =
    let old_os = patOS p'
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
        old_emit_str = patEmitString p'
        new_emit_str = replaceNodeIDInEmitString (getNodeID old_n)
                                                 (getNodeID new_n)
                                                 old_emit_str
    in p' { patOS = new_os
          , patEmitString = new_emit_str
          }

-- | Gets the graph of the given pattern.
getGraph :: InstrPattern -> Graph
getGraph = osGraph . patOS

-- | Replaces the graph in the given pattern with a new graph.
updateGraph :: Graph -> InstrPattern -> InstrPattern
updateGraph new_g p =
  let os = patOS p
      new_os = os { osGraph = new_g }
      new_p = p { patOS = new_os }
  in new_p

-- | Replaces a given node ID in the given 'EmitStringTemplate'.
replaceNodeIDInEmitString
  :: NodeID
     -- ^ The node ID to replace.
  -> NodeID
     -- ^ The node ID to replace with.
  -> EmitStringTemplate
  -> EmitStringTemplate
replaceNodeIDInEmitString old_n new_n str =
  EmitStringTemplate { emitStrParts = map processOuter $
                                      emitStrParts str
                     }
  where
  processOuter = map processPart
  processPart p@(ESVerbatim {})           = p
  processPart p@(ESLocationOfValueNode n) = if n == old_n
                                            then ESLocationOfValueNode new_n
                                            else p
  processPart p@(ESIntConstOfValueNode n) = if n == old_n
                                            then ESIntConstOfValueNode new_n
                                            else p
  processPart p@(ESNameOfBlockNode     n) = if n == old_n
                                            then ESNameOfBlockNode new_n
                                            else p
  processPart p@(ESBlockOfValueNode    n) = if n == old_n
                                            then ESBlockOfValueNode new_n
                                            else p
  processPart p@(ESFuncOfCallNode      n) = if n == old_n
                                            then ESFuncOfCallNode new_n
                                            else p
  processPart p@(ESLocalTemporary {})   = p

-- | For each value node that is copied multiple times, an additional mapping
-- will be inserted for each pattern value node that is mapped to one of the
-- copied values.
alternativeExtend
  :: Function
  -> TargetMachine
  -> Natural
     -- ^ Maximum number of alternatives per case. 0 means no limit.
  -> PatternMatchset
  -> PatternMatchset
alternativeExtend f t limit p =
  let g = osGraph $ functionOS f
      v_ns = filter isValueNode $ getAllNodes g
      copy_related_vs =
        map (map getNodeID) $
        concat $
        map ( \n ->
              let es = getDtFlowOutEdges g n
                  copies = filter isCopyNode $ map (getTargetNode g) es
                  cp_vs = map ( \n' ->
                                let es' = getDtFlowOutEdges g n'
                                in if length es' == 1
                                   then getTargetNode g (head es')
                                   else if length es' == 0
                                        then error $
                                             "alternativeExtend: " ++
                                             show n' ++ " has no data-flow " ++
                                             "edges"
                                        else error $
                                             "alternativeExtend: " ++
                                             show n' ++ " has multiple " ++
                                             "data-flow edges"
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
  in foldr (insertAlternativeMappings t limit) p copy_related_vs

insertAlternativeMappings
  :: TargetMachine
  -> Natural
     -- ^ Maximum number of alternatives per case. 0 means no limit.
  -> [NodeID]
  -> PatternMatchset
  -> PatternMatchset
insertAlternativeMappings t limit vs pm =
  let sorted_vs = sortValueNodesByReusability t pm vs
      processPatternMatch m =
        let p = getInstrPatternFromPatternMatch t m
        in m { pmMatch = processMatch p (pmMatch m) }
      processMatch p m =
        toMatch $
        concatMap (processMapping p m) $
        fromMatch m
      processMapping p match m =
        let fn_id = fNode m
            pn_id = pNode m
            g = osGraph $ patOS p
            pn = let n = findNodesWithNodeID g pn_id
                 in if length n > 0
                    then head n
                    else error $ "insertAlternativeMappings: no pattern " ++
                                 "node with ID " ++ pShow pn_id
        in if fn_id `elem` sorted_vs && not (hasAnyPredecessors g pn)
           then let isCandidate n =
                      let pn_id' = let n' = findFNInMatch match n
                                   in if length n' > 0
                                      then Just $ head n'
                                      else Nothing
                          pn' = if isJust pn_id'
                                then let n' = findNodesWithNodeID g $
                                             fromJust pn_id'
                                     in if length n' > 0
                                        then Just $ head n'
                                        else error $
                                             "insertAlternativeMappings: " ++
                                             "no pattern node with ID " ++
                                             pShow pn_id
                                else Nothing
                      in isNothing pn' ||
                         not (hasAnyPredecessors g (fromJust pn'))
                    candidate_vs = filter isCandidate sorted_vs
                    num_to_take =
                      if limit == 0 then maxBound else fromIntegral limit - 1
                    alts = take num_to_take $
                           filter (/= fn_id) candidate_vs
                    alt_maps = map (\n -> Mapping { fNode = n, pNode = pn_id })
                                   alts
                in (m:alt_maps)
           else [m]
      new_matches = map processPatternMatch $ pmMatches pm
  in pm { pmMatches = new_matches }

-- | Arranges the nodes in decreasing order of reusability. A node has high
-- reusability if the ratio between the number of matches that use the node and
-- the number of matches that would make the value unreusable. A value becomes
-- unreusable if the match both uses and defines the value and does not make it
-- externally available.
sortValueNodesByReusability
  :: TargetMachine
  -> PatternMatchset
  -> [NodeID]
  -> [NodeID]
sortValueNodesByReusability t pm vs =
  let computeScore v =
        let num_uses = length $
                       filter (usesValue v) $
                       pmMatches pm
            num_consumps = length $
                           filter (consumesValue v) $
                           pmMatches pm
            ratio = fromIntegral num_uses / fromIntegral num_consumps
        in ratio :: Float
      usesValue v m =
        let pn = findPNInMatch (pmMatch m) v
            g = osGraph $ patOS $ getInstrPatternFromPatternMatch t m
        in if length pn > 0
           then any ( \nid ->
                      let isUse n = let succs = getSuccessors g n
                                    in length succs > 0
                      in any isUse (findNodesWithNodeID g nid)
                    )
                    pn
           else False
      consumesValue v m =
        let pn = findPNInMatch (pmMatch m) v
            p = getInstrPatternFromPatternMatch t m
            ext_ns = patInputData p ++ patOutputData p
        in if length pn > 0
           then any (\n -> not $ n `elem` ext_ns) pn
           else False
      vs_r = map (\v -> (v, computeScore v)) vs
      -- Sorted in decreasing score order
      sorted_vs_r = sortBy (\(_, r1) (_, r2) -> compare r2 r1) vs_r
  in map fst sorted_vs_r
