{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Transformations
  ( copyExtend
  , lowerPointers
  , alternativeExtend
  )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Functions
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import qualified Language.InstrSel.OpStructures.Transformations as OS
  ( lowerPointers )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.PatternMatching
import Language.InstrSel.Utils
  ( groupBy )
import Language.InstrSel.Utils.Natural

import Data.List
  ( sortBy )
import Data.Maybe
  ( fromJust
  , isJust
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
      new_instrs = map copyExtendInstr (tmInstructions tm)
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
      new_instrs = map lowerInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

-- | For each value node that is copied multiple times, an additional mapping
-- will be inserted for each pattern value node that is mapped to one of the
-- copied values.
alternativeExtend
  :: Function
  -> TargetMachine
  -> Natural
     -- ^ Maximum number of alternatives to insert. 0 means no limit.
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
     -- ^ Maximum number of alternatives to insert. 0 means no limit.
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
        concatMap (processMapping p) $
        fromMatch m
      processMapping p m =
        let fn_id = fNode m
            pn_id = pNode m
            g = osGraph $ patOS p
            pn = let n = findNodesWithNodeID g pn_id
                 in if length n > 0
                    then head n
                    else error $ "insertAlternativeMappings: no pattern " ++
                                 "node with ID " ++ pShow pn_id
        in if fn_id `elem` sorted_vs && not (hasAnyPredecessors g pn)
           then let int_limit = fromIntegral limit
                    num_to_take = if int_limit == 0 ||
                                     int_limit > length sorted_vs
                                  then length sorted_vs
                                  else int_limit
                    alts = take num_to_take $
                           filter (/= fn_id) sorted_vs
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
        in if length pn > 0
           then any (\n -> not $ n `elem` (patExternalData p)) pn
           else False
      vs_r = map (\v -> (v, computeScore v)) vs
      -- Sorted in decreasing score order
      sorted_vs_r = sortBy (\(_, r1) (_, r2) -> compare r2 r1) vs_r
  in map fst sorted_vs_r

getInstrPatternFromPatternMatch :: TargetMachine -> PatternMatch -> InstrPattern
getInstrPatternFromPatternMatch t m =
  let iid = pmInstrID m
      pid = pmPatternID m
      i = findInstruction (tmInstructions t) iid
      p = findInstrPattern (instrPatterns $ fromJust i) pid
  in if isJust i
     then if isJust p
          then fromJust p
          else error $ "getInstrPatternFromPatternMatch: target machine " ++
                       "with instruction ID " ++ pShow iid ++ " has no " ++
                       "pattern with ID " ++ pShow pid
     else error $ "getInstrPatternFromPatternMatch: target machine has no " ++
                  "instruction with ID " ++ pShow iid
