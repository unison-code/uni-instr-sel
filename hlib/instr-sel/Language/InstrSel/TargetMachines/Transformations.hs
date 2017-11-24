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
      copyExtendInstr i = i { instrOS = copyExtendOS $ instrOS i }
      new_instrs = M.map copyExtendInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

-- | For every value node that either has at least one inbound data-flow edge,
-- represents a constant value, or has at least two outbound data-flow edges,
-- insert a copy node along every data-flow edge (except for edges where the
-- target is a copy node or the source already has already been copy
-- extended). This also updates the definition edges to retain the same
-- semantics of the original graph. This means that if there is a definition
-- edge $e$ that involves a value node used by a phi node, then upon copy
-- extension $e$ will be moved to the new value node. Otherwise $e$ will remain
-- on the original value node. Note that definition edges where the target is a
-- value node are not affected.
copyExtendGraph :: Graph -> Graph
copyExtendGraph g =
  let nodes = filter ( \n ->
                       let es = getDtFlowInEdges g n
                       in if length es > 0
                          then not $ isCopyNode $ getSourceNode g $ head es
                          else True
                     ) $
              filter ( \n -> length (getDtFlowInEdges g n) > 0 ||
                             -- To handle input values that are constants
                             isValueNodeWithConstValue n ||
                             -- To handle intput values that are used multiple
                             -- times
                             length (getDtFlowOutEdges g n) > 1
                     ) $
              filter isValueNode $
              getAllNodes g
  in foldl insertCopies g nodes

-- | Inserts a new copy and value node along each outgoing data-flow edge from
-- the given value node (except for edges where the target is already a copy
-- node).
insertCopies :: Graph -> Node -> Graph
insertCopies g0 n =
  let es = filter (not . isCopyNode . getTargetNode g0) $
           getDtFlowOutEdges g0 n
      g1 = foldl insertCopyAlongEdge g0 es
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
      mkNewDataType AnyType = AnyType
      mkNewDataType d = error $ "insertCopyAlongEdge: " ++ show d ++ " not " ++
                                "supported"
                        -- This happens if copy extension is attempted before
                        -- constants have been combined. In many cases, the bit
                        -- width for a constant is known somewhere in a pattern
                        -- but not everywhere. By combining all equal constants
                        -- afterwards, this information is propagated such that
                        -- no constant has unknown bit width.
      old_d_node = getSourceNode g0 df_edge
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
      new_dt = mkNewDataType $ getDataTypeOfValueNode old_d_node
      new_origin = let origins = concatMap getOriginOfValueNode $
                                 filter isValueNodeWithOrigin $
                                 getAllNodes g1
                       fst_chr = head $ head old_d_origin
                       prefix = if length old_d_origin > 0
                                then (if fst_chr /= '%' then "%" else "")
                                     ++ head old_d_origin ++ ".copy."
                                else "%copy."
                   in head $
                      dropWhile (`elem` origins) $
                      map (\i -> prefix ++ show i) $
                      ([1..] :: [Integer])
      (g2, new_d_node) =
        insertNewNodeAlongEdge (ValueNode new_dt [new_origin])
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
  let lowerInstr i = i { instrOS = OS.lowerPointers (tmPointerSize tm)
                                                    (tmNullPointerValue tm)
                                                    (tmPointerSymbolRange tm)
                                                    (instrOS i)
                       }
      new_instrs = M.map lowerInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

-- | Combines value nodes in every pattern graph that represent the same
-- constant.
combineConstants :: TargetMachine -> TargetMachine
combineConstants tm =
  let new_instrs = M.map combineConstantsInInstruction (tmInstructions tm)
  in tm { tmInstructions = new_instrs }

combineConstantsInInstruction :: Instruction -> Instruction
combineConstantsInInstruction i =
  let g = getGraph i
      const_ns = filter isValueNodeWithConstValue (getAllNodes g)
      haveSameConstants n1 n2 =
        let d1 = getDataTypeOfValueNode n1
            d2 = getDataTypeOfValueNode n2
        in d1 `areSameConstants` d2
      grouped_ns = groupBy haveSameConstants const_ns
  in foldl combineValueNodes i grouped_ns

combineValueNodes :: Instruction -> [Node] -> Instruction
combineValueNodes i nodes =
  cc nodes
  where
  cc [] = i
  cc [n] =
    let g = getGraph i
        dt_wo_bitwidth = mkNewDataType $ getDataTypeOfValueNode n
        new_g = updateDataTypeOfValueNode dt_wo_bitwidth n g
    in updateGraph new_g i
  cc ns =
    let nt = ValueNode (mkNewDataType $ getDataTypeOfValueNode $ head ns)
                       (getOriginOfValueNode $ head ns)
             -- Note that the bitwidth is not copied
        g0 = getGraph i
        (g1, new_n) = addNewNode nt g0
        new_i = foldr (replaceValueNode new_n) (updateGraph g1 i) ns
    in new_i
  mkNewDataType IntConstType { intConstValue = r } =
    IntConstType { intConstValue = r, intConstNumBits = Nothing }
  mkNewDataType d = error $ "combineValueNodes: unsupported data type " ++
                            show d
  replaceValueNode new_n old_n i' =
    let old_os = instrOS i'
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
        old_emit_str = instrEmitString i'
        new_emit_str = replaceNodeIDInEmitString (getNodeID old_n)
                                                 (getNodeID new_n)
                                                 old_emit_str
    in i' { instrOS = new_os
          , instrEmitString = new_emit_str
          }

-- | Gets the graph of the given instruction.
getGraph :: Instruction -> Graph
getGraph = osGraph . instrOS

-- | Replaces the graph in the given instruction with a new graph.
updateGraph :: Graph -> Instruction -> Instruction
updateGraph new_g i =
  let os = instrOS i
      new_os = os { osGraph = new_g }
  in i { instrOS = new_os }

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
      copy_related_vs = map (map getNodeID) $
                        filter ((> 1) . length) $
                        concat $
                        map ( groupBy ( \v1 v2 -> getDataTypeOfValueNode v1 ==
                                                  getDataTypeOfValueNode v2
                                      )
                            ) $
                        map (getCopiesOfValue g) v_ns
  in foldr (insertAlternativeMappings t limit) p copy_related_vs

-- | Given a graph and value node, returns all value nodes that are copies of
-- the given value node.
getCopiesOfValue :: Graph -> Node -> [Node]
getCopiesOfValue g n =
  let es = getDtFlowOutEdges g n
      copies = filter isCopyNode $ map (getTargetNode g) es
      cp_vs = map ( \n' ->
                    let es' = getDtFlowOutEdges g n'
                    in if length es' == 1
                       then getTargetNode g (head es')
                       else if length es' == 0
                            then error $
                                 "getCopiesOfValue: " ++ show n' ++
                                 " has no data-flow edges"
                            else error $
                                 "getCopiesOfValue: " ++ show n' ++
                                 " has multiple data-flow edges"
                  ) $
              copies
  in cp_vs

insertAlternativeMappings
  :: TargetMachine
  -> Natural
     -- ^ Maximum number of alternatives per case. 0 means no limit.
  -> [NodeID]
     -- ^ Set of copy-related values.
  -> PatternMatchset
  -> PatternMatchset
insertAlternativeMappings t limit vs p =
  let processPatternMatch pm =
        pm { pmMatch = toMatch $
                       concatMap (processMapping pm) $
                       fromMatch $
                       pmMatch pm
           }
      processMapping pm m =
        let sorted_vs = sortValueNodesByReusability t p vs
            alt_m = computeAlternativeMappings t limit sorted_vs pm m
        in (m:alt_m)
  in p { pmMatches = map processPatternMatch $ pmMatches p }

-- | Arranges the nodes in decreasing order of reusability. A node has high
-- reusability if the ratio between the number of matches that use the node and
-- the number of matches that would make the value unreusable. A value becomes
-- unreusable if the match both uses and defines the value and does not make it
-- externally available.
sortValueNodesByReusability
  :: TargetMachine
  -> PatternMatchset
  -> [NodeID]
     -- ^ Set of copy-related values.
  -> [NodeID]
sortValueNodesByReusability t p vs =
  let computeScore v =
        let num_uses = length $
                       filter (usesValue v) $
                       pmMatches p
            num_consumps = length $
                           filter (consumesValue v) $
                           pmMatches p
            ratio = fromIntegral num_uses / fromIntegral num_consumps
        in ratio :: Float
      usesValue v m =
        let pn = findPNInMatch (pmMatch m) v
            g = getGraph $ getInstructionFromPatternMatch t m
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
            i = getInstructionFromPatternMatch t m
            ext_ns = instrInputData i ++ instrOutputData i
        in if length pn > 0
           then any (\n -> not $ n `elem` ext_ns) pn
           else False
      vs_r = map (\v -> (v, computeScore v)) vs
      -- Sorted in decreasing score order
      sorted_vs_r = sortBy (\(_, r1) (_, r2) -> compare r2 r1) vs_r
  in map fst sorted_vs_r

-- | Given a mapping, computes a set of alternative mappings from the set of
-- copy-related values.
computeAlternativeMappings
  :: TargetMachine
  -> Natural
     -- ^ Maximum number of alternatives per case. 0 means no limit.
  -> [NodeID]
     -- ^ Set of copy-related values. It is assumed they are sorted in
     -- decreasing order of usability.
  -> PatternMatch
  -> Mapping NodeID
  -> [Mapping NodeID]
computeAlternativeMappings t limit sorted_vs pm m =
  let fn_id = fNode m
      pn_id = pNode m
      i = getInstructionFromPatternMatch t pm
      g = getGraph i
      pn = let n = findNodesWithNodeID g pn_id
           in if length n > 0
              then head n
              else error $ "computeAlternativeMappings: no pattern " ++
                           "node with ID " ++ pShow pn_id
  in if fn_id `elem` sorted_vs && not (hasAnyPredecessors g pn)
     then let isCandidate n =
                let pn_id' = let n' = findPNInMatch (pmMatch pm) n
                             in if length n' > 0
                                then Just $ head n'
                                else Nothing
                    pn' = if isJust pn_id'
                          then let n' = findNodesWithNodeID g $
                                       fromJust pn_id'
                               in if length n' > 0
                                  then Just $ head n'
                                  else error $
                                       "computeAlternativeMappings: " ++
                                       "no pattern node with ID " ++
                                       pShow pn_id'
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
          in alt_maps
     else []
