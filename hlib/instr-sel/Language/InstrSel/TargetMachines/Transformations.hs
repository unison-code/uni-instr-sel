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
  ( copyExtend )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )

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
copyExtendGraph g =
  let nodes = filter isValueNode (getAllNodes g)
      edges = concatMap (getDtFlowOutEdges g) nodes
      filtered_edges = filter ( \e ->
                                let src = getSourceNode g e
                                in length (getDtFlowInEdges g src) > 0 ||
                                   isValueNodeWithConstValue src
                              ) $
                       edges
  in foldl insertCopy g filtered_edges

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
                         filter (\n -> getOutEdgeNr n == getOutEdgeNr df_edge) $
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
                      ([1..] :: [Integer]) -- Cast is needed or GHC will
                                           -- complain
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
