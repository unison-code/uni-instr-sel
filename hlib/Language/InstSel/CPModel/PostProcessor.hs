--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.PostProcessor
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Performs the post-processing of the CP solution and post-processing
-- parameters.
--
--------------------------------------------------------------------------------

module Language.InstSel.CPModel.PostProcessor
  ( ControlDataFlowDAG
  , emitInstructions
  , mkControlDataFlowDAG
  )
where

import Language.InstSel.CPModel.Base
import Language.InstSel.Graphs
  ( NodeID )
import Language.InstSel.Patterns.IDs
import Language.InstSel.TargetMachine
  hiding
  ( patAssIDMaps )
import qualified Data.Graph.Inductive as I
import Data.Maybe



--------------
-- Data types
--------------

-- | A data type representing a DAG where the nodes represent pattern instances,
-- and the directed edges represent control and data dependencies between the
-- pattern instances. Each edge represents either the data flowing from one
-- pattern to another, or that there is a state or control dependency between
-- the two.
newtype ControlDataFlowDAG =
  ControlDataFlowDAG { getIntDag :: IControlDataFlowDAG }

-- | Type synonym for the internal graph.
type IControlDataFlowDAG = I.Gr PatternInstanceID ()



-------------
-- Functions
-------------

-- | Takes a CP solution data set and a list of pattern instance IDs, and
-- produces a control and data dependency DAG such that every pattern instance
-- ID is represented by a node, and there is a directed edge between two nodes
-- if the pattern instance indicated by the target node uses data produced by
-- the pattern instance indicated by the source node, or if the destination node
-- represents a pattern with control nodes. Cyclic data dependencies are broken
-- such that the pattern containing the phi node which makes use of the data
-- appears at the top of the DAG. Cyclic control dependencies will appear if
-- there is more than one pattern instance with control nodes in the list.
mkControlDataFlowDAG ::
     CPSolutionData
  -> [PatternInstanceID]
  -> ControlDataFlowDAG
mkControlDataFlowDAG cp_data is =
  let g0 = I.mkGraph (zip [0..] is) []
      g1 = foldr (addUseEdgesToDAG cp_data) g0 is
      g2 = foldr (addControlEdgesToDAG cp_data) g1 is
  in ControlDataFlowDAG g2

-- | Adds an edge for each use of data or state of the given pattern instance
-- ID. If the source node is not present in the graph, no edge is added. It is
-- assumed that there always exists exactly one node in the graph representing
-- the pattern instance ID given as argument to the function. Note that this may
-- lead to cycles, which will have to be broken as a second step.
addUseEdgesToDAG ::
     CPSolutionData
  -> PatternInstanceID
  -> IControlDataFlowDAG
  -> IControlDataFlowDAG
addUseEdgesToDAG cp_data pid g0 =
  let ds = patInstData $ modelParams cp_data
      pi_n = fromJust $ getNodeOfPI g0 pid
      pi_data = getPIData ds pid
      ns = I.labNodes g0
      d_uses_of_pi = filter
                     (`notElem` patDataNodesUsedByPhis pi_data)
                     (patDataNodesUsed pi_data)
      s_uses_of_pi = patStateNodesUsed pi_data
      ns_d_defs = map (\(n, i) -> (n, patDataNodesDefined $ getPIData ds i)) ns
      ns_s_defs = map (\(n, i) -> (n, patStateNodesDefined $ getPIData ds i)) ns
      g1 = foldr (addUseEdgesToDAG' pi_n ns_d_defs) g0 d_uses_of_pi
      g2 = foldr (addUseEdgesToDAG' pi_n ns_s_defs) g1 s_uses_of_pi
  in g2

addUseEdgesToDAG' ::
     I.Node
  -> [(I.Node, [NodeID])]
     -- ^ List of defs.
  -> NodeID
     -- ^ A use.
  -> IControlDataFlowDAG
  -> IControlDataFlowDAG
addUseEdgesToDAG' n def_maps use g =
  let ns = map fst $ filter (\m -> use `elem` snd m) def_maps
  in foldr (\n' g' -> I.insEdge (n', n, ()) g') g ns

-- | If the given pattern instance ID represents a pattern with control nodes,
-- then an edge will be added to the node of that pattern instance ID from every
-- other node.
addControlEdgesToDAG ::
     CPSolutionData
  -> PatternInstanceID
  -> IControlDataFlowDAG
  -> IControlDataFlowDAG
addControlEdgesToDAG cp_data pid g =
  let pi_data = getPIData (patInstData $ modelParams cp_data) pid
  in if patHasControlNodes pi_data
     then let ns = I.labNodes g
              pi_n = fst $ head $ filter (\(_, i) -> i == pid) ns
              other_ns = map fst $ filter (\(_, i) -> i /= pid) ns
          in foldr (\n' g' -> I.insEdge (n', pi_n, ()) g') g other_ns
     else g

-- | Gets the internal node ID (if any) of the node with a given pattern
-- instance ID as its label. It is assumed that there is always at most one such
-- node in the graph.
getNodeOfPI :: IControlDataFlowDAG -> PatternInstanceID -> Maybe I.Node
getNodeOfPI g pid =
  let ns = filter (\n -> snd n == pid) $ I.labNodes g
  in if length ns > 0
     then Just (fst $ head ns)
     else Nothing

-- | Retrieves the 'PatternInstanceData' entity with matching pattern instance
-- ID. It is assumed that exactly one such entity always exists in the given
-- list.
getPIData :: [PatternInstanceData] -> PatternInstanceID -> PatternInstanceData
getPIData ps piid = fromJust $ findPatternInstanceData ps piid

-- | Retrieves the 'Instruction' entity with matching instruction ID. It is
-- assumed that such an entity always exists in the given list.
getInst :: [Instruction] -> InstructionID -> Instruction
getInst is iid = fromJust $ findInstruction is iid

-- | Emits a list of assembly instructions for a given 'ControlDataFlowDAG'.
emitInstructions ::
     CPSolutionData
  -> TargetMachine
  -> ControlDataFlowDAG
  -> [String]
emitInstructions cp m dag =
  let sorted_pis = I.topsort' (getIntDag dag)
  in filter (\i -> length i > 0) $ map (emitInstruction cp m) sorted_pis

-- | Emits the corresponding instruction of a given pattern instance ID.
emitInstruction ::
     CPSolutionData
  -> TargetMachine
  -> PatternInstanceID
  -> String
emitInstruction cp m piid =
  let pi_data = getPIData (patInstData $ modelParams cp) piid
      i_data = getInst (tmInstructions m) (patInstructionID pi_data)
      inst_ass_parts = instAssemblyStr i_data
  in concatMap (produceInstPart cp m pi_data) (assStrParts inst_ass_parts)

getNIDFromAID :: PatternInstanceData -> AssemblyID -> NodeID
getNIDFromAID pid aid = (patAssIDMaps pid) !! (fromIntegral aid)

lookupBBLabel :: NodeID -> [BBLabelData] -> Maybe BBLabelID
lookupBBLabel n ds =
  let found = filter (\d -> labNode d == n) ds
  in if length found > 0
     then Just $ labBB $ head found
     else Nothing

produceInstPart ::
     CPSolutionData
  -> TargetMachine
  -> PatternInstanceData
  -> AssemblyPart
  -> String
produceInstPart _ _ _ (AssemblyVerbatim s) = s
produceInstPart cp _ pid (AssemblyImmValue aid) =
  let n = getNIDFromAID pid aid
      imm = lookup n $ immValuesOfDataNodes cp
      prefix = "#"
  in if isJust imm
     then prefix ++ (show $ fromJust imm)
     else prefix ++ "?"
produceInstPart cp m pid (AssemblyRegister aid) =
  let n = getNIDFromAID pid aid
      reg = lookup n $ regsOfDataNodes cp
  in if isJust reg
     then let regsym = fromJust $ lookup (fromJust reg) (tmRegisters m)
          in show regsym
     else "?"
produceInstPart cp _ pid (AssemblyBBLabel aid) =
  let n = getNIDFromAID pid aid
      lab = lookupBBLabel n $ funcBBLabels $ funcData $ modelParams cp
  in if isJust lab
     then show $ fromJust lab
     else "?"
