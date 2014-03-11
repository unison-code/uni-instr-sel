--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Graphs.VFTwo
-- Copyright   :  (c) Gabriel Hjort Blindell 2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains an implementation of the VF2 subgraph isomorphism algorithm
-- (http://dx.doi.org/10.1109/TPAMI.2004.75). There are always two graphs
-- involved: the search graph and the pattern graph. The search graph is the
-- graph on which another graph will be matched. The graph to match is called
-- the pattern graph.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.VFTwo (
  NodeMatchset
, NodeMapping
, match
) where

import Language.InstructionSelection.Graphs.Base
import Data.List (intersect, union, (\\))



-------------
-- Functions
-------------

match :: Graph            -- ^ The search graph.
         -> Graph         -- ^ The pattern graph.
         -> [NodeMatchset] -- ^ Found matches.
match sg pg = match' sg pg []

-- | Implements the VF2 algorithm. The algorithm first finds a set of node
-- mapping candidates, and then applies a feasibility check on each of
-- them. Each candidate that passes the test is added to the existing mapping
-- state, and then the function is recursively called.

match' :: Graph             -- ^ The search graph.
          -> Graph          -- ^ The pattern graph.
          -> NodeMatchset   -- ^ The current matchset state.
          -> [NodeMatchset] -- ^ Found matches.
match' sg pg st =
  if length st == numNodes pg
     then [st]
     else let candidates = getCandidates sg pg st
              feasible_candidates = filter (checkFeasibility sg pg st)
                                    candidates
              new_states = map (:st) feasible_candidates
          in concatMap (match' sg pg) new_states

-- | Gets a set of node mapping candidates. This set consists of the pairs of
-- nodes which are successors to the nodes currently in the match set. If this
-- resultant set is empty, then the set consists of the pairs of nodes which are
-- corresponding predecessors. If this set too is empty, then the returned set
-- consists of the pairs of nodes not contained in the match set.

getCandidates :: Graph           -- ^ The search graph.
                 -> Graph        -- ^ The pattern graph.
                 -> NodeMatchset -- ^ The current matchset state.
                 -> NodeMatchset -- ^ Potential candidates.
getCandidates sg pg st =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      t_out_sg = getNonMappedSuccsOfMappedNodes mapped_ns_sg sg
      t_out_pg = getNonMappedSuccsOfMappedNodes mapped_ns_pg pg
      pairs_out = if length t_out_pg > 0
                     then [ (n, head t_out_pg) | n <- t_out_sg ]
                     else []
      t_in_sg = getNonMappedPredsOfMappedNodes mapped_ns_sg sg
      t_in_pg = getNonMappedPredsOfMappedNodes mapped_ns_pg pg
      pairs_in = if length t_in_pg > 0
                    then [ (n, head t_in_pg) | n <- t_in_sg ]
                    else []
      t_d_sg = filter (`notElem` mapped_ns_sg) (allNodes sg)
      t_d_pg = filter (`notElem` mapped_ns_pg) (allNodes pg)
      pairs_d = if length t_d_pg > 0
                   then [ (n, head t_d_pg) | n <- t_d_sg ]
                   else []
  in if length pairs_out > 0
        then pairs_out
        else if length pairs_in > 0
             then pairs_in
             else pairs_d

-- | Checks that the node mapping is feasible by comparing their syntax and
-- semantics.

checkFeasibility :: Graph           -- ^ The search graph.
                    -> Graph        -- ^ The pattern graph.
                    -> NodeMatchset -- ^ Current matchset state.
                    -> NodeMapping  -- ^ Candidate mapping.
                    -> Bool
checkFeasibility sg pg st pair =
  (checkSemantics sg pg st pair) && (checkSyntax sg pg st pair)

-- | Checks that the nodes are of the same type and that the edges are
-- compatible.

checkSemantics :: Graph           -- ^ The search graph.
                  -> Graph        -- ^ The pattern graph.
                  -> NodeMatchset -- ^ Current matchset state.
                  -> NodeMapping  -- ^ Candidate mapping.
                  -> Bool
checkSemantics sg pg st (n, m) =
  (nodeType n) == (nodeType m) && (checkEdges sg pg st (n, m))

-- | Checks that for nodes with ordered edges the edges match.

checkEdges :: Graph           -- ^ The search graph.
              -> Graph        -- ^ The pattern graph.
              -> NodeMatchset -- ^ Current matchset state.
              -> NodeMapping  -- ^ Candidate mapping.
              -> Bool
checkEdges sg pg st pair =
  (checkInEdges sg pg st pair) && (checkOutEdges sg pg st pair)

-- | Checks that the already-mapped predecessors of the pattern node in the
-- candidate mapping, for those which has ordered outbound edges, that the
-- numbers are the same.

checkOutEdges :: Graph           -- ^ The search graph.
                 -> Graph        -- ^ The pattern graph.
                 -> NodeMatchset -- ^ Current matchset state.
                 -> NodeMapping  -- ^ Candidate mapping.
                 -> Bool
checkOutEdges sg pg st (n, m) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      mapped_preds_m = getMappedPredsOfMappedNode m mapped_ns_pg pg
      pg_preds_to_check = filter hasOrderedOutEdges mapped_preds_m
      sg_preds_to_check = map (getMappedSNode st) pg_preds_to_check
      pg_edges = concatMap (edges pg m) pg_preds_to_check
      sg_edges = concatMap (edges sg n) sg_preds_to_check
      edge_pairs = zip pg_edges sg_edges
  in all (\(pg_e, sg_e) -> (outEdgeNr pg_e) == (outEdgeNr sg_e)) edge_pairs

-- | Same as checkOutEdges but for predecessors and their inbound edges.

checkInEdges :: Graph           -- ^ The search graph.
                -> Graph        -- ^ The pattern graph.
                -> NodeMatchset -- ^ Current matchset state.
                -> NodeMapping  -- ^ Candidate mapping.
                -> Bool
checkInEdges sg pg st (n, m) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      mapped_succs_m = getMappedSuccsOfMappedNode m mapped_ns_pg pg
      pg_succs_to_check = filter hasOrderedInEdges mapped_succs_m
      sg_succs_to_check = map (getMappedSNode st) pg_succs_to_check
      pg_edges = concatMap (edges pg m) pg_succs_to_check
      sg_edges = concatMap (edges sg n) sg_succs_to_check
      edge_pairs = zip pg_edges sg_edges
  in all (\(pg_e, sg_e) -> (inEdgeNr pg_e) == (inEdgeNr sg_e)) edge_pairs

-- | Checks that the syntax of matched nodes are compatible (equation 2 in the
-- paper).

checkSyntax :: Graph           -- ^ The search graph.
               -> Graph        -- ^ The pattern graph.
               -> NodeMatchset -- ^ Current matchset state.
               -> NodeMapping  -- ^ Candidate mapping.
               -> Bool
checkSyntax sg pg st pair =
     (checkSyntaxPred sg pg st pair)
  && (checkSyntaxSucc sg pg st pair)
  && (checkSyntaxIn sg pg st pair)
  && (checkSyntaxOut sg pg st pair)
  && (checkSyntaxNew sg pg st pair)

-- | Checks that for each predecessor A of the matched node that appears in the
-- current matchset state, there also exists some node mapping for A (equation 3
-- in the paper). This is a consistency check.

checkSyntaxPred :: Graph           -- ^ The search graph.
                   -> Graph        -- ^ The pattern graph.
                   -> NodeMatchset -- ^ Current matchset state.
                   -> NodeMapping  -- ^ Candidate mapping.
                   -> Bool
checkSyntaxPred sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      preds_sn = predecessors sg sn
      preds_pn = predecessors pg pn
      preds_sn_in_st = preds_sn `intersect` mapped_ns_sg
      preds_pn_in_st = preds_pn `intersect` mapped_ns_pg
  in    all (\n -> any (\m -> (n, m) `elem` st) preds_pn) preds_sn_in_st
     && all (\m -> any (\n -> (n, m) `elem` st) preds_sn) preds_pn_in_st

-- | Same as checkSyntaxPred but for the successors (equation 4 in the paper).

checkSyntaxSucc :: Graph           -- ^ The search graph.
                   -> Graph        -- ^ The pattern graph.
                   -> NodeMatchset -- ^ Current matchset state.
                   -> NodeMapping  -- ^ Candidate mapping.
                   -> Bool
checkSyntaxSucc sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      succs_sn = successors sg sn
      succs_pn = successors pg pn
      succs_sn_in_st = succs_sn `intersect` mapped_ns_sg
      succs_pn_in_st = succs_pn `intersect` mapped_ns_pg
  in    all (\n -> any (\m -> (n, m) `elem` st) succs_pn) succs_sn_in_st
     && all (\m -> any (\n -> (n, m) `elem` st) succs_sn) succs_pn_in_st

-- | Checks that there exists a sufficient number of predecessors to map in the
-- search graph (equation 5 in the paper). This is a 1-look-ahead check.

checkSyntaxIn :: Graph           -- ^ The search graph.
                 -> Graph        -- ^ The pattern graph.
                 -> NodeMatchset -- ^ Current matchset state.
                 -> NodeMapping  -- ^ Candidate mapping.
                 -> Bool
checkSyntaxIn sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      preds_sn = predecessors sg sn
      preds_pn = predecessors pg pn
      succs_sn = successors sg sn
      succs_pn = successors pg pn
      t_in_sg = getNonMappedPredsOfMappedNodes mapped_ns_sg sg
      t_in_pg = getNonMappedPredsOfMappedNodes mapped_ns_pg pg
  in    length (succs_sn `intersect` t_in_sg)
        >= length (succs_pn `intersect` t_in_pg)
     && length (preds_sn `intersect` t_in_sg)
        >= length (preds_pn `intersect` t_in_pg)

-- | Same as checkSyntaxIn but for successors (equation 6 in the paper).

checkSyntaxOut :: Graph           -- ^ The search graph.
                  -> Graph        -- ^ The pattern graph.
                  -> NodeMatchset -- ^ Current matchset state.
                  -> NodeMapping  -- ^ Candidate mapping.
                  -> Bool
checkSyntaxOut sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      preds_sn = predecessors sg sn
      preds_pn = predecessors pg pn
      succs_sn = successors sg sn
      succs_pn = successors pg pn
      t_out_sg = getNonMappedSuccsOfMappedNodes mapped_ns_sg sg
      t_out_pg = getNonMappedSuccsOfMappedNodes mapped_ns_pg pg
  in    length (succs_sn `intersect` t_out_sg)
        >= length (succs_pn `intersect` t_out_pg)
     && length (preds_sn `intersect` t_out_sg)
        >= length (preds_pn `intersect` t_out_pg)

-- | Not really sure what the intuition behind this check is (equation 7 in the
-- paper), other than that it is a 2-look-ahead check.

checkSyntaxNew :: Graph           -- ^ The search graph.
                  -> Graph        -- ^ The pattern graph.
                  -> NodeMatchset -- ^ Current matchset state.
                  -> NodeMapping  -- ^ Candidate mapping.
                  -> Bool
checkSyntaxNew sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatchset st
      preds_sn = predecessors sg sn
      preds_pn = predecessors pg pn
      succs_sn = successors sg sn
      succs_pn = successors pg pn
      new_ns_sg = getNonMappedNonAdjNodes mapped_ns_sg sg
      new_ns_pg = getNonMappedNonAdjNodes mapped_ns_pg pg
  in    length (new_ns_sg `intersect` preds_sn)
        >= length (new_ns_pg `intersect` preds_pn)
     && length (new_ns_sg `intersect` succs_sn)
        >= length (new_ns_pg `intersect` succs_pn)

-- | Splits a match into two node sets: the ones contained in the search graph,
-- and the ones contained in the pattern graph.

splitMatchset :: NodeMatchset -> ( [Node] -- ^ Matched nodes in the search
                                          -- graph.
                                 , [Node] -- ^ Matched nodes in the pattern
                                          -- graph.
                                 )
splitMatchset m =
  let nodes_sg = map fst m
      nodes_pg = map snd m
  in (nodes_sg, nodes_pg)

getNonMappedSuccsOfMappedNodes :: [Node]   -- ^ The already-mapped nodes.
                                  -> Graph -- ^ Original graph in which the
                                           -- mapped nodes appear.
                                  -> [Node]
getNonMappedSuccsOfMappedNodes ns g =
  filter (`notElem` ns) (concatMap (successors g) ns)

getNonMappedPredsOfMappedNodes :: [Node]   -- ^ The already-mapped nodes.
                                  -> Graph -- ^ Original graph in which the
                                           -- mapped nodes appear.
                                  -> [Node]
getNonMappedPredsOfMappedNodes ns g =
  filter (`notElem` ns) (concatMap (predecessors g) ns)

getNonMappedAdjsOfMappedNodes :: [Node]   -- ^ The already-mapped nodes.
                                 -> Graph -- ^ Original graph in which the
                                          -- mapped nodes appear.
                                 -> [Node]
getNonMappedAdjsOfMappedNodes ns g =
  (getNonMappedSuccsOfMappedNodes ns g) `union`
  (getNonMappedPredsOfMappedNodes ns g)

getNonMappedNonAdjNodes :: [Node]   -- ^ The already-mapped nodes.
                           -> Graph -- ^ Original graph in which the mapped
                                    -- nodes appear.
                           -> [Node]
getNonMappedNonAdjNodes ns g =
  ((allNodes g) \\ ns) \\ (getNonMappedAdjsOfMappedNodes ns g)

getMappedSuccsOfMappedNode :: Node      -- ^ Mapped node to get the successors
                                        -- from.
                              -> [Node] -- ^ The already-mapped nodes.
                              -> Graph  -- ^ Original graph in which the mapped
                                        -- nodes appear.
                              -> [Node]
getMappedSuccsOfMappedNode n ns g =
  filter (`elem` ns) (successors g n)

getMappedPredsOfMappedNode :: Node      -- ^ Mapped node to get the predecessors
                                        -- from.
                              -> [Node] -- ^ The already-mapped nodes.
                              -> Graph  -- ^ Original graph in which the mapped
                                        -- nodes appear.
                              -> [Node]
getMappedPredsOfMappedNode n ns g =
  filter (`elem` ns) (predecessors g n)

-- | Gets the corresponding search node from a mapped pattern graph node. It is
-- assumed that such a mapping exists in the matchset.

getMappedSNode :: NodeMatchset -- ^ Matchset.
                  -> Node      -- ^ Mapped node in the pattern graph.
                  -> Node      -- ^ Corresponding mapped node in the search
                               -- graph.
getMappedSNode st m =
  fst $ head $ filter (\(n', m') -> m' == m) st

-- | Gets the corresponding pattern node from a mapped search graph node. It is
-- assumed that such a mapping exists in the matchset.

getMappedPNode :: NodeMatchset -- ^ Matchset.
                  -> Node      -- ^ Mapped node in the search graph.
                  -> Node      -- ^ Corresponding mapped node in the pattern
                               -- graph.
getMappedPNode st n =
  snd $ head $ filter (\(n', m') -> n' == n) st
