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
  Match
, NodeMapping
, match
) where

import Language.InstructionSelection.Graphs.Base
import Data.List (intersect, union, (\\))



--------------
-- Data types
--------------

type Match = [NodeMapping]
type NodeMapping = ( Node -- ^ Node in search graph.
                   , Node -- ^ Node in pattern graph.
                   )


-------------
-- Functions
-------------

match :: Graph      -- ^ The search graph.
         -> Graph   -- ^ The pattern graph.
         -> [Match] -- ^ Found matches.
match sg pg = match' sg pg []

-- | Implements the VF2 algorithm. The algorithm first finds a set of node
-- mapping candidates, and then applies a feasibility check on each of
-- them. Each candidate that passes the test is added to the existing mapping
-- state, and then the function is recursively called.

match' :: Graph      -- ^ The search graph.
          -> Graph   -- ^ The pattern graph.
          -> Match   -- ^ The current matching state.
          -> [Match] -- ^ Found matches.
match' sg pg st =
  if length st == numNodes pg
     then [st]
     else let candidates = getCandidates sg pg st
              feasible_candidates = filter (checkFeasibility sg pg st)
                                    candidates
              new_states = map (:st) feasible_candidates
          in concatMap (match' sg pg) new_states

-- | Gets a set of node mapping candidates. This set consists of the pairs of
-- nodes which are successors to the nodes currently in the matching set. If
-- this resultant set is empty, then the set consists of the pairs of nodes
-- which are corresponding predecessors. If this set too is empty, then the
-- returned set consists of the pairs of nodes not contained in the matching
-- set.

getCandidates :: Graph    -- ^ The search graph.
                 -> Graph -- ^ The pattern graph.
                 -> Match -- ^ The current matching state.
                 -> Match -- ^ Potential candidates.
getCandidates sg pg st =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      t_out_sg = getNonMappedSuccsOfMappedNodes mapped_ns_sg sg
      t_out_pg = getNonMappedSuccsOfMappedNodes mapped_ns_pg pg
      pairs_out = [ (n, m) | n <- t_out_sg, m <- t_out_pg ]
      t_in_sg = getNonMappedPredsOfMappedNodes mapped_ns_sg sg
      t_in_pg = getNonMappedPredsOfMappedNodes mapped_ns_pg pg
      pairs_in = [ (n, m) | n <- t_in_sg, m <- t_in_pg ]
      t_d_sg = filter (`notElem` mapped_ns_sg) (nodes sg)
      t_d_pg = filter (`notElem` mapped_ns_pg) (nodes pg)
      pairs_d = [ (n, head t_d_pg) | n <- t_d_sg ]
  in if length pairs_out > 0
        then pairs_out
        else if length pairs_in > 0
             then pairs_in
             else pairs_d

-- | Checks that the node mapping is feasible by comparing their syntax and
-- semantics.

checkFeasibility :: Graph          -- ^ The search graph.
                    -> Graph       -- ^ The pattern graph.
                    -> Match       -- ^ Current matching state.
                    -> NodeMapping -- ^ Candidate mapping.
                    -> Bool
checkFeasibility sg pg st pair =
  (checkSemantics sg pg st pair) && (checkSyntax sg pg st pair)

-- | Checks that the nodes are of the same type and that the edges are
-- compatible.

checkSemantics :: Graph          -- ^ The search graph.
                  -> Graph       -- ^ The pattern graph.
                  -> Match       -- ^ Current matching state.
                  -> NodeMapping -- ^ Candidate mapping.
                  -> Bool
checkSemantics sg pg st (n, m) =
  (nodeType n) == (nodeType m) && (checkEdges sg pg st (n, m))

-- | TODO: write description

checkEdges :: Graph          -- ^ The search graph.
              -> Graph       -- ^ The pattern graph.
              -> Match       -- ^ Current matching state.
              -> NodeMapping -- ^ Candidate mapping.
              -> Bool
checkEdges sg pg st (n, m) =
  -- TODO: implement
  True

-- | Checks that the syntax of matched nodes are compatible (equation 2 in the
-- paper).

checkSyntax :: Graph          -- ^ The search graph.
               -> Graph       -- ^ The pattern graph.
               -> Match       -- ^ Current matching state.
               -> NodeMapping -- ^ Candidate mapping.
               -> Bool
checkSyntax sg pg st pair =
     (checkSyntaxPred sg pg st pair)
  && (checkSyntaxSucc sg pg st pair)
  && (checkSyntaxIn sg pg st pair)
  && (checkSyntaxOut sg pg st pair)
  && (checkSyntaxNew sg pg st pair)

-- | Checks that for each predecessor A of the matched node that appears in the
-- current matching state, there also exists some node mapping for A (equation 3
-- in the paper). This is a consistency check.

checkSyntaxPred :: Graph          -- ^ The search graph.
                   -> Graph       -- ^ The pattern graph.
                   -> Match       -- ^ Current matching state.
                   -> NodeMapping -- ^ Candidate mapping.
                   -> Bool
checkSyntaxPred sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      preds_sn = predecessors sn sg
      preds_pn = predecessors pn pg
      preds_sn_in_st = preds_sn `intersect` mapped_ns_sg
      preds_pn_in_st = preds_pn `intersect` mapped_ns_pg
  in    all (\n -> any (\m -> (n, m) `elem` st) preds_pn) preds_sn_in_st
     && all (\m -> any (\n -> (n, m) `elem` st) preds_sn) preds_pn_in_st

-- | Same as checkSyntaxPred but for the successors (equation 4 in the paper).

checkSyntaxSucc :: Graph          -- ^ The search graph.
                   -> Graph       -- ^ The pattern graph.
                   -> Match       -- ^ Current matching state.
                   -> NodeMapping -- ^ Candidate mapping.
                   -> Bool
checkSyntaxSucc sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      succs_sn = successors sn sg
      succs_pn = successors pn pg
      succs_sn_in_st = succs_sn `intersect` mapped_ns_sg
      succs_pn_in_st = succs_pn `intersect` mapped_ns_pg
  in    all (\n -> any (\m -> (n, m) `elem` st) succs_pn) succs_sn_in_st
     && all (\m -> any (\n -> (n, m) `elem` st) succs_sn) succs_pn_in_st

-- | Checks that there exists a sufficient number of predecessors to map in the
-- search graph (equation 5 in the paper). This is a 1-look-ahead check.

checkSyntaxIn :: Graph          -- ^ The search graph.
                 -> Graph       -- ^ The pattern graph.
                 -> Match       -- ^ Current matching state.
                 -> NodeMapping -- ^ Candidate mapping.
                 -> Bool
checkSyntaxIn sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      preds_sn = predecessors sn sg
      preds_pn = predecessors pn pg
      succs_sn = successors sn sg
      succs_pn = successors pn pg
      t_in_sg = getNonMappedPredsOfMappedNodes mapped_ns_sg sg
      t_in_pg = getNonMappedPredsOfMappedNodes mapped_ns_pg pg
  in    length (succs_sn `intersect` t_in_sg)
        >= length (succs_pn `intersect` t_in_pg)
     && length (preds_sn `intersect` t_in_sg)
        >= length (preds_pn `intersect` t_in_pg)

-- | Same as checkSyntaxIn but for successors (equation 6 in the paper).

checkSyntaxOut :: Graph          -- ^ The search graph.
                  -> Graph       -- ^ The pattern graph.
                  -> Match       -- ^ Current matching state.
                  -> NodeMapping -- ^ Candidate mapping.
                  -> Bool
checkSyntaxOut sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      preds_sn = predecessors sn sg
      preds_pn = predecessors pn pg
      succs_sn = successors sn sg
      succs_pn = successors pn pg
      t_out_sg = getNonMappedSuccsOfMappedNodes mapped_ns_sg sg
      t_out_pg = getNonMappedSuccsOfMappedNodes mapped_ns_pg pg
  in    length (succs_sn `intersect` t_out_sg)
        >= length (succs_pn `intersect` t_out_pg)
     && length (preds_sn `intersect` t_out_sg)
        >= length (preds_pn `intersect` t_out_pg)

-- | Not really sure what the intuition behind this check is (equation 7 in the
-- paper), other than that it is a 2-look-ahead check.

checkSyntaxNew :: Graph          -- ^ The search graph.
                  -> Graph       -- ^ The pattern graph.
                  -> Match       -- ^ Current matching state.
                  -> NodeMapping -- ^ Candidate mapping.
                  -> Bool
checkSyntaxNew sg pg st (sn, pn) =
  let (mapped_ns_sg, mapped_ns_pg) = splitMatch st
      preds_sn = predecessors sn sg
      preds_pn = predecessors pn pg
      succs_sn = successors sn sg
      succs_pn = successors pn pg
      new_ns_sg = getNonMappedNonAdjNodes mapped_ns_sg sg
      new_ns_pg = getNonMappedNonAdjNodes mapped_ns_pg pg
  in    length (new_ns_sg `intersect` preds_sn)
        >= length (new_ns_pg `intersect` preds_pn)
     && length (new_ns_sg `intersect` succs_sn)
        >= length (new_ns_pg `intersect` succs_pn)

-- | Splits a match into two node sets: the ones contained in the search graph,
-- and the ones contained in the pattern graph.

splitMatch :: Match -> ( [Node] -- ^ Matched nodes in the search graph.
                       , [Node] -- ^ Matched nodes in the pattern graph.
                       )
splitMatch m =
  let nodes_sg = map fst m
      nodes_pg = map snd m
  in (nodes_sg, nodes_pg)

getNonMappedSuccsOfMappedNodes :: [Node]   -- ^ The already-mapped nodes.
                                  -> Graph -- ^ Original graph in which the
                                           -- mapped nodes appear.
                                  -> [Node]
getNonMappedSuccsOfMappedNodes ns g =
  filter (`notElem` ns) (concatMap ((flip successors) g) ns)

getNonMappedPredsOfMappedNodes :: [Node]   -- ^ The already-mapped nodes.
                                  -> Graph -- ^ Original graph in which the
                                           -- mapped nodes appear.
                                  -> [Node]
getNonMappedPredsOfMappedNodes ns g =
  filter (`notElem` ns) (concatMap ((flip predecessors) g) ns)

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
  ((nodes g) \\ ns) \\ (getNonMappedAdjsOfMappedNodes ns g)
