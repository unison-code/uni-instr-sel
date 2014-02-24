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
  let (g_sg, g_pg) = splitMatch st
      t_out_sg = filter (`notElem` g_sg) (concatMap ((flip successors) sg) g_sg)
      t_out_pg = filter (`notElem` g_pg) (concatMap ((flip successors) pg) g_pg)
      pairs_out = [ (n, m) | n <- t_out_sg, m <- t_out_pg ]
      t_in_sg = filter (`notElem` g_sg) (concatMap ((flip predecessors) sg)
                                         g_sg)
      t_in_pg = filter (`notElem` g_pg) (concatMap ((flip predecessors) pg)
                                         g_pg)
      pairs_in = [ (n, m) | n <- t_in_sg, m <- t_in_pg ]
      t_d_sg = filter (`notElem` g_sg) (nodes sg)
      t_d_pg = filter (`notElem` g_pg) (nodes pg)
      pairs_d = [ (n, m) | n <- t_d_sg, m <- t_d_pg ]
  in if length pairs_out > 0
        then pairs_out
        else if length pairs_in > 0
             then pairs_in
             else pairs_d

-- | TODO: write description

checkFeasibility :: Graph          -- ^ The search graph.
                    -> Graph       -- ^ The pattern graph.
                    -> Match       -- ^ Current matching state.
                    -> NodeMapping -- ^ Candidate mapping.
                    -> Bool
checkFeasibility sg pg st (sn, pm) =
  -- TODO: implement
  False

-- | Splits a match into two node sets: the ones contained in the search graph,
-- and the ones contained in the pattern graph.

splitMatch :: Match -> ( [Node] -- ^ Nodes in the search graph.
                       , [Node] -- ^ Nodes in the pattern graph.
                       )
splitMatch m =
  let nodes_sg = map fst m
      nodes_pg = map snd m
  in (nodes_sg, nodes_pg)
