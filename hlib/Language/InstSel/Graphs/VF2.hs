-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.VF2
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains an implementation of the VF2 subgraph isomorphism algorithm
-- (http://dx.doi.org/10.1109/TPAMI.2004.75). There are always two graphs
-- involved: the function graph and the pattern graph. The function graph is the
-- graph on which another graph will be matched. The graph to match is called
-- the pattern graph.
--
-- The VF2 algorithm assumes that neither graph contains multi-edges (that is,
-- more than one edges between the same pair of nodes). This limitations,
-- however, can be circumvented by duplicating nodes and giving them the same
-- node IDs before invoking the algorithm.
--
-- It seems that the paper has some bugs as it forbids matching of certain
-- subgraph isomorphism. Basically, if there is an edge in the function graph
-- between two nodes which does not appear in the pattern graph, then it will
-- not match. But we're only interested in matching all edges in the pattern,
-- not necessarily all edges in the function graph! I should contact the authors
-- about this and see whether there's a mistake.
--
-- TODO: implement routine for undoing multi-edges
-- TODO: handle function graphs with multiple edges that have the same edge
-- numbers
--------------------------------------------------------------------------------

module Language.InstSel.Graphs.VF2
  ( findMatches )
where

import Language.InstSel.Graphs.Base
import Data.List
  ( intersect
  , nub
  )



-------------
-- Functions
-------------

-- | Finds all instances where a pattern graph matches within a function graph.
findMatches ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Match Node]
     -- ^ Found matches.
findMatches fg pg =
  let fg' = breakMultiEdges fg
      pg' = breakMultiEdges pg
  in map Match (match fg' pg' [])

-- | Implements the VF2 algorithm. The algorithm first finds a set of node
-- mapping candidates, and then applies a feasibility check on each of
-- them. Each candidate that passes the test is added to the existing mapping
-- state, and then the function is recursively called.
match ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ The current mapping state.
  -> [[Mapping Node]]
     -- ^ Found matches.
match fg pg st
  | length st == getNumNodes pg = [st]
  | otherwise = let cs = getCandidates fg pg st
                    good_cs = filter (checkFeasibility fg pg st) cs
                in concatMap (match fg pg) (map (:st) good_cs)

-- | Breaks multi-edges by duplicating the source node such that they share the
-- same node ID.
--
-- TODO: implement
breakMultiEdges :: Graph -> Graph
breakMultiEdges g = g

-- | Gets a set of node mapping candidates. This set consists of the pairs of
-- nodes which are successors to the nodes currently in the match set. If this
-- resultant set is empty, then the set consists of the pairs of nodes which are
-- corresponding predecessors. If this set too is empty, then the returned set
-- consists of the pairs of nodes not contained in the match set.
getCandidates ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ The current mapping state.
  -> [Mapping Node]
     -- ^ Potential candidates.
getCandidates fg pg st =
  let toMapping (fn, pn) = Mapping { fNode = fn, pNode = pn }
      m_fg = map fNode st
      m_pg = map pNode st
      t_out_fg = getTOutSet fg m_fg
      t_out_pg = getTOutSet pg m_pg
      t_in_fg = getTInSet fg m_fg
      t_in_pg = getTInSet pg m_pg
      t_d_fg = getTDSet fg m_fg
      t_d_pg = getTDSet pg m_pg
      p_out = [ (n, head t_out_pg) | n <- t_out_fg, not (null t_out_pg) ]
      p_in  = [ (n, head t_in_pg) | n <- t_in_fg, not (null t_in_pg) ]
      p_d   = [ (n, head t_d_pg) | n <- t_d_fg, not (null t_d_pg) ]
  in if length p_out > 0
     then map toMapping p_out
     else if length p_in > 0
          then map toMapping p_in
          else map toMapping p_d

-- | Checks that the node mapping is feasible by comparing their syntax and
-- semantics.
checkFeasibility ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkFeasibility fg pg st c =
  let fn = fNode c
      pn = pNode c
      preds_pn = filter (`elem` (map pNode st)) (getPredecessors pg pn)
      preds_fn = findFNsInMatch (Match st) preds_pn
      pred_es_pairs = zip
                      (map (flip (getEdges fg) fn) preds_fn)
                      (map (flip (getEdges pg) pn) preds_pn)
      succs_pn = filter (`elem` (map pNode st)) (getSuccessors pg pn)
      succs_fn = findFNsInMatch (Match st) succs_pn
      succ_es_pairs = zip
                      (map (getEdges fg fn) succs_fn)
                      (map (getEdges pg pn) succs_pn)
  in doNodesMatch fg pg (fNode c) (pNode c)
     &&
     all (\(fes, pes) -> doEdgesMatch fg pg fes pes) pred_es_pairs
     &&
     all (\(fes, pes) -> doEdgesMatch fg pg fes pes) succ_es_pairs
     &&
     checkSyntax fg pg st c

-- | Checks that the syntax of matched nodes are compatible (modified version of
-- equation 2 in the paper).
--
-- The modification is that the last check is removed as it appear to forbid
-- certain cases of subgraph isomorphism which we still want.
checkSyntax ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkSyntax fg pg st c =
  checkSyntaxPred fg pg st c
  &&
  checkSyntaxSucc fg pg st c
  &&
  checkSyntaxIn   fg pg st c
  &&
  checkSyntaxOut  fg pg st c

-- | Checks that for each predecessor of the matched pattern node that appears
-- in the current mapping state, there exists a node mapping for the
-- predecessor (modified version of equation 3 in the paper). This is a
-- consistency check.
--
-- The modification is that in this implementation I have removed the equivalent
-- check for the matched function node; I believe this to be a bug in the paper.
checkSyntaxPred ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkSyntaxPred fg pg st c =
  let m_pg = map pNode st
      preds_fn = getPredecessors fg (fNode c)
      preds_pn = getPredecessors pg (pNode c)
      preds_pn_in_m = preds_pn `intersect` m_pg
  in all
     ( \pn -> any
              (\fn -> (Mapping { fNode = fn, pNode = pn }) `elem` st)
              preds_fn
     )
     preds_pn_in_m

-- | Same as checkSyntaxPred but for the successors (modified version of
-- equation 4 in the paper).
checkSyntaxSucc ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkSyntaxSucc fg pg st c =
  let m_pg = map pNode st
      succs_fn = getSuccessors fg (fNode c)
      succs_pn = getSuccessors pg (pNode c)
      succs_pn_in_m = succs_pn `intersect` m_pg
  in all
     ( \pn -> any
              (\fn -> (Mapping { fNode = fn, pNode = pn }) `elem` st)
              succs_fn
     )
     succs_pn_in_m

-- | Checks that there exists a sufficient number of unmapped predecessors left
-- in the function graph to cover the unmapped predecessors left in the pattern
-- graph (equation 5 in the paper). This is a 1-look-ahead check.
checkSyntaxIn ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkSyntaxIn fg pg st c =
  let m_fg = map fNode st
      m_pg = map pNode st
      fn = fNode c
      pn = pNode c
      preds_fn = getPredecessors fg fn
      preds_pn = getPredecessors pg pn
      succs_fn = getSuccessors fg fn
      succs_pn = getSuccessors pg pn
      t_in_fg = getTInSet fg m_fg
      t_in_pg = getTInSet pg m_pg
  in ( length (succs_fn `intersect` t_in_fg)
       >=
       length (succs_pn `intersect` t_in_pg)
     )
     &&
     ( length (preds_fn `intersect` t_in_fg)
       >=
       length (preds_pn `intersect` t_in_pg)
     )

-- | Same as checkSyntaxIn but for successors (equation 6 in the paper).
checkSyntaxOut ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Mapping Node]
     -- ^ Current mapping state.
  -> Mapping Node
     -- ^ Candidate mapping.
  -> Bool
checkSyntaxOut fg pg st c =
  let m_fg = map fNode st
      m_pg = map pNode st
      fn = fNode c
      pn = pNode c
      preds_fn = getPredecessors fg fn
      preds_pn = getPredecessors pg pn
      succs_fn = getSuccessors fg fn
      succs_pn = getSuccessors pg pn
      t_out_fg = getTOutSet fg m_fg
      t_out_pg = getTOutSet pg m_pg
  in ( length (succs_fn `intersect` t_out_fg)
       >=
       length (succs_pn `intersect` t_out_pg)
     )
     &&
     ( length (preds_fn `intersect` t_out_fg)
       >=
       length (preds_pn `intersect` t_out_pg)
     )

getTOutSet ::
     Graph
     -- ^ Graph in which the nodes of M belong.
  -> [Node]
     -- ^ The M set.
  -> [Node]
getTOutSet g ns =
  nub $ filter (`notElem` ns) (concatMap (getSuccessors g) ns)

getTInSet ::
     Graph
     -- ^ Graph in which the nodes of M belong.
  -> [Node]
     -- ^ The M set.
  -> [Node]
getTInSet g ns =
  nub $ filter (`notElem` ns) (concatMap (getPredecessors g) ns)

getTDSet ::
     Graph
     -- ^ Graph in which the nodes of M belong.
  -> [Node]
     -- ^ The M set.
  -> [Node]
getTDSet g ns = filter (`notElem` ns) (getAllNodes g)
