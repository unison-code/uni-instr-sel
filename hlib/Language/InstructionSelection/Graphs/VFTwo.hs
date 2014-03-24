-------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Graphs.VFTwo
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains an implementation of the VF2 subgraph isomorphism algorithm
-- (http://dx.doi.org/10.1109/TPAMI.2004.75). There are always two graphs
-- involved: the function graph and the pattern graph. The function graph is the
-- graph on which another graph will be matched. The graph to match is called
-- the pattern graph.
--
-- However, it seems that the paper has some bugs as it forbids matching of
-- certain subgraph isomorphism. Basically, if there is an edge in the function
-- graph between two nodes which does not appear in the pattern graph, then it
-- will not match. But we're only interested in matching all edges in the
-- pattern, not necessarily all edges in the function graph! I should contact
-- the authors about this and see whether there's a mistake.
--------------------------------------------------------------------------------

module Language.InstructionSelection.Graphs.VFTwo (
  match
) where

import Language.InstructionSelection.Graphs.Base
import Language.InstructionSelection.Utils (removeDuplicates)
import Data.List (intersect, union, (\\))



-------------
-- Functions
-------------

-- | Finds all instances where a pattern graph matches within a function graph.

match :: Graph              -- ^ The function graph.
         -> Graph           -- ^ The pattern graph.
         -> [Matchset Node] -- ^ Found matches.
match fg pg = match' fg pg (Matchset [])

-- | Implements the VF2 algorithm. The algorithm first finds a set of node
-- mapping candidates, and then applies a feasibility check on each of
-- them. Each candidate that passes the test is added to the existing mapping
-- state, and then the function is recursively called.

match' :: Graph              -- ^ The function graph.
          -> Graph           -- ^ The pattern graph.
          -> Matchset Node   -- ^ The current matchset state.
          -> [Matchset Node] -- ^ Found matches.
match' fg pg st
  | length (fromMatchset st) == numNodes pg = [st]
  | otherwise = let cs = getCandidates fg pg st
                    good_cs = filter (checkFeasibility fg pg st) cs
                in concatMap (match' fg pg) (map (addToMatchset st) good_cs)

-- | Gets a set of node mapping candidates. This set consists of the pairs of
-- nodes which are successors to the nodes currently in the match set. If this
-- resultant set is empty, then the set consists of the pairs of nodes which are
-- corresponding predecessors. If this set too is empty, then the returned set
-- consists of the pairs of nodes not contained in the match set.

getCandidates :: Graph             -- ^ The function graph.
                 -> Graph          -- ^ The pattern graph.
                 -> Matchset Node  -- ^ The current matchset state.
                 -> [Mapping Node] -- ^ Potential candidates.
getCandidates fg pg st =
  let m_fg = fNodes st
      m_pg = pNodes st
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

checkFeasibility :: Graph            -- ^ The function graph.
                    -> Graph         -- ^ The pattern graph.
                    -> Matchset Node -- ^ Current matchset state.
                    -> Mapping Node  -- ^ Candidate mapping.
                    -> Bool
checkFeasibility fg pg st c =
  matchingNodes fg pg st c && checkSyntax fg pg st c

-- | Checks that the syntax of matched nodes are compatible (modified version of
-- equation 2 in the paper).
--
-- The modification is that the last check is removed as it appear to forbid
-- certain cases of subgraph isomorphism which we still want.

checkSyntax :: Graph            -- ^ The function graph.
               -> Graph         -- ^ The pattern graph.
               -> Matchset Node -- ^ Current matchset state.
               -> Mapping Node  -- ^ Candidate mapping.
               -> Bool
checkSyntax fg pg st c =
      checkSyntaxPred fg pg st c
   && checkSyntaxSucc fg pg st c
   && checkSyntaxIn   fg pg st c
   && checkSyntaxOut  fg pg st c
-- && checkSyntaxNew  fg pg st c

-- | Checks that for each predecessor of the matched pattern node that appears
-- in the current matchset state, there exists a node mapping for the
-- predecessor (modified version of equation 3 in the paper). This is a
-- consistency check.
--
-- The modification is that in this implementation I have removed the equivalent
-- check for the matched function node; I believe this to be a bug in the paper.

checkSyntaxPred :: Graph            -- ^ The function graph.
                   -> Graph         -- ^ The pattern graph.
                   -> Matchset Node -- ^ Current matchset state.
                   -> Mapping Node  -- ^ Candidate mapping.
                   -> Bool
checkSyntaxPred fg pg st c =
  let m_fg = fNodes st
      m_pg = pNodes st
      preds_fn = predecessors fg (fNode c)
      preds_pn = predecessors pg (pNode c)
      preds_fn_in_m = preds_fn `intersect` m_fg
      preds_pn_in_m = preds_pn `intersect` m_pg
  in all (\pn -> any (\fn -> isInMatchset st (Mapping (fn, pn))) preds_fn)
         preds_pn_in_m

-- | Same as checkSyntaxPred but for the successors (modified version of
-- equation 4 in the paper).

checkSyntaxSucc :: Graph            -- ^ The function graph.
                   -> Graph         -- ^ The pattern graph.
                   -> Matchset Node -- ^ Current matchset state.
                   -> Mapping Node  -- ^ Candidate mapping.
                   -> Bool
checkSyntaxSucc fg pg st c =
  let m_fg = fNodes st
      m_pg = pNodes st
      succs_fn = successors fg (fNode c)
      succs_pn = successors pg (pNode c)
      succs_fn_in_m = succs_fn `intersect` m_fg
      succs_pn_in_m = succs_pn `intersect` m_pg
  in all (\pn -> any (\fn -> isInMatchset st (Mapping (fn, pn))) succs_fn)
     succs_pn_in_m

-- | Checks that there exists a sufficient number of unmapped predecessors left
-- in the function graph to cover the unmapped predecessors left in the pattern
-- graph (equation 5 in the paper). This is a 1-look-ahead check.

checkSyntaxIn :: Graph            -- ^ The function graph.
                 -> Graph         -- ^ The pattern graph.
                 -> Matchset Node -- ^ Current matchset state.
                 -> Mapping Node  -- ^ Candidate mapping.
                 -> Bool
checkSyntaxIn fg pg st c =
  let m_fg = fNodes st
      m_pg = pNodes st
      fn = fNode c
      pn = pNode c
      preds_fn = predecessors fg fn
      preds_pn = predecessors pg pn
      succs_fn = successors fg fn
      succs_pn = successors pg pn
      t_in_fg = getTInSet fg m_fg
      t_in_pg = getTInSet pg m_pg
  in    length (succs_fn `intersect` t_in_fg)
        >= length (succs_pn `intersect` t_in_pg)
     && length (preds_fn `intersect` t_in_fg)
        >= length (preds_pn `intersect` t_in_pg)

-- | Same as checkSyntaxIn but for successors (equation 6 in the paper).

checkSyntaxOut :: Graph            -- ^ The function graph.
                  -> Graph         -- ^ The pattern graph.
                  -> Matchset Node -- ^ Current matchset state.
                  -> Mapping Node  -- ^ Candidate mapping.
                  -> Bool
checkSyntaxOut fg pg st c =
  let m_fg = fNodes st
      m_pg = pNodes st
      fn = fNode c
      pn = pNode c
      preds_fn = predecessors fg fn
      preds_pn = predecessors pg pn
      succs_fn = successors fg fn
      succs_pn = successors pg pn
      t_out_fg = getTOutSet fg m_fg
      t_out_pg = getTOutSet pg m_pg
  in    length (succs_fn `intersect` t_out_fg)
        >= length (succs_pn `intersect` t_out_pg)
     && length (preds_fn `intersect` t_out_fg)
        >= length (preds_pn `intersect` t_out_pg)

-- | Not really sure what the intuition behind this check is (equation 7 in the
-- paper), other than that it is a 2-look-ahead check.

checkSyntaxNew :: Graph            -- ^ The function graph.
                  -> Graph         -- ^ The pattern graph.
                  -> Matchset Node -- ^ Current matchset state.
                  -> Mapping Node  -- ^ Candidate mapping.
                  -> Bool
checkSyntaxNew fg pg st c =
  let m_fg = fNodes st
      m_pg = pNodes st
      fn = fNode c
      pn = pNode c
      preds_fn = predecessors fg fn
      preds_pn = predecessors pg pn
      succs_fn = successors fg fn
      succs_pn = successors pg pn
      getNSet g ns = ((allNodes g) \\ ns)
                     \\ ((getTOutSet g ns) `union` (getTInSet g ns))
      n_fg = getNSet fg m_fg
      n_pg = getNSet pg m_pg
  in    length (n_fg `intersect` preds_fn) >= length (n_pg `intersect` preds_pn)
     && length (n_fg `intersect` succs_fn) >= length (n_pg `intersect` succs_pn)

getTOutSet :: Graph     -- ^ Graph in which the nodes of M belong.
              -> [Node] -- ^ The M set.
              -> [Node]
getTOutSet g ns =
  removeDuplicates $ filter (`notElem` ns) (concatMap (successors g) ns)

getTInSet :: Graph     -- ^ Graph in which the nodes of M belong.
             -> [Node] -- ^ The M set.
             -> [Node]
getTInSet g ns =
  removeDuplicates $ filter (`notElem` ns) (concatMap (predecessors g) ns)

getTDSet :: Graph     -- ^ Graph in which the nodes of M belong.
             -> [Node] -- ^ The M set.
             -> [Node]
getTDSet g ns = filter (`notElem` ns) (allNodes g)
