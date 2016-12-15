{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se

Functions copied from the Graphalyze package.
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Graphs.Graphalyze
  ( componentsOf
  , isReachableComponent
  , areGraphsIsomorphic
  , findCycles
  )
where

import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.PatternMatching.VF2

import qualified Data.Graph.Inductive as I

import Control.Arrow
  ( first )
import Data.Function
  ( on )
import Data.List
  ( unfoldr
  , foldl'
  , nubBy
  )



-------------
-- Functions
-------------

-- | Gets the internal node ID from a node.
getIntNodeID :: Node -> I.Node
getIntNodeID (Node (nid, _)) = nid

-- | Gives the connected components of the given node.
componentsOf :: Graph -> [Graph]
componentsOf (Graph g) =
    let gs = componentsOf' g
    in map Graph gs

-- | Find all connected components of a graph.
componentsOf' :: (I.DynGraph g) => g a b -> [g a b]
componentsOf' = unfoldr splitComponent

-- | Find the next component and split it off from the graph.
splitComponent :: (I.DynGraph g) => g a b -> Maybe (g a b, g a b)
splitComponent g
    | I.isEmpty g = Nothing
    | otherwise = Just .            -- Get the type right
                  first I.buildGr . -- Create the subgraph
                  extractNode .     -- Extract components of subgraph
                  first Just .      -- Getting the types right
                  I.matchAny $ g    -- Choose an arbitrary node to begin with

-- | Extract the given node and all nodes it is transitively
--   connected to from the graph.
extractNode :: (I.DynGraph g) => I.Decomp g a b -> ([I.Context a b], g a b)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | I.isEmpty gr = ([ctxt], I.empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
    nbrs = I.neighbors' ctxt

-- | Helper function for 'extractNode' above.
nodeExtractor :: (I.DynGraph g) => ([I.Context a b], g a b) -> I.Node
              -> ([I.Context a b], g a b)
nodeExtractor cg@(cs,g) n
    | I.gelem n g = first (++ cs) . extractNode $ I.match n g
    | otherwise = cg

-- | Tests whether there is a path in the graph @g@ from a node in component
-- @c1@ to a node in component @c2@.
isReachableComponent
  :: Graph
     -- ^ Graph @g@.
  -> Graph
     -- ^ Component @c1@.
  -> Graph
     -- ^ Component @c2@.
  -> Bool
isReachableComponent g c1 c2 =
  or [isReachable g n1 n2 | n1 <- getAllNodes c1, n2 <- getAllNodes c2,
                                  n1 /= n2]
  where isReachable (Graph g') n1 n2 =
          let rns = I.reachable (getIntNodeID n1) g'
          in getIntNodeID n2 `elem` rns

-- | Tests if two graphs are isomorphic to each other.
areGraphsIsomorphic :: Graph -> Graph -> Bool
areGraphsIsomorphic g1 g2 = length (findMatches g1 g2) == 1

-- | Find all cycles in the given graph, returning just the nodes.
findCycles :: (I.DynGraph g) => g a b -> [[I.Node]]
findCycles = concat . unfoldr findCycles' . mkSimple

findCycles' :: (I.DynGraph g) => g a b -> Maybe ([[I.Node]], g a b)
findCycles' g
  | I.isEmpty g = Nothing
  | otherwise = Just . getCycles . I.matchAny $ g
  where
  getCycles (ctx,g') = (cyclesFor (ctx, g'), g')

-- | Find all cycles for the given node.
cyclesFor :: (I.DynGraph g) => I.GDecomp g a b -> [[I.Node]]
cyclesFor = map init .
            filter isCycle .
            pathTree .
            first Just
  where
  isCycle p = not (single p) && (head p == last p)

-- | Return true if and only if the list contains a single element.
single :: [a] -> Bool
single [_] = True
single  _  = False

-- | Find all possible paths from this given node, avoiding loops,
--   cycles, etc.
pathTree :: (I.DynGraph g) => I.Decomp g a b -> [[I.Node]]
pathTree (Nothing,_) = []
pathTree (Just ct,g)
  | I.isEmpty g = []
  | null sucs = [[n]]
  | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
  where
  n = I.node' ct
  sucs = I.suc' ct
  -- Avoid infinite loops by not letting it continue any further
  ct' = makeLeaf ct
  g' = ct' I.& g
  subPathTree gr n' = pathTree $ I.match n' gr

-- | Remove all outgoing edges
makeLeaf :: I.Context a b -> I.Context a b
makeLeaf (p,n,a,_) = (p', n, a, [])
  where
  -- Ensure there isn't an edge (n,n)
  p' = filter (\(_,n') -> n' /= n) p

-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (I.DynGraph gr) => gr a b -> gr a b
mkSimple = I.gmap simplify
    where
      rmLoops n = filter ((/=) n . snd)
      rmDups = nubBy ((==) `on` snd)
      simpleEdges n = rmDups . rmLoops n
      simplify (p,n,l,s) = (p',n,l,s')
          where
            p' = simpleEdges n p
            s' = simpleEdges n s
