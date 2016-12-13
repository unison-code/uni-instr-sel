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
  )
where

import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.PatternMatching.VF2

import qualified Data.Graph.Inductive as I

import Control.Arrow
  ( first )
import Data.List
  ( unfoldr
  , foldl'
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
