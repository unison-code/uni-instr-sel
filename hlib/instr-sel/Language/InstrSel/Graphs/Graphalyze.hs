--------------------------------------------------------------------------------
-- |
-- Copyright (c) 2008, Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Graphs.Graphalyze
  ( componentsOf
  , isReachableComponent
  )
where

import Language.InstrSel.Graphs.Base

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

-- | Tests whether there is a path in g from a node in c1 to a node in c2
isReachableComponent :: Graph -> Graph -> Graph -> Bool
isReachableComponent g c1 c2 =
    or [isReachable g n1 n2 | n1 <- getAllNodes c1, n2 <- getAllNodes c2,
                                    n1 /= n2]

-- | Tests whether there is a path in g from a node n1 to a node n2
isReachable :: Graph -> Node -> Node -> Bool
isReachable (Graph g) n1 n2 =
    let rns = I.reachable (getIntNodeID n1) g
    in getIntNodeID n2 `elem` rns
