-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.PatternMatching.CP
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains routines for invoking constraint programming for doing pattern
-- matching. This is an alternative to the VF2 algorithm which does not support
-- multi-edges as well as only finds mappings that are subgraph isomorphisms,
-- whereas what we really are looking for are plain graph homomorphisms along
-- with an injective mapping for the edges.
--
-- This module invokes an external script which performs the actual pattern
-- matching, reads its output, and constructs a list of matches.
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstSel.Graphs.PatternMatching.CP
  ( findMatches )
where

import Language.InstSel.Graphs.Base
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import Data.List
  ( elemIndex
  , nub
  )
import Data.Maybe
  ( fromJust )

import Data.Maybe
  ( fromJust
  , isJust
  )
import qualified Data.Text as T
  ( unpack )


---------
-- Types
---------

-- | A data type used to conveniently pass data from one function to another.
data Parameters =
  Parameters
    { indexedPatternNodes :: [Node]
      -- ^ The nodes in the pattern graph, where each has been given a unique
      -- index (which is the element index).
    , actionPatternNodes :: [Int]
      -- ^ The indices of the pattern nodes that are action nodes.
    , indexedPatternEdges :: [(Int, Int)]
      -- ^ The edges in the pattern graph, where the first value is the index of
      -- the source node and the second value is the index of the target
      -- node. Each edge has also been given a unique index (which is the
      -- element index).
    , indexedFunctionNodes :: [Node]
      -- ^ The nodes in the function graph, where each has been given a unique
      -- index (which is the element index).
    , indexedFunctionEdges :: [(Int, Int)]
      -- ^ The edges in the function graph, where the first value is the index
      -- of the source node and the second value is the index of the target
      -- node. Each edge has also been given a unique index (which is the
      -- element index).
    , initialNodeDomains :: [[Int]]
      -- ^ The initial domains of the constraint variables for the pattern
      -- nodes. The domain consists of function node indices.
    , initialEdgeDomains :: [[Int]]
      -- ^ The initial domains of the constraint variables for the pattern
      -- edges. The domain consists of function edge indices.
    , alternativeEdges :: [[Int]]
      -- ^ List of set of edges that are considered to be alternatives, meaning
      -- at most one edge from each set may be matched by the pattern graph.
    }
  deriving (Show)

-- | A data type used to conveniently dump the data to a JSON file.
data JsonData =
  JsonData
    { jsonNumPatternNodes :: Int
    , jsonPatternEdges :: [(Int, Int)]
    , jsonNumFunctionNodes :: Int
    , jsonFunctionEdges :: [(Int, Int)]
    , jsonInitialNodeDomains :: [[Int]]
    , jsonInitialEdgeDomains :: [[Int]]
    , jsonAlternativeEdges :: [[Int]]
    }

instance ToJSON JsonData where
  toJSON p =
    object [ "num-pattern-nodes"    .= (jsonNumPatternNodes p)
           , "pattern-edges"        .= (jsonPatternEdges p)
           , "num-function-nodes"   .= (jsonNumFunctionNodes p)
           , "function-edges"       .= (jsonFunctionEdges p)
           , "initial-node-domains" .= (jsonInitialNodeDomains p)
           , "initial-edge-domains" .= (jsonInitialEdgeDomains p)
           , "alternative-edges"    .= (jsonAlternativeEdges p)
           ]



-------------
-- Functions
-------------

-- | Finds all occurrences where a pattern graph matches within a function
-- graph. A valid pattern graph match is a graph homomorphism from the pattern
-- graph to the function graph, with the additional constraint that the edge
-- mapping must be injective. Matches that are identical to one another will be
-- removed such that only one match will remain.
findMatches ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Match Node]
     -- ^ Found matches.
findMatches fg pg =
  let params = computeParameters fg pg
      matches = invokeMatcher params
  in nub matches

computeParameters ::
     Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> Parameters
     -- ^ The parameters needed for the pattern matcher.
computeParameters fg pg =
  let pg_node_index_maps = getAllNodes pg
      pg_edge_index_maps = getAllEdges pg
      pg_edges_as_indices =
        map
        ( \e ->
            ( fromJust $ (getSourceNode pg e) `elemIndex` pg_node_index_maps
            , fromJust $ (getTargetNode pg e) `elemIndex` pg_node_index_maps
            )
        )
        pg_edge_index_maps
      fg_node_index_maps = getAllNodes fg
      fg_edge_index_maps = getAllEdges fg
      fg_edges_as_indices =
        map
        ( \e ->
            ( fromJust $ (getSourceNode fg e) `elemIndex` fg_node_index_maps
            , fromJust $ (getTargetNode fg e) `elemIndex` fg_node_index_maps
            )
        )
        fg_edge_index_maps
      pn_init_doms =
        map
          ( \pn ->
              [ fi | (fi, fn) <- zip [0..] fg_node_index_maps
                   , doNodesMatch fg pg fn pn
              ]
          )
          pg_node_index_maps
      pg_op_nodes_indices =
        map
          fst
          (filter (isActionNode . snd) (zip [0..] pg_node_index_maps))
      pe_init_doms =
        map
          ( \pe ->
              [ fi | (fi, fe) <- zip [0..] fg_edge_index_maps
                   , doEdgeListsMatch fg pg [fe] [pe]
              ]
          )
          pg_edge_index_maps
      fg_alt_edges =
        map
          (map (\e -> fromJust $ e `elemIndex` fg_edge_index_maps))
          (findAlternativeEdges fg)
  in Parameters
       { indexedPatternNodes = pg_node_index_maps
       , actionPatternNodes = pg_op_nodes_indices
       , indexedPatternEdges = pg_edges_as_indices
       , indexedFunctionNodes = fg_node_index_maps
       , indexedFunctionEdges = fg_edges_as_indices
       , initialNodeDomains = pn_init_doms
       , initialEdgeDomains = pe_init_doms
       , alternativeEdges = fg_alt_edges
       }

-- | Finds and aggregates the edges that are alternatives to one another,
-- meaning at most one of them may be matched per pattern.
findAlternativeEdges ::
     Graph
  -> [[Edge]]
     -- ^ List of set of edges that are alternative.
findAlternativeEdges g =
  concatMap
    (\n -> findAlternativeInEdges g n ++ findAlternativeOutEdges g n)
    (getAllNodes g)

findAlternativeInEdges :: Graph -> Node -> [[Edge]]
findAlternativeInEdges g n =
  let areAlternatives e1 e2 =
        getEdgeType e1 == getEdgeType e2
        &&
        getInEdgeNr e1 == getInEdgeNr e2
  in groupEdges areAlternatives (getInEdges g n)

findAlternativeOutEdges :: Graph -> Node -> [[Edge]]
findAlternativeOutEdges g n =
  let areAlternatives e1 e2 =
        getEdgeType e1 == getEdgeType e2
        &&
        getOutEdgeNr e1 == getOutEdgeNr e2
  in groupEdges areAlternatives (getOutEdges g n)

-- | Groups edges according to a predicate function such that a set of edges,
-- for which the predicate holds for every edge pair in that set, are grouped
-- together. It is assumed that the predicate function is commutative and
-- associative.
groupEdges :: (Edge -> Edge -> Bool) -> [Edge] -> [[Edge]]
groupEdges f es =
  foldr (gr f) [] es
  where gr _ e [] = [[e]]
        gr f' e (p:ps) =
          if belongs f' e p then (e:p):ps else p:(gr f' e ps)
        belongs f'' e' es' = any (f'' e') es'

invokeMatcher :: Parameters -> [Match Node]
invokeMatcher p =
  -- TODO: implement
  []

toJsonData :: Parameters -> JsonData
toJsonData p =
  JsonData
    { jsonNumPatternNodes = length $ indexedPatternNodes p
    , jsonPatternEdges = indexedPatternEdges p
    , jsonNumFunctionNodes = length $ indexedFunctionNodes p
    , jsonFunctionEdges = indexedFunctionEdges p
    , jsonInitialNodeDomains = initialNodeDomains p
    , jsonInitialEdgeDomains = initialEdgeDomains p
    , jsonAlternativeEdges = alternativeEdges p
    }
