-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs.PatternMatching.CP
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
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
-- matching, reads its output, and constructs a list of matches. The script to
-- execute is expected to be of certain name and located in the working
-- directory (see the `queryScriptPath` function).
--------------------------------------------------------------------------------

-- Needed for JSON package
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- Needed for Shelly package
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.InstrSel.Graphs.PatternMatching.CP
  ( findMatches )
where

import Language.InstrSel.Graphs.Base
import Language.InstrSel.Utils
  ( groupBy )

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import qualified Data.Text as T

import Shelly
import System.IO.Unsafe
  ( unsafePerformIO )

import Control.Monad
  ( mzero )
import Data.List
  ( elemIndex
  , nub
  )
import Data.Maybe
  ( fromJust
  , isJust
  )
import Prelude
  hiding ( FilePath )



--------------
-- Data types
--------------

-- | A data type used to conveniently pass data from one function to another.
data Parameters
  = Parameters
      { indexedPatternNodes :: [Node]
        -- ^ The nodes in the pattern graph, where each has been given a unique
        -- index (which is the element index).
      , patternOperationNodes :: [Int]
        -- ^ The indices of the pattern nodes that are operations.
      , indexedPatternEdges :: [(Int, Int)]
        -- ^ The edges in the pattern graph, where the first value is the index
        -- of the source node and the second value is the index of the target
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
        -- ^ List of set of edges that are considered to be alternatives,
        -- meaning at most one edge from each set may be matched by the pattern
        -- graph.
      }
  deriving (Show)

-- | A data type used to conveniently dump parameter data to a JSON file.
data JsonParamData
  = JsonParamData
      { jsonNumPatternNodes :: Int
      , jsonPatternEdges :: [(Int, Int)]
      , jsonNumFunctionNodes :: Int
      , jsonFunctionEdges :: [(Int, Int)]
      , jsonInitialNodeDomains :: [[Int]]
      , jsonInitialEdgeDomains :: [[Int]]
      , jsonAlternativeEdges :: [[Int]]
      }

data SolutionData =
  SolutionData
    { nodeMaps :: [[Int]]
      -- ^ Found node maps. The index of the pattern node is the element index
      -- to the inner list, where each such element is the index of a function
      -- node.
    }



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance ToJSON JsonParamData where
  toJSON p =
    object [ "num-pattern-nodes"    .= (jsonNumPatternNodes p)
           , "pattern-edges"        .= (jsonPatternEdges p)
           , "num-function-nodes"   .= (jsonNumFunctionNodes p)
           , "function-edges"       .= (jsonFunctionEdges p)
           , "initial-node-domains" .= (jsonInitialNodeDomains p)
           , "initial-edge-domains" .= (jsonInitialEdgeDomains p)
           , "alternative-edges"    .= (jsonAlternativeEdges p)
           ]

instance FromJSON SolutionData where
  parseJSON (Object v) =
    SolutionData
      <$> v .: "pat-to-func-node-maps"
  parseJSON _ = mzero



-------------
-- Functions
-------------

-- | Finds all occurrences where a pattern graph matches within a function
-- graph. A valid pattern graph match is a graph homomorphism from the pattern
-- graph to the function graph, with the additional constraint that the edge
-- mapping must be injective. Matches that are identical to one another will be
-- removed such that only one match will remain.
{-# NOINLINE findMatches #-}
findMatches
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> [Match Node]
     -- ^ Found matches.
findMatches fg pg =
  let params = computeParameters fg pg
  in if not $ hasParamsEmptyDomains params
     then let sol = unsafePerformIO (invokeMatcher params)
              matches = makeMatchesFromSolutionData params sol
          in nub matches
     else []

computeParameters
  :: Graph
     -- ^ The function graph.
  -> Graph
     -- ^ The pattern graph.
  -> Parameters
     -- ^ The parameters needed for the pattern matcher.
computeParameters fg pg =
  let pg_node_index_maps = getAllNodes pg
      pg_edge_index_maps = getAllEdges pg
      pg_edges_as_indices =
        map ( \e ->
              ( fromJust $ (getSourceNode pg e) `elemIndex` pg_node_index_maps
              , fromJust $ (getTargetNode pg e) `elemIndex` pg_node_index_maps
              )
            )
            pg_edge_index_maps
      fg_node_index_maps = getAllNodes fg
      fg_edge_index_maps = getAllEdges fg
      fg_edges_as_indices =
        map ( \e ->
              ( fromJust $ (getSourceNode fg e) `elemIndex` fg_node_index_maps
              , fromJust $ (getTargetNode fg e) `elemIndex` fg_node_index_maps
              )
            )
            fg_edge_index_maps
      pn_init_doms =
        map ( \pn ->
              [ fi | (fi, fn) <- zip [0..] fg_node_index_maps
                   , doNodesMatch fg pg fn pn
              ]
            )
            pg_node_index_maps
      pg_ops_indices =
        map fst
            (filter (isNodeAnOperation . snd) (zip [0..] pg_node_index_maps))
      pe_init_doms =
        map ( \pe ->
              [ fi | (fi, fe) <- zip [0..] fg_edge_index_maps
                   , doEdgeListsMatch fg pg [fe] [pe]
              ]
            )
            pg_edge_index_maps
      fg_alt_edges =
        map (map (\e -> fromJust $ e `elemIndex` fg_edge_index_maps))
            (findAlternativeEdges fg)
  in Parameters { indexedPatternNodes = pg_node_index_maps
                , patternOperationNodes = pg_ops_indices
                , indexedPatternEdges = pg_edges_as_indices
                , indexedFunctionNodes = fg_node_index_maps
                , indexedFunctionEdges = fg_edges_as_indices
                , initialNodeDomains = pn_init_doms
                , initialEdgeDomains = pe_init_doms
                , alternativeEdges = fg_alt_edges
                }

-- | Checks if the given parameters has any field that is an empty domain.
hasParamsEmptyDomains :: Parameters -> Bool
hasParamsEmptyDomains p =
  let domains = [ initialNodeDomains p
                , initialEdgeDomains p
                ]
  in any null domains

-- | Finds and aggregates the edges that are alternatives to one another,
-- meaning at most one of them may be matched per pattern.
findAlternativeEdges
  :: Graph
  -> [[Edge]]
     -- ^ List of set of edges that are alternative.
findAlternativeEdges g =
  concatMap (\n -> findAlternativeInEdges g n ++ findAlternativeOutEdges g n)
            (getAllNodes g)

findAlternativeInEdges :: Graph -> Node -> [[Edge]]
findAlternativeInEdges g n =
  let areAlternatives e1 e2 =
        getEdgeType e1 == getEdgeType e2
        &&
        getInEdgeNr e1 == getInEdgeNr e2
      groups = groupBy areAlternatives (getInEdges g n)
  in filter (\p -> length p > 1) groups

findAlternativeOutEdges :: Graph -> Node -> [[Edge]]
findAlternativeOutEdges g n =
  let areAlternatives e1 e2 =
        getEdgeType e1 == getEdgeType e2
        &&
        getOutEdgeNr e1 == getOutEdgeNr e2
      groups = groupBy areAlternatives (getOutEdges g n)
  in filter (\p -> length p > 1) groups

invokeMatcher :: Parameters -> IO SolutionData
invokeMatcher p = shelly (withTmpDir $ invokeMatcherShell p)

invokeMatcherShell
  :: Parameters
     -- ^ Data for the pattern matcher.
  -> FilePath
     -- ^ Path to the directory wherein intermediate files can be stored. The
     -- path will be destroyed afterwards.
  -> Sh SolutionData
invokeMatcherShell p dir =
  do json_input_file <- queryJsonInputFilePath dir
     json_output_file <- queryJsonOutputFilePath dir
     dumpParamsToJsonFile p json_input_file
     abs_json_input_file <- absPath json_input_file
     abs_json_output_file <- absPath json_output_file
     script <- queryScriptPath
     print_stdout
       False
       ( run_ script [ "-o"
                     , toTextIgnore abs_json_output_file
                     , toTextIgnore abs_json_input_file
                     ]
       )
     result <- readfile json_output_file
     readSolutionData result

toJsonParamData :: Parameters -> JsonParamData
toJsonParamData p =
  JsonParamData
    { jsonNumPatternNodes = length $ indexedPatternNodes p
    , jsonPatternEdges = indexedPatternEdges p
    , jsonNumFunctionNodes = length $ indexedFunctionNodes p
    , jsonFunctionEdges = indexedFunctionEdges p
    , jsonInitialNodeDomains = initialNodeDomains p
    , jsonInitialEdgeDomains = initialEdgeDomains p
    , jsonAlternativeEdges = alternativeEdges p
    }

queryJsonInputFilePath
  :: FilePath
     -- ^ Path to directory wherein the input file will be available.
  -> Sh FilePath
queryJsonInputFilePath dir =
  return $ dir </> "pat-match-data.json"

queryJsonOutputFilePath
  :: FilePath
     -- ^ Path to directory wherein the output file will be available.
  -> Sh FilePath
queryJsonOutputFilePath dir =
  return $ dir </> "solutions.json"

queryScriptPath :: Sh FilePath
queryScriptPath =
  do return "./pattern-matcher"

dumpParamsToJsonFile :: Parameters -> FilePath -> Sh ()
dumpParamsToJsonFile p file =
  do let json_str = T.pack $ BS.unpack $ encode $ toJsonParamData p
     writefile file json_str

readSolutionData :: T.Text -> Sh SolutionData
readSolutionData t =
  do let result = decode (BS.pack $ T.unpack t)
     if isJust result
     then return $ fromJust result
     else terror "failed to parse JSON"

-- | Converts the solution data into matches.
makeMatchesFromSolutionData :: Parameters -> SolutionData -> [Match Node]
makeMatchesFromSolutionData params sol =
  let makeMatchFromIndexMapping maps =
        map makeNodeMappingFromIndexMapping (zip [0..] maps)
      makeNodeMappingFromIndexMapping (n1, n2) =
        let pn = indexedPatternNodes params !! n1
            fn = indexedFunctionNodes params !! n2
        in Mapping { pNode = pn, fNode = fn }
  in map (toMatch . makeMatchFromIndexMapping) (nodeMaps sol)
