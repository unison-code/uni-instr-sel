--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.PlotFunctionGraphs
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting various information about a function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.PlotFunctionGraphs
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Graphs
  ( Graph
  , extractCFG
  , extractSSA
  )
import Language.InstSel.Functions
import Language.InstSel.OpStructures

import Language.InstSel.Graphs.GraphViz

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

-- | Produces DOT data as output by applying a given graph-to-graph function on
-- the given 'Function'.
produceDotOutputWith :: (Graph -> Graph) -> Function -> IO [Output]
produceDotOutputWith f fun =
  do let dot = toDotString $ f $ osGraph $ functionOS fun
     return [toOutputWithoutID dot]

run :: PlotAction -> Function -> IO [Output]

run PlotFunctionFullGraph fun = produceDotOutputWith id fun

run PlotFunctionControlFlowGraph fun = produceDotOutputWith extractCFG fun

run PlotFunctionSSAGraph fun = produceDotOutputWith extractSSA fun

run _ _ = reportError "PlotFunction: unsupported action"
