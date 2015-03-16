--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.PlotFunctionGraphs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting various information about a function graph.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.PlotFunctionGraphs
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Graphs
  ( Graph
  , extractCFG
  , extractSSA
  )
import Language.InstrSel.Functions
import Language.InstrSel.OpStructures

import Language.InstrSel.Graphs.GraphViz

import Language.InstrSel.Utils.IO
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

run _ _ = reportError "PlotFunctionGraphs: unsupported action"
