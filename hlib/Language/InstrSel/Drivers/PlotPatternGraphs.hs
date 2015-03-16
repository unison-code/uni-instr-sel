--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Drivers.PlotPatternGraphs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting various information about a pattern graph.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Drivers.PlotPatternGraphs
  ( run )
where

import Language.InstrSel.Drivers.Base
import Language.InstrSel.Graphs
  ( Graph
  , extractCFG
  , extractSSA
  )
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )
import Language.InstrSel.OpStructures

import Language.InstrSel.Graphs.GraphViz

import Language.InstrSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

-- | Produces DOT data as output by applying a given graph-to-graph function on
-- the given 'InstrPattern'.
produceDotOutputWith :: (Graph -> Graph) -> InstrPattern -> IO [Output]
produceDotOutputWith f p =
  do let dot = toDotString $ f $ osGraph $ patOS p
     return [toOutputWithoutID dot]

run :: PlotAction -> InstrPattern -> IO [Output]

run PlotPatternFullGraph p = produceDotOutputWith id p

run PlotPatternControlFlowGraph p = produceDotOutputWith extractCFG p

run PlotPatternSSAGraph p = produceDotOutputWith extractSSA p

run _ _ = reportError "PlotPatternGraphs: unsupported action"
