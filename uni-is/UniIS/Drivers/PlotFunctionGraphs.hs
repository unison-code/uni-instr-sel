{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.PlotFunctionGraphs
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Graphs
  ( Graph
  , extractCFG
  , extractSSA
  )
import Language.InstrSel.Functions
import Language.InstrSel.OpStructures

import Language.InstrSel.Graphs.GraphViz

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

-- | Produces DOT data as output by applying a given graph-to-graph function on
-- the given 'Function'.
produceDotOutputWith :: (Graph -> Graph) -> Function -> IO [Output]
produceDotOutputWith f fun =
  do let dot = toDotString $ f $ osGraph $ functionOS fun
     return [toOutput dot]

run :: PlotAction -> Function -> IO [Output]

run PlotFunctionFullGraph fun = produceDotOutputWith id fun

run PlotFunctionControlFlowGraph fun = produceDotOutputWith extractCFG fun

run PlotFunctionSSAGraph fun = produceDotOutputWith extractSSA fun

run _ _ = reportErrorAndExit "PlotFunctionGraphs: unsupported action"
