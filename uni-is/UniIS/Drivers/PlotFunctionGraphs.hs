{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
produceDotOutputWith :: Bool -> (Graph -> Graph) -> Function -> IO [Output]
produceDotOutputWith show_edge_nrs f fun =
  do let g = f $ osGraph $ functionOS fun
         ef = if show_edge_nrs then showEdgeNrsAttr else noMoreEdgeAttr
         dot = toDotStringWith noMoreNodeAttr ef g
     return [toOutput dot]

run :: PlotAction
    -> Bool
       -- ^ Whether to show edge numbers.
    -> Function
    -> IO [Output]

run PlotFunctionFullGraph show_edge_nrs fun =
  produceDotOutputWith show_edge_nrs id fun

run PlotFunctionControlFlowGraph show_edge_nrs fun =
  produceDotOutputWith show_edge_nrs extractCFG fun

run PlotFunctionSSAGraph show_edge_nrs fun =
  produceDotOutputWith show_edge_nrs extractSSA fun

run _ _ _ = reportErrorAndExit "PlotFunctionGraphs: unsupported action"
