{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.PlotPatternGraphs
  ( run )
where

import UniIS.Drivers.Base
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
  ( reportErrorAndExit )



-------------
-- Functions
-------------

-- | Produces DOT data as output by applying a given graph-to-graph function on
-- the given 'InstrPattern'.
produceDotOutputWith :: Bool -> (Graph -> Graph) -> InstrPattern -> IO [Output]
produceDotOutputWith show_edge_nrs f pat =
  do let g = f $ osGraph $ patOS pat
         ef = if show_edge_nrs then showEdgeNrsAttr else noMoreEdgeAttr
         dot = toDotStringWith noMoreNodeAttr ef g
     return [toOutput dot]

run :: PlotAction
    -> Bool
       -- ^ Whether to show edge numbers.
    -> InstrPattern
    -> IO [Output]

run PlotPatternFullGraph show_edge_nrs p =
  produceDotOutputWith show_edge_nrs id p

run PlotPatternControlFlowGraph show_edge_nrs p =
  produceDotOutputWith show_edge_nrs extractCFG p

run PlotPatternSSAGraph show_edge_nrs p =
  produceDotOutputWith show_edge_nrs extractSSA p

run _ _ _ = reportErrorAndExit "PlotPatternGraphs: unsupported action"
