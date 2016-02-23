--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.PlotCoverGraphs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting how matches cover a function graph.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.PlotCoverGraphs
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.PatternMatching

import Language.InstrSel.Graphs.GraphViz
import qualified Data.GraphViz as GV

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.Maybe
  ( isJust )



-------------
-- Functions
-------------

run :: PlotAction -> Function -> PatternMatchset -> IO [Output]

run PlotCoverAllMatches function matchset =
  do let matches = map pmMatch (pmMatches matchset)
     dot <- mkCoveragePlot function matches
     return [toOutputWithoutID dot]

run PlotCoverPerMatch function matchset =
  do let matches = pmMatches matchset
     mapM
       ( \m ->
          do dot <- mkCoveragePlot function [pmMatch m]
             let oid = "m" ++ pShow (pmMatchID m)
                       ++ "-" ++
                       "i" ++ pShow (pmInstrID m)
                       ++ "-" ++
                       "p" ++ pShow (pmPatternID m)
             return $ toOutput oid dot
       )
       matches

run _ _ _ = reportErrorAndExit "PlotCoverGraph: unsupported action"

mkCoveragePlot :: Function -> [Match NodeID] -> IO String
mkCoveragePlot function matches =
  do let hasMatch n = any (\m -> isJust $ findPNInMatch m (getNodeID n)) matches
         nf n = [ GV.style GV.filled,
                  if hasMatch n
                  then GV.fillColor GV.Green
                  else GV.fillColor GV.Red
                ]
         dot = (toDotStringWith nf noMoreEdgeAttr)
               $ osGraph
               $ functionOS function
     return dot
