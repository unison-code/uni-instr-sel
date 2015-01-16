--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.PlotCoverGraph
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting how matches cover a function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.PlotCoverGraph
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import Language.InstSel.TargetMachines.PatternMatching

import Language.InstSel.Graphs.GraphViz
import qualified Data.GraphViz as GV

import Language.InstSel.Utils.IO
  ( reportError )

import Data.Maybe
  ( isJust )



-------------
-- Functions
-------------

run :: PlotAction -> Function -> MatchsetInfo -> IO [Output]

run PlotCoverAllMatches function matchset =
  do let matches = map mdMatch (msiMatches matchset)
     dot <- mkCoveragePlot function matches
     return [toOutputWithoutID dot]

run PlotCoverPerMatch function matchset =
  do let matches = msiMatches matchset
     mapM
       ( \m ->
          do dot <- mkCoveragePlot function [mdMatch m]
             let oid = show (mdInstrID m)
                       ++ "-" ++
                       show (mdPatternID m)
                       ++ "-" ++
                       show (mdMatchID m)
             return $ toOutput oid dot
       )
       matches

run _ _ _ = reportError "PlotCoverGraph: unsupported action"

mkCoveragePlot :: Function -> [Match NodeID] -> IO String
mkCoveragePlot function matches =
  do let hasMatch n =
           any
             (\m -> isJust $ findPNInMatch m (getNodeID n))
             matches
         nf n = [ GV.style GV.filled,
                  if hasMatch n
                  then GV.fillColor GV.Green
                  else GV.fillColor GV.Red
                ]
         dot = (toDotStringWith nf noMoreEdgeAttr) $
                 osGraph $
                   functionOS function
     return dot
