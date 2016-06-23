{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

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
     return [toOutput dot]

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
             return $ toOutputWithID oid dot
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
