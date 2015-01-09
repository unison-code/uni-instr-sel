--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Plotter
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting various information about the input.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Plotter
  ( PlotAction (..)
  , run
  )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
import Language.InstSel.TargetMachines.PatternMatching
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON
  hiding
  ( unpack )

import Language.InstSel.Graphs.GraphViz
import qualified Data.GraphViz as GV

import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  )
import System.Exit
  ( exitFailure )



--------------
-- Data types
--------------

data PlotAction
  = PlotFunctionGraph
  | PlotFunctionGraphCoverage
  | PlotFunctionGraphCoveragePerMatch

-------------
-- Functions
-------------

-- | Gets the matchest information from a JSON string. Reports error if this
-- fails.
getMatchsetInfo :: Maybe String -> IO MatchsetInfo
getMatchsetInfo str =
  do when (isNothing str) $
       do putStrLn "No match file provided."
          exitFailure
     let res = fromJson $ fromJust str
     when (isLeft res) $
       do putStrLn $ fromLeft res
          exitFailure
     return $ fromRight res

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

run
  :: String
     -- ^ The content of the function graph.
  -> Maybe String
     -- ^ The content of the matches, if needed.
  -> PlotAction
     -- ^ The action to perform.
  -> IO [Output]
     -- ^ The produced output.

run str _ PlotFunctionGraph =
  do function <- getFunction str
     let dot = toDotString $ osGraph $ functionOS function
     return [toOutputWithoutID dot]

run f_str m_str PlotFunctionGraphCoverage =
  do function <- getFunction f_str
     match_info <- getMatchsetInfo m_str
     let matches = map mdMatch (msiMatches match_info)
     dot <- mkCoveragePlot function matches
     return [toOutputWithoutID dot]

run f_str m_str PlotFunctionGraphCoveragePerMatch =
  do function <- getFunction f_str
     match_info <- getMatchsetInfo m_str
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
       (msiMatches match_info)
