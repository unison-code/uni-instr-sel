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

import Language.InstSel.Graphs.GraphViz
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

import Data.Maybe
  ( fromJust
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

-------------
-- Functions
-------------

run
  :: String
     -- ^ The content of the function graph.
  -> Maybe String
     -- ^ The content of the matches, if needed.
  -> PlotAction
     -- ^ The action to perform.
  -> (String -> IO ())
     -- ^ The function that takes care of outputting the result.
  -> IO ()

run f_str _ PlotFunctionGraph output =
  do let f_res = fromJson f_str
     when (isLeft f_res) $
       do putStrLn $ fromLeft f_res
          exitFailure
     let function = fromRight f_res
         dot = toDotString $ osGraph $ functionOS function
     output dot

run f_str m_str PlotFunctionGraphCoverage output =
  do let f_res = fromJson f_str
     when (isLeft f_res) $
       do putStrLn $ fromLeft f_res
          exitFailure
     when (isNothing m_str) $
       do putStrLn "No match file provided."
          exitFailure
     let m_res = fromJson $ fromJust m_str
     when (isLeft m_res) $
       do putStrLn $ fromLeft m_res
          exitFailure
     let function = fromRight f_res
         matches = fromRight m_res :: MatchsetInfo
         dot = toDotString $ osGraph $ functionOS function
     output dot
