--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.PlotFunctionGraph
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for plotting various information about a function graph.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.PlotFunctionGraph
  ( run )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions
import Language.InstSel.OpStructures

import Language.InstSel.Graphs.GraphViz

import Language.InstSel.Utils.IO
  ( reportError )



-------------
-- Functions
-------------

run :: PlotAction -> Function -> IO [Output]

run PlotFunctionGraph f =
  do let dot = toDotString $ osGraph $ functionOS f
     return [toOutputWithoutID dot]

run _ _ = reportError "PlotFunction: unsupported action"
