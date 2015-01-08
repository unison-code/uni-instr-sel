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
  ( run )
where

import Language.InstSel.Graphs.GraphViz
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON
  hiding
  ( unpack )
import Data.GraphViz.Printing
  ( renderDot
  , toDot
  )
import Data.Text.Lazy
  ( unpack )
import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

run ::
     String
     -- ^ The content of the function graph.
  -> Maybe TargetMachine
     -- ^ The target machine, if needed.
  -> [Bool]
     -- ^ List of Boolean arguments which determine what to plot. Note that the
     -- order is important.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the JSON data.
  -> IO ()
run f_str _ [ plotFunctionGraph ] emit =
  do let f_res = fromJson f_str
     when (isLeft f_res) $
       do putStrLn $ fromLeft f_res
          exitFailure
     let function = fromRight f_res
     when plotFunctionGraph $
       do let dot = unpack $
                      renderDot $
                        toDot $
                          toDotGraph $
                            osGraph $
                              functionOS function
          emit dot
     return ()
run _ _ _ _ = error "Invalid call to Plotter.run!"
