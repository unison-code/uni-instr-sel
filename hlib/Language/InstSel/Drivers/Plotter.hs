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
import Language.InstSel.Drivers.Modeler
  ( parseFunction )
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules
import Language.InstSel.TargetMachines
  ( TargetMachine )
import Control.Monad
  ( when )
import Data.GraphViz.Printing
  ( renderDot
  , toDot
  )
import qualified Data.Text.Lazy as T
  ( unpack )



-------------
-- Functions
-------------

run ::
     String
     -- ^ The content of some input file.
  -> TargetMachine
     -- ^ The target machine.
  -> [Bool]
     -- ^ List of Boolean arguments which determine what to plot. Note that the
     -- order is important.
  -> (String -> IO ())
     -- ^ The function that takes care of emitting the JSON data.
  -> IO ()
run str target [ plotFunctionGraph ] emit =
  do function <- parseFunction str
     when plotFunctionGraph $
       do let dot = T.unpack $
                      renderDot $
                        toDot $
                          toDotGraph $
                            osGraph $
                              functionOS function
          emit dot
     return ()
run _ _ _ _ = error "Invalid call to Plotter.run!"
