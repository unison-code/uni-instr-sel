--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Modeler
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph, a set of pattern matches as input, and produces the
-- data needed for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Modeler
  ( run )
where


import Language.InstSel.Drivers.Base
import Language.InstSel.CPModel.ParamMaker
import Language.InstSel.TargetMachines.Targets
  ( getTargetMachine )
import Language.InstSel.TargetMachines.PatternMatching
  ( MatchsetInfo (..) )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.JSON

import Data.Maybe
  ( fromJust
  , isNothing
  )
import System.Exit
  ( exitFailure )



-------------
-- Functions
-------------

run
  :: String
     -- ^ The function in JSON format.
  -> String
     -- ^ The instruction pattern matches in JSON format.
  -> IO [Output]
     -- ^ The produced output.
run f_str m_str =
  do let f_res = fromJson f_str
         m_res = fromJson m_str
     when (isLeft f_res) $
       do putStrLn $ fromLeft f_res
          exitFailure
     when (isLeft m_res) $
       do putStrLn $ fromLeft m_res
          exitFailure
     let function = fromRight f_res
         msinfo = fromRight m_res
         matches = msiMatches msinfo
         target_id = msiTarget msinfo
         mtarget = getTargetMachine target_id
     when (isNothing mtarget) $
       do putStrLn $ "No target machine with ID '" ++ show target_id ++ "'"
          exitFailure
     let target = fromJust mtarget
         params = mkParams target function matches
     return [toOutputWithoutID $ toJson params]
