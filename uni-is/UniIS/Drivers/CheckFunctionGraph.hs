--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.CheckFunctionGraph
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph and performs various checks on it.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.CheckFunctionGraph
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , PatternMatchset (..)
  )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.List
  ( intercalate )
import Data.Maybe
  ( isJust )



-------------
-- Functions
-------------

run :: CheckAction -> Function -> PatternMatchset -> IO [Output]

run CheckFunctionGraphCoverage function matchset =
  let g = osGraph $ functionOS function
      op_nodes = map getNodeID $ filter isNodeAnOperation $ getAllNodes g
      matches = pmMatches matchset
      isNodeCoverable n = any (isJust . (flip findPNInMatch) n . pmMatch)
                              matches
      uncovered_nodes = filter (not . isNodeCoverable) op_nodes
  in if length uncovered_nodes > 0
     then let node_strs = intercalate ", " $ map pShow uncovered_nodes
          in return [toOutputWithoutID $ "NOT COVERABLE\n" ++
                                         "  Non-coverable nodes: " ++ node_strs]
     else return [toOutputWithoutID "OK"]

run _ _ _ = reportErrorAndExit "CheckFunctionGraph: unsupported action"
