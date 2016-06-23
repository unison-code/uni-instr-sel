{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

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
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatch (..)
  , PatternMatchset (..)
  )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , errorExitCode
  )

import qualified Data.Set as S

import Data.List
  ( intercalate )
import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

run
  :: CheckAction
  -> Function
  -> PatternMatchset
  -> Maybe TargetMachine
  -> IO [Output]

run CheckFunctionGraphCoverage function matchset _ =
  let g = osGraph $ functionOS function
      op_nodes = map getNodeID $ filter isOperationNode $ getAllNodes g
      matches = pmMatches matchset
      isNodeCoverable n =
        any (isJust . (flip findPNInMatch) n . pmMatch) matches
      uncovered_nodes = filter (not . isNodeCoverable) op_nodes
  in if length uncovered_nodes > 0
     then let node_strs = intercalate ", " $ map pShow uncovered_nodes
          in return [ toOutputWithExitCode errorExitCode
                                           ( "FAILED\n"
                                             ++ "  Non-coverable op nodes: "
                                             ++ node_strs
                                           )
                    ]
     else return [toOutput "OK"]

run CheckFunctionGraphLocationOverlap function matchset (Just tm) =
  let getValidLocs os nid =
        let locs = lookup nid $ osValidLocations os
        in if isJust locs
           then fromJust locs
           else []
      g = osGraph $ functionOS function
      data_nodes = map getNodeID $ filter isDatumNode $ getAllNodes g
      matches = pmMatches matchset
      getPatternGraph pm =
        let i = findInstruction (tmInstructions tm) (pmInstrID pm)
            ip = findInstrPattern (instrPatterns $ fromJust i) (pmPatternID pm)
        in if isJust i && isJust ip
           then patOS $ fromJust $ ip
           else error "run: no instruction or pattern with matching ID"
      getDefLocationsForNode os nid =
        let ssa_g = extractSSA $ osGraph os
            n = head $ findNodesWithNodeID ssa_g nid
        in if length (getDtFlowInEdges ssa_g n) > 0
           then getValidLocs os nid
           else []
      getUseLocationsForNode os nid =
        let ssa_g = extractSSA $ osGraph os
            n = head $ findNodesWithNodeID ssa_g nid
        in if length (getDtFlowInEdges ssa_g n) == 0
           then getValidLocs os nid
           else []
      hasNodeOverlappingLocs n =
        let def_locs =
              concatMap ( \pm -> let m = pmMatch pm
                                     pn = findPNInMatch m n
                                     os = getPatternGraph pm
                                 in if isJust pn
                                    then getDefLocationsForNode os (fromJust pn)
                                    else []
                        )
                        matches
            use_locs =
              concatMap ( \pm -> let m = pmMatch pm
                                     pn = findPNInMatch m n
                                     os = getPatternGraph pm
                                 in if isJust pn
                                    then getUseLocationsForNode os (fromJust pn)
                                    else []
                        )
                        matches
        in -- If either list is empty, it means there are no restrictions on the
           -- locations and thus there is automatically an overlap
           if length def_locs > 0 && length use_locs > 0
           then not
                $ S.null
                $ (S.fromList def_locs) `S.intersection` (S.fromList use_locs)
           else True
      non_overlapping_nodes = filter (not . hasNodeOverlappingLocs) data_nodes
  in if length non_overlapping_nodes > 0
     then let node_strs = intercalate ", " $ map pShow non_overlapping_nodes
          in return [ toOutputWithExitCode errorExitCode
                                           ( "FAILED\n"
                                             ++ "  Data nodes with "
                                             ++ "non-overlapping location "
                                             ++ "requirements: "
                                             ++ node_strs
                                           )
                    ]
     else return [toOutput "OK"]

run _ _ _ _ = reportErrorAndExit "CheckFunctionGraph: unsupported action"
