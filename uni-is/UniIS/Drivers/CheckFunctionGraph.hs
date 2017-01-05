{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
import qualified Language.InstrSel.Utils.Set as S
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

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
      op_nodes = filter isOperationNode $ getAllNodes g
      matches = pmMatches matchset
      isNodeCoverable n =
        any (\m -> length (findPNInMatch (pmMatch m) (getNodeID n)) > 0)
            matches
      uncovered_ops = filter (not . isNodeCoverable) op_nodes
      l = concatLogs $
          map (reportUncoveredOp g) uncovered_ops
  in return $ mkOutputFromLog l

run CheckFunctionGraphLocationOverlap function matchset (Just tm) =
  let getValidLocs os nid =
        let locs = lookup nid $ osValidLocations os
        in if isJust locs
           then fromJust locs
           else []
      g = osGraph $ functionOS function
      data_nodes = filter isDatumNode $ getAllNodes g
      matches = pmMatches matchset
      getPatternGraph pm =
        let i = findInstruction tm (pmInstrID pm)
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
        let nid = getNodeID n
            def_locs =
              S.unions $
              map ( \pm -> let m = pmMatch pm
                               pn = findPNInMatch m nid
                               os = getPatternGraph pm
                               locs = map ( S.fromList .
                                            getDefLocationsForNode os
                                          )
                                          pn
                           in S.intersections locs
                        ) $
              matches
            use_locs =
              S.unions $
              map ( \pm -> let m = pmMatch pm
                               pn = findPNInMatch m nid
                               os = getPatternGraph pm
                               locs = map ( S.fromList .
                                            getUseLocationsForNode os
                                          )
                                          pn
                           in S.intersections locs
                        ) $
              matches
        in -- If either list is empty, it means there are no restrictions on the
           -- locations and thus there is automatically an overlap
           if S.size def_locs > 0 && S.size use_locs > 0
           then not $
                S.null $
                def_locs `S.intersection` use_locs
           else True
      non_overlapping_nodes = filter (not . hasNodeOverlappingLocs) data_nodes
      l = concatLogs $
          map reportNonOverlappingLocsForDatum non_overlapping_nodes
  in return $ mkOutputFromLog l

run _ _ _ _ = reportErrorAndExit "CheckFunctionGraph: unsupported action"

reportUncoveredOp :: Graph -> Node -> Log
reportUncoveredOp g o =
  let in_ds = filter isDatumNode $
              map (getSourceNode g) $
              getDtFlowInEdges g o
      out_ds = filter isDatumNode $
               map (getTargetNode g) $
               getDtFlowOutEdges g o
  in toLog $
     ErrorMessage $
     "Uncovered operation: " ++ pShow o ++ " with in-data " ++ pShow in_ds ++
     " and out-data " ++ pShow out_ds

reportNonOverlappingLocsForDatum :: Node -> Log
reportNonOverlappingLocsForDatum n =
  toLog $
  ErrorMessage $
  "Datum node with non-overlapping location requirements: " ++ pShow n
