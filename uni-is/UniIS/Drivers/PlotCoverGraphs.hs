{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
import UniIS.Targets
import Language.InstrSel.Functions
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
import Language.InstrSel.TargetMachines.PatternMatching

import Language.InstrSel.Graphs.GraphViz
import qualified Data.GraphViz as GV

import Language.InstrSel.Utils
  ( isLeft
  , fromLeft
  , fromRight
  )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , when
  )

import Data.Maybe
  ( catMaybes
  , fromJust
  , isJust
  , isNothing
  )



-------------
-- Functions
-------------

run :: PlotAction
    -> Bool
       -- ^ Whether to show edge numbers.
    -> Bool
       -- ^ Whether to hide null instructions.
    -> Bool
       -- ^ Whether to hide inactive instructions.
    -> Function
    -> PatternMatchset
    -> IO [Output]

run PlotCoverAllMatches
    show_edge_nrs
    hide_null_instrs
    hide_inactive_instrs
    function
    matchset
  =
  do let tid = pmTarget matchset
         tm_res = retrieveTargetMachine tid
     when (isNothing tm_res) $
       reportErrorAndExit $ "Unrecognized target machine: " ++ (pShow tid)
     let tm = fromJust tm_res
         ms0 = pmMatches matchset
         matches_res = do ms1 <- if hide_null_instrs
                                 then filterMatches tm
                                                    (not . isInstructionNull)
                                                    ms0
                                 else return ms0
                          ms2 <- if hide_inactive_instrs
                                 then filterMatches tm
                                                    ( not
                                                      . isInstructionInactive
                                                    )
                                                    ms1
                                 else return ms1
                          return ms2
     when (isLeft matches_res) $
       reportErrorAndExit $ fromLeft matches_res
     let matches = map pmMatch $ fromRight matches_res
     dot <- mkCoveragePlot show_edge_nrs function matches
     return [toOutput dot]

run PlotCoverPerMatch show_edge_nrs _ _ function matchset =
  do let matches = pmMatches matchset
     mapM
       ( \m ->
          do dot <- mkCoveragePlot show_edge_nrs function [pmMatch m]
             let oid = "m" ++ pShow (pmMatchID m) ++ "-" ++
                       "i" ++ pShow (pmInstrID m) ++ "-" ++
                       "p" ++ pShow (pmPatternID m)
             return $ toOutputWithID oid dot
       )
       matches

run _ _ _ _ _ _ = reportErrorAndExit "PlotCoverGraph: unsupported action"

mkCoveragePlot :: Bool -> Function -> [Match NodeID] -> IO String
mkCoveragePlot show_edge_nrs function matches =
  do let hasMatch n = any (\m -> length (findPNInMatch m $ getNodeID n) > 0)
                          matches
         nf n = [ GV.style GV.filled
                , if hasMatch n
                  then GV.fillColor GV.Green
                  else GV.fillColor GV.Red
                ]
         ef = if show_edge_nrs then showEdgeNrsAttr else noMoreEdgeAttr
         dot = (toDotStringWith nf ef) $
               osGraph $
               functionOS function
     return dot

filterMatches
  :: TargetMachine
  -> (Instruction -> Bool)
  -> [PatternMatch]
  -> Either String [PatternMatch]
filterMatches tm p_fun ms =
  do ms' <- mapM ( \m -> let iid = pmInstrID m
                             i = findInstruction tm iid
                         in if isJust i
                            then if p_fun $ fromJust i
                                 then return $ Just m
                                 else return Nothing
                            else Left $ "No instruction with ID "
                                        ++ pShow iid
                 ) $
            ms
     return $ catMaybes ms'
