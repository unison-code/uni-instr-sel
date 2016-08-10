{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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

run :: PlotAction -> Function -> PatternMatchset -> Bool -> IO [Output]

run PlotCoverAllMatches function matchset hide_inactive_instrs =
  do let tid = pmTarget matchset
         tm_res = retrieveTargetMachine tid
     when (isNothing tm_res) $
       reportErrorAndExit $ "Unrecognized target machine: " ++ (pShow tid)
     let tm = fromJust tm_res
         matches_res =
           let ms = pmMatches matchset
           in if not hide_inactive_instrs
              then Right $ map pmMatch ms
              else do ms' <- mapM ( \m ->
                                      let is = tmInstructions tm
                                          iid = pmInstrID m
                                          i = findInstruction is iid
                                      in if isJust i
                                         then if not
                                                 $ isInstructionInactive
                                                 $ fromJust i
                                              then return $ Just m
                                              else return Nothing
                                         else Left $ "No instruction with ID "
                                                     ++ pShow iid
                                  )
                                  ms
                      return $ map pmMatch $ catMaybes ms'
     when (isLeft matches_res) $
       reportErrorAndExit $ fromLeft matches_res
     let matches = fromRight matches_res
     dot <- mkCoveragePlot function matches
     return [toOutput dot]

run PlotCoverPerMatch function matchset _ =
  do let matches = pmMatches matchset
     mapM
       ( \m ->
          do dot <- mkCoveragePlot function [pmMatch m]
             let oid = "m" ++ pShow (pmMatchID m)
                       ++ "-" ++
                       "i" ++ pShow (pmInstrID m)
                       ++ "-" ++
                       "p" ++ pShow (pmPatternID m)
             return $ toOutputWithID oid dot
       )
       matches

run _ _ _ _ = reportErrorAndExit "PlotCoverGraph: unsupported action"

mkCoveragePlot :: Function -> [Match NodeID] -> IO String
mkCoveragePlot function matches =
  do let hasMatch n = any (\m -> isJust $ findPNInMatch m (getNodeID n)) matches
         nf n = [ GV.style GV.filled,
                  if hasMatch n
                  then GV.fillColor GV.Green
                  else GV.fillColor GV.Red
                ]
         dot = (toDotStringWith nf noMoreEdgeAttr)
               $ osGraph
               $ functionOS function
     return dot
