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
import Language.InstrSel.ConstraintModels
import Language.InstrSel.Functions
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.PrettyShow

import Language.InstrSel.Graphs.GraphViz
import qualified Data.GraphViz as GV

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run :: PlotAction
    -> Bool
       -- ^ Whether to show edge numbers.
    -> Bool
       -- ^ Whether to hide null instructions.
    -> Bool
       -- ^ Whether to hide kill instructions.
    -> Function
    -> LowLevelModel
    -> ArrayIndexMaplists
    -> IO [Output]

run PlotCoverAllMatches
    show_edge_nrs
    hide_null_instrs
    hide_kill_instrs
    function
    model
    ai_maps
  =
  do let ai_ms0 = take (fromIntegral $ llNumMatches model) $
                  ([0..] :: [ArrayIndex])
         ai_ms1 = if hide_null_instrs
                  then filter (`notElem` llMatchNullInstructions model) ai_ms0
                  else ai_ms0
         ai_ms2 = if hide_kill_instrs
                  then filter (`notElem` llMatchKillInstructions model) ai_ms1
                  else ai_ms1
         ns_list = concatMap (getFunctionNodesCoveredByMatch model ai_maps) $
                   ai_ms2
     dot <- mkCoveragePlot show_edge_nrs function ns_list
     return [toOutput dot]

run PlotCoverPerMatch show_edge_nrs _ _ function model ai_maps =
  do let ai_ms = take (fromIntegral $ llNumMatches model) $
                 ([0..] :: [ArrayIndex])
     mapM
       ( \m ->
          do let ns_list = getFunctionNodesCoveredByMatch model ai_maps m
             dot <- mkCoveragePlot show_edge_nrs function ns_list
             let mid = (ai2MatchIDs ai_maps) !! (fromIntegral m)
                 iid = (llMatchInstructionIDs model) !! (fromIntegral m)
                 oid = "m" ++ pShow mid ++ "-" ++ "i" ++ pShow iid
             return $ toOutputWithID oid dot
       )
       ai_ms

run _ _ _ _ _ _ _ = reportErrorAndExit "PlotCoverGraph: unsupported action"

mkCoveragePlot :: Bool -> Function -> [[NodeID]] -> IO ByteString
mkCoveragePlot show_edge_nrs function ns_list =
  do let hasMatch n = (getNodeID n) `elem` (concat ns_list)
         onlyInAlternatives n = all (\ns -> length ns > 1) $
                                filter ((getNodeID n) `elem`) $
                                ns_list
         nf _ n = [ GV.style GV.filled
                  , if hasMatch n
                    then if onlyInAlternatives n
                         then GV.fillColor GV.Yellow
                         else GV.fillColor GV.Green
                    else GV.fillColor GV.Red
                  ]
         ef = if show_edge_nrs then showEdgeNrsAttr else noMoreEdgeAttr
         dot = (toDotStringWith nf ef) $
               osGraph $
               functionOS function
     return dot

-- | Returns the function nodes that are covered by a given match. Since an
-- operand may have several alternatives, a pattern node may map to several
-- function nodes.
getFunctionNodesCoveredByMatch
  :: LowLevelModel
  -> ArrayIndexMaplists
  -> ArrayIndex
     -- ^ Array index of the match.
  -> [[NodeID]]
getFunctionNodesCoveredByMatch model ai_maps m =
  let ai_bs = (llMatchSpannedBlocks model) !! (fromIntegral m)
      ai_os = (llMatchOperationsCovered model) !! (fromIntegral m)
      ai_ps = (llMatchOperandsUsed model) !! (fromIntegral m) ++
              (llMatchOperandsDefined model) !! (fromIntegral m)
  in map (\ai -> [(ai2BlockNodeIDs ai_maps) !! (fromIntegral ai)]) ai_bs ++
     map (\ai -> [(ai2OperationNodeIDs ai_maps) !! (fromIntegral ai)]) ai_os ++
     map ( \p_ai -> map ( \n_ai ->
                          (ai2DatumNodeIDs ai_maps) !! (fromIntegral n_ai)
                        ) $
                    (llOperandAlternatives model) !! (fromIntegral p_ai)
         )
         ai_ps
