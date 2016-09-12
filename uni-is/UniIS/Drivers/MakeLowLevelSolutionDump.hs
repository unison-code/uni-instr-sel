{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeLowLevelSolutionDump
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Targets

import Language.InstrSel.ConstraintModels
  ( ArrayIndexMaplists (..)
  , LowLevelModel (..)
  , LowLevelSolution (..)
  )
import Language.InstrSel.ConstraintModels.IDs
  ( ArrayIndex (..) )
import Language.InstrSel.Functions
  ( Function (..) )
import qualified Language.InstrSel.Graphs as G
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines

import Language.InstrSel.Utils
  ( replace )
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.Maybe
  ( isJust
  , fromJust
  )

import Data.List
  ( genericIndex )



-------------
-- Functions
-------------

run :: MakeAction
    -> Function
    -> LowLevelModel
    -> ArrayIndexMaplists
    -> LowLevelSolution
    -> IO [Output]

run MakeLowLevelSolutionDump function model ai_maps sol =
  let addPadding ai = (take (length $ pShow ai) $ repeat ' ') ++ "    "
      function_g = osGraph $ functionOS function
      mkNodeInfo n ai =
        "Node ID: " ++ pShow n
        ++
        "\n"
        ++
        addPadding ai
        ++
        ( pShow $
          G.getNodeType $
          head $
          G.findNodesWithNodeID function_g n
        )
      dumpOperationNodes ns =
        let dumpNode n ai =
              mkNodeInfo n ai
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Covered by match: "
              ++
              ( pShow $
                head $
                filter (genericIndex (llSolIsMatchSelected sol)) $
                map snd $
                filter (\(nodes, _) -> ai `elem` nodes) $
                zip (llMatchOperationsCovered model) ([0..] :: [ArrayIndex])
                -- Cast needed to prevent compiler warning
              )
        in concatMap (\(n, ai) -> pShow ai ++ " -> " ++ dumpNode n ai ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
      dumpDataNodes ns =
        let isNodeAnOpAlt n ops =
              any ( \o -> n `elem` ( (llOperandAlternatives model)
                                     !!
                                     (fromIntegral o)
                                   )
                  )
                  ops
            dumpNode n ai =
              mkNodeInfo n ai
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Defined by match: "
              ++
              ( pShow $
                head $
                filter (genericIndex (llSolIsMatchSelected sol)) $
                map snd $
                filter (isNodeAnOpAlt ai . fst) $
                zip (llMatchOperandsDefined model) ([0..] :: [ArrayIndex])
                -- Cast needed to prevent compiler warning
              )
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Used by matches: "
              ++
              ( pShow $
                filter (genericIndex (llSolIsMatchSelected sol)) $
                map snd $
                filter (isNodeAnOpAlt ai . fst) $
                zip (llMatchOperandsUsed model) ([0..] :: [ArrayIndex])
                -- Cast needed to prevent compiler warning
              )
        in concatMap (\(n, ai) -> pShow ai ++ " -> " ++ dumpNode n ai ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
      dumpBlockNodes ns =
        let dumpNode n ai =
              mkNodeInfo n ai
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Spanned by matches: "
              ++
              ( pShow $
                filter (genericIndex (llSolIsMatchSelected sol)) $
                map snd $
                filter (\(nodes, _) -> ai `elem` nodes) $
                zip (llMatchSpannedBlocks model) ([0..] :: [ArrayIndex])
                -- Cast needed to prevent compiler warning
              )
        in concatMap (\(n, ai) -> pShow ai ++ " -> " ++ dumpNode n ai ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
      dumpMatches ms =
        let mkMatchInfo m ai =
              let tm_res = retrieveTargetMachine $ llTMID model
                  tm = fromJust tm_res
                  iid = (llMatchInstructionIDs model) !! (fromIntegral ai)
                  instr_res = findInstruction (tmInstructions tm) iid
                  instr = if isJust instr_res
                          then fromJust instr_res
                          else error $ "No instruction with ID " ++ (pShow iid)
                  pid = (llMatchPatternIDs model) !! (fromIntegral ai)
                  pat_res = findInstrPattern (instrPatterns instr) pid
                  pat = if isJust instr_res
                        then fromJust pat_res
                        else error $ "No pattern with ID " ++ (pShow pid) ++
                                     " in instruction with ID " ++ (pShow iid)
                  emit_str = patEmitString pat
              in "Match ID: " ++ pShow m
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Operations covered: "
                 ++
                 (pShow $ (llMatchOperationsCovered model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Data / operands defined: "
                 ++
                 ( pShow $
                   map ( \o -> (llOperandAlternatives model) !! (fromIntegral o)
                       ) $
                   (llMatchOperandsDefined model) !! (fromIntegral ai)
                 )
                 ++
                 " / "
                 ++
                 (pShow $ (llMatchOperandsDefined model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Data / operands used: "
                 ++
                 ( pShow $
                   map ( \o -> (llOperandAlternatives model) !! (fromIntegral o)
                       ) $
                   (llMatchOperandsUsed model) !! (fromIntegral ai)
                 )
                 ++
                 " / "
                 ++
                 (pShow $ (llMatchOperandsUsed model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Blocks spanned: "
                 ++
                 (pShow $ (llMatchSpannedBlocks model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Instruction ID: "
                 ++
                 (pShow iid)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Pattern ID: "
                 ++
                 (pShow pid)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Latency: "
                 ++
                 (pShow $ (llMatchLatencies model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Code size: "
                 ++
                 (pShow $ (llMatchCodeSizes model) !! (fromIntegral ai))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Emit string: "
                 ++
                 ( replace "\n" ("\n" ++ addPadding ai ++ "             ") $
                   (pShow emit_str)
                 )
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Is null instruction: "
                 ++
                 (pShow $ ai `elem` (llMatchNullInstructions model))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Is inactive instruction: "
                 ++
                 (pShow $ ai `elem` (llMatchInactiveInstructions model))
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Placed in block: "
                 ++
                 (pShow $ genericIndex (llSolBlocksOfMatches sol) ai)
        in concatMap (\(m, i) -> pShow i ++ " -> " ++ mkMatchInfo m i ++ "\n\n")
                     ( filter ( \(_, ai) ->
                                genericIndex (llSolIsMatchSelected sol) ai
                              ) $
                       zip ms ([0..] :: [ArrayIndex])
                       -- Cast needed to prevent compiler warning
                     )
  in do return [ toOutput $
                 "OPERATIONS" ++ "\n\n" ++
                 (dumpOperationNodes $ ai2OperationNodeIDs ai_maps) ++
                 "\n\n" ++
                 "DATA" ++ "\n\n" ++
                 (dumpDataNodes $ ai2DatumNodeIDs ai_maps) ++
                 "\n\n" ++
                 "BLOCKS" ++ "\n\n" ++
                 (dumpBlockNodes $ ai2BlockNodeIDs ai_maps) ++
                 "\n\n" ++
                 "SELECTED MATCHES" ++ "\n\n" ++
                 (dumpMatches $ ai2MatchIDs ai_maps)
               ]

run _ _ _ _ _ =
  reportErrorAndExit "MakeLowLevelSolutionDump: unsupported action"
