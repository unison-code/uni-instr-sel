{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )
import Language.InstrSel.Utils.String
  ( replace )

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
  let tm_id = llTMID model
      tm_res = retrieveTargetMachine tm_id
      tm = if isJust tm_res
           then fromJust tm_res
           else error $ "run: found no target machine with ID " ++ pShow tm_id
      addPadding ai = (take (length $ pShow ai) $ repeat ' ') ++ "    "
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
              any ( \o -> n `elem` ( (llOperandAlternatives model) !!
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
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Assigned location: "
              ++
              ( let i = fromIntegral ai
                    sol_ai = if llSolHasDatumLocation sol !! i
                             then Just $ llSolLocationsOfData sol !! i
                             else Nothing
                in if isJust sol_ai
                   then let loc_id = ai2LocationIDs ai_maps !!
                                     (fromIntegral $ fromJust sol_ai)
                            loc = findLocation tm loc_id
                        in if isJust loc
                           then pShow loc
                           else error $ "run: found no location with ID " ++
                                        pShow loc_id
                   else "NULL"
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
              let i = fromIntegral ai
                  iid = (llMatchInstructionIDs model) !! i
                  instr_res = findInstruction tm iid
                  instr = if isJust instr_res
                          then fromJust instr_res
                          else error $ "No instruction with ID " ++ (pShow iid)
                  emit_str = instrEmitString instr
              in "Match ID: " ++ pShow m
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
                   (llMatchOperandsUsed model) !! i
                 )
                 ++
                 " / "
                 ++
                 (pShow $ (llMatchOperandsUsed model) !! i)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Operations covered: "
                 ++
                 (pShow $ (llMatchOperationsCovered model) !! i)
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
                   (llMatchOperandsDefined model) !! i
                 )
                 ++
                 " / "
                 ++
                 (pShow $ (llMatchOperandsDefined model) !! i)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Blocks spanned: "
                 ++
                 (pShow $ (llMatchSpannedBlocks model) !! i)
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
                 "Latency: "
                 ++
                 (pShow $ (llMatchLatencies model) !! i)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Code size: "
                 ++
                 (pShow $ (llMatchCodeSizes model) !! i)
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
                 "Is kill instruction: "
                 ++
                 (pShow $ ai `elem` (llMatchKillInstructions model))
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
