{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.MakeLowLevelModelDump
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Targets

import Language.InstrSel.ConstraintModels
  ( ArrayIndexMaplists (..)
  , LowLevelModel (..)
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

import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )
import Language.InstrSel.Utils.String
  ( replace )

import Data.Maybe
  ( isJust
  , fromJust
  )
import Data.List
  ( intercalate
  , nub
  , sort
  )



-------------
-- Functions
-------------

-- | Function for executing this driver.
run :: MakeAction
    -> Function
    -> LowLevelModel
    -> ArrayIndexMaplists
    -> IO [Output]

run MakeLowLevelModelDump function model ai_maps =
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
              "Covered by matches: "
              ++
              ( pShow $
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
              "Alternative to operands: "
              ++
              ( pShow
                $ map snd
                $ filter (\(nodes, _) -> ai `elem` nodes)
                $ zip (llOperandAlternatives model) ([0..] :: [ArrayIndex])
                  -- Cast needed to prevent compiler warning
              )
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Defined by matches: "
              ++
              ( pShow
                $ map snd
                $ filter (isNodeAnOpAlt ai . fst)
                $ zip (llMatchOperandsDefined model) ([0..] :: [ArrayIndex])
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
                map snd $
                filter (\(nodes, _) -> ai `elem` nodes) $
                zip (llMatchSpannedBlocks model) ([0..] :: [ArrayIndex])
                      -- Cast needed to prevent compiler warning
              )
        in concatMap (\(n, ai) -> pShow ai ++ " -> " ++ dumpNode n ai ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
      aggregateLocsPerOpForMatch ai ls =
        let op_loc_pairs = map (\(_, o, l) -> (o, l)) $
                           filter (\(ai', _, _) -> ai' == ai) $
                           ls
            ops = sort $
                  nub $
                  map fst op_loc_pairs
            op_locs_pairs = map ( \o -> ( o
                                        , sort $
                                          map snd $
                                          filter (\(o', _) -> o' == o) $
                                          op_loc_pairs
                                        )
                                ) $
                            ops
        in op_locs_pairs
      dumpMatches ms =
        let mkMatchInfo m ai =
              let tm_res = retrieveTargetMachine $ llTMID model
                  tm = fromJust tm_res
                  iid = (llMatchInstructionIDs model) !! (fromIntegral ai)
                  instr_res = findInstruction tm iid
                  instr = if isJust instr_res
                          then fromJust instr_res
                          else error $ "No instruction with ID " ++ (pShow iid)
                  ops_used = (llMatchOperandsUsed model) !! (fromIntegral ai)
                  ops_defined =
                    (llMatchOperandsDefined model) !! (fromIntegral ai)
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
                   ops_used
                 )
                 ++
                 " / "
                 ++
                 (pShow ops_used)
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
                   ops_defined
                 )
                 ++
                 " / "
                 ++
                 (pShow ops_defined)
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
                   pShow $ instrEmitString instr
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
                 "Locations requirements: "
                 ++
                 ( intercalate ( "\n" ++ addPadding ai ++
                                 "                        "
                               ) $
                   map ( \(op, locs) ->
                         "operand " ++ pShow op ++ " in " ++ pShow locs
                       ) $
                   aggregateLocsPerOpForMatch ai $
                   llMatchValidValueLocs model
                 )
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Operands that must be in same location: "
                 ++
                 ( intercalate ( "\n" ++ addPadding ai ++
                                 "                                        "
                               ) $
                   map (\(_, p1, p2) -> pShow (p1, p2)) $
                   filter (\(ai', _, _) -> ai' == ai) $
                   llMatchSameValueLocs model
                 )
        in concatMap (\(m, i) -> pShow i ++ " -> " ++ mkMatchInfo m i ++ "\n\n")
                     (zip ms ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
  in do return [ toOutput $
                 BS.pack $
                 "OPERATIONS" ++ "\n\n" ++
                 (dumpOperationNodes $ ai2OperationNodeIDs ai_maps) ++
                 "\n\n" ++
                 "DATA" ++ "\n\n" ++
                 (dumpDataNodes $ ai2DatumNodeIDs ai_maps) ++
                 "\n\n" ++
                 "BLOCKS" ++ "\n\n" ++
                 (dumpBlockNodes $ ai2BlockNodeIDs ai_maps) ++
                 "\n\n" ++
                 "MATCHES" ++ "\n\n" ++
                 (dumpMatches $ ai2MatchIDs ai_maps)
               ]

run _ _ _ _ =
  reportErrorAndExit "MakeLowLevelModelDump: unsupported action"
