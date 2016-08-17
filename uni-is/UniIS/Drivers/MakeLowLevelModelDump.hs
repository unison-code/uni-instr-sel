{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.Maybe
  ( isJust
  , fromJust
  )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> LowLevelModel -> ArrayIndexMaplists
       -> IO [Output]

run MakeLowLevelModelDump function model ai_maps =
  let addPadding ai = (take (length $ pShow ai) $ repeat ' ') ++ "    "
      function_g = osGraph $ functionOS function
      mkNodeInfo n =
        "Node ID: " ++ pShow n ++ ", "
        ++
        ( pShow $
          G.getNodeType $
          head $
          G.findNodesWithNodeID function_g n
        )
      dumpOperationNodes ns =
        let dumpNode n ai =
              mkNodeInfo n
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Covered by: "
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
              mkNodeInfo n
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Alternative to: "
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
              "Defined by: "
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
              "Used by: "
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
              mkNodeInfo n
              ++
              "\n"
              ++
              addPadding ai
              ++
              "Spanned by: "
              ++
              ( pShow
                $ map snd
                $ filter (\(nodes, _) -> ai `elem` nodes)
                $ zip (llMatchSpannedBlocks model) ([0..] :: [ArrayIndex])
                      -- Cast needed to prevent compiler warning
              )
        in concatMap (\(n, ai) -> pShow ai ++ " -> " ++ dumpNode n ai ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
      dumpMatches ns =
        let mkMatchInfo m ai =
              let tm_res = retrieveTargetMachine $ llTMID model
                  tm = fromJust tm_res
                  iid = (llMatchInstructionIDs model) !! (fromIntegral ai)
                  instr_res = findInstruction (tmInstructions tm) iid
                  instr = fromJust instr_res
                  pid = (llMatchPatternIDs model) !! (fromIntegral ai)
                  pat_res = findInstrPattern (instrPatterns instr) pid
                  pat = fromJust pat_res
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
                 (pShow $ (llMatchOperandsDefined model) !! (fromIntegral ai))
                 ++
                 " / "
                 ++
                 ( pShow $
                   map ( \o -> (llOperandAlternatives model) !! (fromIntegral o)
                       ) $
                   (llMatchOperandsDefined model) !! (fromIntegral ai)
                 )
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Data / operands used: "
                 ++
                 (pShow $ (llMatchOperandsUsed model) !! (fromIntegral ai))
                 ++
                 " / "
                 ++
                 ( pShow $
                   map ( \o -> (llOperandAlternatives model) !! (fromIntegral o)
                       ) $
                   (llMatchOperandsUsed model) !! (fromIntegral ai)
                 )
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
                 (pShow emit_str)
                 ++
                 "\n"
                 ++
                 addPadding ai
                 ++
                 "Is inactive instruction: "
                 ++
                 (pShow $ isInstructionInactive instr)
        in concatMap (\(m, i) -> pShow i ++ " -> " ++ mkMatchInfo m i ++ "\n\n")
                     (zip ns ([0..] :: [ArrayIndex]))
                     -- Cast needed to prevent compiler warning
  in do return [ toOutput
                 $ "OPERATIONS" ++ "\n\n" ++
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
