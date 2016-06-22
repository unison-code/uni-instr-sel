--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.MakeDumpFromArrayIndexMaplists
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes a function graph, pattern matchset, and array index maplists as input,
-- and produces a dump containing the combined information.
--
--------------------------------------------------------------------------------

module UniIS.Drivers.MakeDumpFromArrayIndexMaplists
  ( run )
where

import UniIS.Drivers.Base
import UniIS.Targets

import Language.InstrSel.ConstraintModels
  ( ArrayIndexMaplists (..) )
import Language.InstrSel.Functions
  ( Function (..) )
import qualified Language.InstrSel.Graphs as G
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( TargetMachine (..)
  , findLocation
  )
import Language.InstrSel.TargetMachines.PatternMatching
  ( PatternMatchset (..)
  , PatternMatch (..)
  , findPatternMatchesWithMatchID
  )

import Data.Maybe
  ( fromJust
  , isJust
  )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )



-------------
-- Functions
-------------

run :: MakeAction -> Function -> PatternMatchset -> ArrayIndexMaplists
       -> IO [Output]

run MakeDumpFromArrayIndexMaplists function matchset ai_maps =
  let function_g = osGraph $ functionOS function
      dumpNodes ns =
        let mtup = zip ([0..] :: [Integer]) (pmMatches matchset)
                   -- Cast needed to prevent compiler warning
            mkNodeInfo ai n =
              "Node ID " ++ pShow n ++ ", "
              ++
              ( pShow
                $ G.getNodeType
                $ head
                $ G.findNodesWithNodeID function_g n
              )
              ++
              "\n"
              ++
              (take (length $ pShow ai) $ repeat ' ')
              ++
              "    Appears in matches: "
              ++
              ( pShow
                $ map fst
                $ filter (\(_, m) -> isJust $ G.findPNInMatch (pmMatch m) n)
                $ mtup
              )
        in concatMap (\(ai, n) -> pShow ai ++ " -> " ++ mkNodeInfo ai n ++ "\n")
                     (zip ([0..] :: [Integer]) ns) -- Cast needed to prevent
                                                   -- compiler warning
      dumpMatches ns =
        let mkMatchInfo ai m =
              let pmatch = head $ findPatternMatchesWithMatchID matchset m
              in "Match ID " ++ pShow m ++ ", "
                 ++
                 "Instruction ID " ++ (pShow $ pmInstrID pmatch) ++ ", "
                 ++
                 "Pattern ID " ++ (pShow $ pmPatternID pmatch)
                 ++
                 "\n"
                 ++
                 (take (length $ pShow ai) $ repeat ' ')
                 ++
                 "    Nodes: "
                 ++
                 (pShow $ map G.fNode $ G.fromMatch $ pmMatch pmatch)
        in concatMap (\(i, m) -> pShow i ++ " -> " ++ mkMatchInfo i m ++ "\n")
                     (zip ([0..] :: [Integer]) ns) -- Cast needed to prevent
                                                   -- compiler warning
      dumpLocations ns =
        let tm = fromJust $ retrieveTargetMachine $ pmTarget matchset
            mkLocInfo l = pShow
                          $ fromJust
                          $ findLocation (tmLocations tm) l
        in concatMap (\(ai, m) -> pShow ai ++ " -> " ++ mkLocInfo m ++ "\n")
                     (zip ([0..] :: [Integer]) ns) -- Cast needed to prevent
                                                   -- compiler warning
      dumpInstructions ns =
        let mkInstrInfo i = "Instruction ID " ++ pShow i
        in concatMap (\(ai, i) -> pShow ai ++ " -> " ++ mkInstrInfo i ++ "\n")
                     (zip ([0..] :: [Integer]) ns) -- Cast needed to prevent
                                                   -- compiler warning

  in do return [ toOutput
                 $ "OPERATIONS" ++ "\n" ++
                   (dumpNodes $ ai2OperationNodeIDs ai_maps)
                   ++ "\n"
                   ++ "DATA" ++ "\n" ++
                   (dumpNodes $ ai2DatumNodeIDs ai_maps)
                   ++ "\n"
                   ++ "BLOCKS" ++ "\n" ++
                   (dumpNodes $ ai2BlockNodeIDs ai_maps)
                   ++ "\n"
                   ++ "MATCHES" ++ "\n" ++
                   (dumpMatches $ ai2MatchIDs ai_maps)
                   ++ "\n"
                   ++ "LOCATIONS" ++ "\n" ++
                   (dumpLocations $ ai2LocationIDs ai_maps)
                   ++ "\n"
                   ++ "INSTRUCTIONS" ++ "\n" ++
                   (dumpInstructions $ ai2InstructionIDs ai_maps)
               ]

run _ _ _ _ =
  reportErrorAndExit "MakeDumpFromArrayIndexMaplists: unsupported action"
