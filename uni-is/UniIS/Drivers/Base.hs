{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE DeriveDataTypeable #-}

module UniIS.Drivers.Base
  ( CheckAction (..)
  , MakeAction (..)
  , PlotAction (..)
  , TransformAction (..)
  , Options (..)
  , module Language.InstrSel.DriverTools
  )
where

import System.Console.CmdArgs
  ( Data
  , Typeable
  )

import Language.InstrSel.DriverTools



--------------
-- Data types
--------------

-- | Options that can be given on the command line.
data Options
  = Options
      { command :: String
      , functionFile :: Maybe String
      , patternMatchsetFile :: Maybe String
      , modelFile :: Maybe String
      , arrayIndexMaplistsFile :: Maybe String
      , solutionFile :: Maybe String
      , targetName :: Maybe String
      , instructionID :: Maybe Integer
      , outFile :: Maybe String
      , makeAction :: MakeAction
      , transformAction :: TransformAction
      , plotAction :: PlotAction
      , showEdgeNumbers :: Maybe Bool
      , hideNullInstructions :: Maybe Bool
      , hideKillInstructions :: Maybe Bool
      , altLimit :: Maybe Int
      , checkAction :: CheckAction
      }
  deriving (Data, Typeable)

-- | Represents 'make' actions.
data MakeAction
  = MakeNothing
  | MakeFunctionGraphFromLLVM
  | MakePatternMatchset
  | MakeArrayIndexMaplists
  | MakeLowLevelModelDump
  | MakeLowLevelSolutionDump
  | MakeHighLevelCPModel
  | MakeAssemblyCode
  deriving (Eq, Typeable, Data)

-- | Represents 'transform' actions.
data TransformAction
  = TransformNothing
  | RemoveRedundantConversionsInFunctionGraph
  | RemoveDeadCodeInFunctionGraph
  | EnforcePhiNodeInvariantsInFunctionGraph
  | RemovePhiNodeRedundanciesInFunctionGraph
  | LowerPointersInFunctionGraph
  | CopyExtendFunctionGraph
  | CombineConstantsInFunctionGraph
  | AlternativeExtendPatternMatchset
  | AddOperandsToHighLevelCPModel
  | RaiseLowLevelCPSolution
  | LowerHighLevelCPModel
  deriving (Eq, Typeable, Data)

-- | Represents 'plot' actions.
data PlotAction
  = PlotNothing
  | PlotFunctionFullGraph
  | PlotFunctionControlFlowGraph
  | PlotFunctionSSAGraph
  | PlotPatternFullGraph
  | PlotPatternControlFlowGraph
  | PlotPatternSSAGraph
  | PlotCoverAllMatches
  | PlotCoverPerMatch
  deriving (Eq, Typeable, Data)

-- | Represents 'check' actions.
data CheckAction
  = CheckNothing
  | CheckFunctionGraphCoverage
  | CheckFunctionGraphLocationOverlap
  | CheckFunctionIntegrity
  | CheckPatternIntegrity
  deriving (Eq, Typeable, Data)
