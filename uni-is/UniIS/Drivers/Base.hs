{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
      , patternID :: Maybe Integer
      , outFile :: Maybe String
      , makeAction :: MakeAction
      , transformAction :: TransformAction
      , plotAction :: PlotAction
      , hideInactiveInstructions :: Maybe Bool
      , checkAction :: CheckAction
      }
  deriving (Data, Typeable)

data MakeAction
  = MakeNothing
  | MakeFunctionGraphFromLLVM
  | MakePatternMatchset
  | MakeArrayIndexMaplists
  | MakeLowLevelModelDump
  | MakeHighLevelCPModelNoOp
  | MakeHighLevelCPModelWOp
  | MakeAssemblyCode
  deriving (Eq, Typeable, Data)

data TransformAction
  = TransformNothing
  | CopyExtendFunctionGraph
  | BranchExtendFunctionGraph
  | CombineConstantsInFunctionGraph
  | AlternativeExtendFunctionGraph
  | AddOperandsToHighLevelCPModel
  | RaiseLowLevelCPSolution
  | LowerHighLevelCPModel
  deriving (Eq, Typeable, Data)

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

data CheckAction
  = CheckNothing
  | CheckFunctionGraphCoverage
  | CheckFunctionGraphLocationOverlap
  deriving (Eq, Typeable, Data)
