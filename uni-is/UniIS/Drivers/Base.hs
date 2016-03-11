--------------------------------------------------------------------------------
-- |
-- Module      : UniIS.Drivers.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains data types for drivers.
--
--------------------------------------------------------------------------------

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
      , checkAction :: CheckAction
      }
  deriving (Data, Typeable)

data MakeAction
  = MakeNothing
  | MakeFunctionGraphFromLLVM
  | MakePatternMatchset
  | MakeArrayIndexMaplists
  | MakeDumpFromArrayIndexMaplists
  | MakeHighLevelCPModel
  | MakeAssemblyCode
  deriving (Eq, Typeable, Data)

data TransformAction
  = TransformNothing
  | CopyExtendFunctionGraph
  | BranchExtendFunctionGraph
  | CombineConstantsInFunctionGraph
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
  deriving (Eq, Typeable, Data)
