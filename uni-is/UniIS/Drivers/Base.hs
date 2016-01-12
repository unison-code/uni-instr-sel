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
  , Output (..)
  , emitToStdout
  , emitToFile
  , toOutput
  , toOutputWithoutID
  )
where

import Data.Maybe
  ( fromJust
  , isJust
  )

import System.Console.CmdArgs
  ( Data
  , Typeable
  )

import System.FilePath.Posix
  ( splitExtension )



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

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: Maybe String
        -- ^ A unique string that identifies this output. If the output is to be
        -- written to file, the ID will be suffixed to the file name.
      , oData :: String
        -- ^ The produced output.
      }

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
  deriving (Eq, Typeable, Data)



-------------
-- Functions
-------------

-- | Creates an output that has no ID. This is useful when there is exactly one
-- output produced.
toOutputWithoutID
  :: String
     -- ^ The output string.
  -> Output
toOutputWithoutID = toOutput' Nothing

-- | Creates an output.
toOutput
  :: String
     -- ^ The ID.
  -> String
     -- ^ The output string.
  -> Output
toOutput oid s = toOutput' (Just oid) s

toOutput' :: Maybe String -> String -> Output
toOutput' oid s = Output { oID = oid, oData = s }

-- | Emits output to 'STDOUT'.
emitToStdout :: Output -> IO ()
emitToStdout = putStrLn . oData

-- | Emits output to a file of a given name and the output ID suffixed.
emitToFile :: FilePath -> Output -> IO ()
emitToFile fp o =
  let (fname, ext) = splitExtension fp
      oid = oID o
      filename =
        fname ++ (if isJust oid then "." ++ fromJust oid else "") ++ ext
  in writeFile filename (oData o)
