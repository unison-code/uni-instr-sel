--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
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

module Language.InstSel.Drivers.Base
  ( CheckAction (..)
  , MakeAction (..)
  , Options (..)
  , Output (..)
  , PlotAction (..)
  , TransformAction (..)
  , toOutput
  , toOutputWithoutID
  )
where

import System.Console.CmdArgs
  ( Data
  , Typeable
  )


--------------
-- Data types
--------------

data Options
  = Options
      { command :: String
      , functionFile :: Maybe String
      , solutionFile :: Maybe String
      , postFile :: Maybe String
      , outFile :: Maybe String
      , targetName :: Maybe String
      , matchsetFile :: Maybe String
      , makeAction :: MakeAction
      , transformAction :: TransformAction
      , plotAction :: PlotAction
      , checkAction :: CheckAction
      }
  deriving (Data, Typeable)

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: String
        -- ^ A unique string that identifies this output. If the output is to be
        -- written to file, the ID will be suffixed to the file name.
      , oData :: String
        -- ^ The produced output.
      }

data MakeAction
  = MakeNothing
  | MakeFunctionGraphFromLLVM
  deriving (Eq, Typeable, Data)

data TransformAction
  = TransformNothing
  | CopyExtendFunctionGraph
  | BranchExtendFunctionGraph
  deriving (Eq, Typeable, Data)

data PlotAction
  = PlotNothing
  | PlotFunctionGraph
  | PlotFunctionCFG
  | PlotFunctionSSA
  | PlotPatternGraph
  | PlotPatternCFG
  | PlotPatternSSA
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
toOutputWithoutID = toOutput ""

-- | Creates an output.
toOutput
  :: String
     -- ^ The ID.
  -> String
     -- ^ The output string.
  -> Output
toOutput oid s = Output { oID = oid, oData = s }
