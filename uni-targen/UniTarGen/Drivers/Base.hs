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

module UniTarGen.Drivers.Base
  ( module Language.InstrSel.DriverTools
  , Options (..)
  , defaultMaxInstructionsPerSubModule
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
      { machDescFile :: Maybe String
      , outDir :: Maybe String
      , parentModule :: Maybe String
      , maxInstructionsPerSubModule :: Maybe Int
      , prettyPrint :: Maybe Bool
      }
  deriving (Data, Typeable)



-------------
-- Functions
-------------

-- | Returns maximum number of instructions allowed per target-machine
-- submodule.
defaultMaxInstructionsPerSubModule :: Int
defaultMaxInstructionsPerSubModule = 1000
