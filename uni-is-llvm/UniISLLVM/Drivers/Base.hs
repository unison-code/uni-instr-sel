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

module UniISLLVM.Drivers.Base
  ( MakeAction (..)
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
      , outFile :: Maybe String
      , makeAction :: MakeAction
      }
  deriving (Data, Typeable)

-- | Represents 'make' actions.
data MakeAction
  = MakeNothing
  | MakeFunctionGraphFromLLVM
  deriving (Eq, Typeable, Data)
