--------------------------------------------------------------------------------
-- |
-- Module      : UniTarGen.Drivers.Base
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

module UniTarGen.Drivers.Base
  ( Options (..)
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
      { machDescFile :: Maybe String
      , outDir :: Maybe String
      }
  deriving (Data, Typeable)
