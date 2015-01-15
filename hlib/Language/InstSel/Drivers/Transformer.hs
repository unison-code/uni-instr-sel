--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Transformer
-- Copyright   : (c) Gabriel Hjort Blindell 2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Used for transforming the input.
--
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Language.InstSel.Drivers.Transformer
  ( TransformAction (..)
  , run
  )
where

import Language.InstSel.Drivers.Base
import Language.InstSel.Functions.Transformations

import System.Console.CmdArgs
  ( Data
  , Typeable
  )



--------------
-- Data types
--------------

data TransformAction
  = CopyExtendFunction
  | BranchExtendFunction
  | DoNothing
  deriving (Typeable, Data)



-------------
-- Functions
-------------

run
  :: String
     -- ^ The content of the function graph.
  -> TransformAction
     -- ^ The action to perform.
  -> IO [Output]
     -- ^ The produced output.

run _ DoNothing = reportError "No transform action provided."

run str CopyExtendFunction =
  do f <- parseJson str
     let new_f = copyExtend f
     return [toOutputWithoutID $ toJson new_f]

run str BranchExtendFunction =
  do f <- parseJson str
     let new_f = branchExtend f
     return [toOutputWithoutID $ toJson new_f]
