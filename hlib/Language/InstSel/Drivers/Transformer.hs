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

import Language.InstSel.Graphs.Transformations
import Language.InstSel.OpStructures
  ( OpStructure (..) )
import Language.InstSel.ProgramModules
  ( Function (..) )

import Language.InstSel.Utils.JSON

import System.Console.CmdArgs
  ( Data
  , Typeable
  )



--------------
-- Data types
--------------

data TransformAction
  = CopyExtendFunction
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
  do function <- getFunction str
     let os = functionOS function
         g = osGraph os
         new_g = copyExtendEverywhere g
         new_os = os { osGraph = new_g }
         new_function = function { functionOS = new_os }
     return [toOutputWithoutID $ toJson new_function]
