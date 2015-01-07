--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ProgramModules.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types and records for representing program modules, which
-- basically consist of a list of functions. This is the format on which
-- subsequent preparation for instruction selection will build on (i.e. other
-- programs forms, such as those based on LLVM, will be converted into this
-- format).
--
-- Since only the function name is retained, the names of overloaded functions
-- must have been resolved such that each is given a unique name.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.InstSel.ProgramModules.Base
  ( module Language.InstSel.ProgramModules.IDs
  , ExecFreq (..)
  , Function (..)
  , fromExecFreq
  , getExecFreqOfBBInFunction
  , toExecFreq
  )
where

import Language.InstSel.Graphs
  ( NodeID )
import Language.InstSel.OpStructures
import Language.InstSel.ProgramModules.IDs
import Language.InstSel.Utils
  ( Natural
  , toNatural
  )



--------------
-- Data types
--------------

-- | Represents the execution frequency of a basic block.
newtype ExecFreq =
  ExecFreq Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show ExecFreq where
  show (ExecFreq i) = show i

-- | The record of representing a program function.
data Function =
    Function
      { functionName :: Maybe String
        -- ^ The function name.
      , functionOS :: OpStructure
        -- ^ The semantics of the function.
      , functionInputs :: [NodeID]
        -- ^ The IDs of the data nodes in the operation structure which
        -- represent the function input arguments. The order of the list is the
        -- same as the order specified in the original code from which the
        -- semantics have been derived.
      , functionBBExecFreq :: [(BasicBlockLabel, ExecFreq)]
        -- ^ The execution frequency of the basic blocks.
      }
  deriving (Show)



-------------
-- Functions
-------------

-- | Gets the execution frequency of a given basic block in a function.
getExecFreqOfBBInFunction :: Function -> BasicBlockLabel -> Maybe ExecFreq
getExecFreqOfBBInFunction f bb =
  let matching_data = filter ((bb ==) . fst) (functionBBExecFreq f)
  in if length matching_data > 0
     then Just $ snd $ head matching_data
     else Nothing

fromExecFreq :: ExecFreq -> Natural
fromExecFreq (ExecFreq i) = i

toExecFreq :: (Integral i) => i -> ExecFreq
toExecFreq = ExecFreq . toNatural
