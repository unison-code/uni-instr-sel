--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Functions.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types and records for representing functions. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other programs forms, such as those based on LLVM, will be converted
-- into this format).
--
-- Since only the function name is retained, the names of overloaded functions
-- must have been resolved such that each is given a unique name.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.Functions.Base
  ( ExecFreq (..)
  , Function (..)
  , fromExecFreq
  , toExecFreq
  )
where

import Language.InstrSel.Graphs
  ( NodeID )
import Language.InstrSel.OpStructures
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Utils
  ( Natural
  , toNatural
  )
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Represents the execution frequency of a basic block.
newtype ExecFreq
  = ExecFreq Natural
  deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show ExecFreq where
  show (ExecFreq i) = show i

-- | The record of representing a program function.
data Function
  = Function
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



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON Function where
  parseJSON (Object v) =
    Function
      <$> v .: "name"
      <*> v .: "op-struct"
      <*> v .: "inputs"
      <*> v .: "bb-exec-freqs"
  parseJSON _ = mzero

instance ToJSON Function where
  toJSON f =
    object [ "name"          .= (functionName f)
           , "op-struct"     .= (functionOS f)
           , "inputs"        .= (functionInputs f)
           , "bb-exec-freqs" .= (functionBBExecFreq f)
           ]

instance FromJSON ExecFreq where
  parseJSON (Number sn) = return $ toExecFreq $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON ExecFreq where
  toJSON i = toJSON (fromExecFreq i)



-------------
-- Functions
-------------

fromExecFreq :: ExecFreq -> Natural
fromExecFreq (ExecFreq i) = i

toExecFreq :: (Integral i) => i -> ExecFreq
toExecFreq = ExecFreq . toNatural
