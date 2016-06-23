{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

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
import Language.InstrSel.PrettyShow
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Represents the execution frequency of a block.
newtype ExecFreq
  = ExecFreq Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow ExecFreq where
  pShow (ExecFreq i) = pShow i

-- | The record of representing a program function.
data Function
  = Function
      { functionName :: Maybe String
        -- ^ The function name.
      , functionOS :: OpStructure
        -- ^ The semantics of the function.
      , functionInputs :: [NodeID]
        -- ^ The IDs of the value nodes in the operation structure which
        -- represent the function input arguments. The order of the list is the
        -- same as the order specified in the original code from which the
        -- semantics have been derived.
      , functionBBExecFreq :: [(BlockName, ExecFreq)]
        -- ^ The execution frequency of the blocks.
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
