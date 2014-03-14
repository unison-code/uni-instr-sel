--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.JsonDumper
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Dumps a CP model parameter data structure in a JSON format.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.InstructionSelection.CPModel.JsonDumper (
  toJson
) where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs ( MatchsetId
                                            , NodeId
                                            , NodeIdMatchset)
import Language.InstructionSelection.Patterns (InstProperties (..))
import Language.InstructionSelection.Utils (Natural, fromNatural)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)



-------------
-- Functions
-------------

toJson :: CPModelParams -> String
toJson = unpack . encode



------------------------
-- Type class instances
------------------------

instance ToJSON CPModelParams where
  toJSON p =
    object [ "function-data"  .= (funcData p)
           , "pattern-instance-data" .= (patInstData p)
           , "instruction-data"  .= (instrData p)
           , "machine-data"  .= (machData p)
           ]

instance ToJSON FunctionGraphData where
  toJSON d =
    object [ "action-nodes"  .= (funcActionNodes d)
           , "entity-nodes"  .= (funcEntityNodes d)
           , "label-nodes"   .= (funcLabelNodes d)
           , "label-domsets" .= map f (funcLabelDoms d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           ]
    where f (id, domset) = object [ "dominated-id" .= id
                                  , "domset"       .= domset
                                  ]

instance ToJSON PatternInstanceData where
  toJSON d =
    object [ "matchset-id"              .= (patMatchsetId d)
           , "action-nodes-covered"     .= (patCoveredActionNodes d)
           , "entity-nodes-defined"     .= (patDefinedEntityNodes d)
           , "entity-nodes-used"        .= (patUsedEntityNodes d)
           , "label-nodes-internalized" .= (patInternalizedLabelNodes d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           ]

instance ToJSON InstructionData where
  toJSON d =
    object [ "code-size" .= (codeSize $ instrProps d)
           , "latency"   .= (latency $ instrProps d)
           , "matchsets" .= (instrMatchsetIds d)
           ]

instance ToJSON MachineData where
  toJSON d =
    object [ -- TODO: implement
           ]

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)
