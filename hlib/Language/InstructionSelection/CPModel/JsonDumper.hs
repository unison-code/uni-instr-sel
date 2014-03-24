--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.JsonDumper
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
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
import Language.InstructionSelection.Graphs ( NodeId (..)
                                            , Matchset (..)
                                            )
import Language.InstructionSelection.Patterns (InstanceId (..))
import Language.InstructionSelection.Utils ( Natural (..)
                                           , fromNatural
                                           )
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
    object [ "function-data"         .= (funcData p)
           , "pattern-instance-data" .= (patInstData p)
           , "machine-data"          .= (machData p)
           ]

instance ToJSON FunctionGraphData where
  toJSON d =
    object [ "action-nodes" .= (funcActionNodes d)
           , "entity-nodes" .= (funcEntityNodes d)
           , "label-nodes"  .= map f (funcLabelDoms d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           ]
    where f (id, domset) = object [ "node"   .= id
                                  , "domset" .= domset
                                  ]

instance ToJSON PatternInstanceData where
  toJSON d =
    object [ "instance-id"          .= (patInstanceId d)
           , "action-nodes-covered" .= (patCoveredActionNodes d)
           , "entity-nodes-defined" .= (patDefinedEntityNodes d)
           , "entity-nodes-used"    .= (patUsedEntityNodes d)
           , "cost"                 .= (patCost d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           ]

instance ToJSON MachineData where
  toJSON d =
    object [ -- TODO: implement
           ]

instance ToJSON NodeId where
  toJSON (NodeId i) = toJSON i

instance ToJSON InstanceId where
  toJSON (InstanceId i) = toJSON i

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)
