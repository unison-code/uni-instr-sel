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
import Language.InstructionSelection.Patterns (PatternId)
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
  toJSON (CPModelParams func pats m) =
    object [ "function"  .= func
           , "patterns" .= pats
           , "machine"  .= m
           ]

instance ToJSON FunctionGraphData where
  toJSON d =
    object [ "nodes"          .= (funcNodes d)
           , "label-doms"     .= (funcLabelDoms d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           ]

instance ToJSON PatternGraphData where
  toJSON d =
    object [ "id"               .= (patId d)
           , "code-size"        .= (patCodeSize d)
           , "latency"          .= (patLatency d)
           , "nodes"            .= (patNodes d)
           , "data-nodes-info"  .= (patDataUseDefs d)
           , "label-nodes-info" .= (patLabelUseDefs d)
           , "state-nodes-info" .= (patStateUseDefs d)
-- TODO: enable
--         , "constraints" .= (funcConstraints p)
           , "matchsets" .= map f (patMatchsets d)
           ]
    where f (matchset, id) = object [ "id" .= id
                                    , "mappings" .= matchset
                                    ]

instance ToJSON MachineData where
  toJSON d =
    object [ -- TODO: implement
           ]

instance ToJSON UseDefNodes where
  toJSON ns =
    object [ "uses" .= (useNodes ns)
           , "defs" .= (defNodes ns)
           ]

instance ToJSON NodePartition where
  toJSON np =
    object [ "computation-nodes" .= (computationNodes np)
           , "control-nodes" .= (controlNodes np)
           , "data-nodes" .= (dataNodes np)
           , "label-nodes" .= (labelNodes np)
           , "phi-nodes" .= (phiNodes np)
           , "state-nodes" .= (stateNodes np)
           , "transfer-nodes" .= (transferNodes np)
           ]

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)
