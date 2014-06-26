--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.CPModel.Json
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts the parameter data structure in and out of JSON format.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstructionSelection.CPModel.Json
  ( fromJson
  , toJson
  )
where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Constraints.SExpressions
import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs
  ( NodeID
  , fromNodeID
  )
import Language.InstructionSelection.Patterns
  ( PatternInstanceID
  , fromPatternInstanceID
  )
import Language.InstructionSelection.TargetMachine
  ( RegisterID
  , fromRegisterID
  )
import Language.InstructionSelection.Utils
  ( Natural (..)
  , fromNatural
  )
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)



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
           , "data-nodes"   .= (funcDataNodes d)
           , "state-nodes"  .= (funcStateNodes d)
           , "label-nodes"  .= map f (funcLabelDoms d)
           , "root-label"   .= (funcRootLabel d)
           , "constraints"  .= (funcConstraints d)
           ]
    where f (nid, domset) = object [ "node"   .= nid
                                   , "domset" .= domset
                                   ]

instance ToJSON PatternInstanceData where
  toJSON d =
    object ([ "pattern-instance-id"  .= (patInstanceID d)
            , "action-nodes-covered" .= (patActionNodesCovered d)
            , "data-nodes-defined"   .= (patDataNodesDefined d)
            , "data-nodes-used"      .= (patDataNodesUsed d)
            , "state-nodes-defined"  .= (patStateNodesDefined d)
            , "state-nodes-used"     .= (patStateNodesUsed d)
            , "label-nodes-referred" .= (patLabelNodesReferred d)
            , "code-size"            .= (patCodeSize d)
            , "latency"              .= (patLatency d)
            , "constraints"          .= (patConstraints d)
            ]
            ++ if patNoUseDefConstraints d
                  then ["no-use-def-dom-constraints" .= True]
                  else []
           )

instance ToJSON MachineData where
  toJSON d =
    object [ "registers" .= (machRegisters d)
           ]

instance ToJSON Constraint where
  toJSON = toJSON . toLispExpr

instance ToJSON NodeID where
  toJSON nid = toJSON (fromNodeID nid)

instance ToJSON PatternInstanceID where
  toJSON iid = toJSON (fromPatternInstanceID iid)

instance ToJSON RegisterID where
  toJSON rid = toJSON (fromRegisterID rid)

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)



-------------
-- Functions
-------------

-- | Parses a JSON string into a 'CPModelParams'.

fromJson :: CPModelParams -> String
fromJson _ = "not implemented"
-- TODO: implement



-- | Converts a 'CPModelParams' into a JSON string.

toJson :: CPModelParams -> String
toJson = unpack . encode
