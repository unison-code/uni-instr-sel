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
  , toNodeID
  )
import Language.InstructionSelection.Patterns
  ( PatternInstanceID
  , fromPatternInstanceID
  , toPatternInstanceID
  )
import Language.InstructionSelection.TargetMachine
  ( RegisterID
  , fromRegisterID
  , toRegisterID
  )
import Language.InstructionSelection.Utils
  ( Natural (..)
  , fromLeft
  , fromNatural
  , fromRight
  , isLeft
  , toNatural
  )
import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Control.Monad
  ( mzero
  , when
  )
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.Number (Number (..))
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import Data.Maybe
  ( fromJust
  , isJust
  )



------------------------
-- Type class instances
------------------------

instance FromJSON CPModelParams where
  parseJSON (Object v) =
    CPModelParams
    <$> v .: "function-data"
    <*> v .: "pattern-instance-data"
    <*> v .: "machine-data"
  parseJSON _ = mzero

instance ToJSON CPModelParams where
  toJSON p =
    object [ "function-data"         .= (funcData p)
           , "pattern-instance-data" .= (patInstData p)
           , "machine-data"          .= (machData p)
           ]

instance FromJSON FunctionGraphData where
  parseJSON (Object v) =
    FunctionGraphData
    <$> v .: "action-nodes"
    <*> v .: "data-nodes"
    <*> v .: "state-nodes"
    <*> v .: "label-nodes"
    <*> v .: "root-label"
    <*> v .: "constraints"
  parseJSON _ = mzero

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

instance FromJSON PatternInstanceData where
  parseJSON (Object v) =
    PatternInstanceData
    <$> v .:  "pattern-instance-id"
    <*> v .:  "action-nodes-covered"
    <*> v .:  "data-nodes-defined"
    <*> v .:  "data-nodes-used"
    <*> v .:  "state-nodes-defined"
    <*> v .:  "state-nodes-used"
    <*> v .:  "label-nodes-referred"
    <*> v .:  "constraints"
    <*> v .:? "no-use-def-dom-constraints" .!= False
    <*> v .:  "code-size"
    <*> v .:  "latency"
  parseJSON _ = mzero

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

instance FromJSON MachineData where
  parseJSON (Object v) =
    MachineData
    <$> v .: "registers"
  parseJSON _ = mzero

instance ToJSON MachineData where
  toJSON d =
    object [ "registers" .= (machRegisters d)
           ]

instance FromJSON Constraint where
  parseJSON (String s) =
    do let res = fromLispExpr $ show s
       when (isLeft res) $ fail $ fromLeft res
       return (fromRight res)
  parseJSON _ = mzero

instance ToJSON Constraint where
  toJSON = toJSON . toLispExpr

instance FromJSON NodeID where
  parseJSON (Number n) =
    do when (n <= 0) $ fail "number is not a node ID"
       return (toNodeID n)
  parseJSON _ = mzero

instance ToJSON NodeID where
  toJSON nid = toJSON (fromNodeID nid)

instance FromJSON PatternInstanceID where
  parseJSON (Number i) =
    do when (i <= 0) $ fail "number is not a pattern instance ID"
       return (toPatternInstanceID i)
  parseJSON _ = mzero

instance ToJSON PatternInstanceID where
  toJSON iid = toJSON (fromPatternInstanceID iid)

instance FromJSON RegisterID where
  parseJSON (Number i) =
    do when (i <= 0) $ fail "number is not a register ID"
       return (toRegisterID i)
  parseJSON _ = mzero

instance ToJSON RegisterID where
  toJSON rid = toJSON (fromRegisterID rid)

instance FromJSON Natural where
  parseJSON (Number (I i)) =
    do when (i <= 0) $ fail "number is not a natural"
       return (toNatural i)
  parseJSON _ = mzero

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)



-------------
-- Functions
-------------

-- | Parses a JSON string into a 'CPModelParams'.

fromJSON :: String
            -> Either String        -- ^ Contains the error message, if the
                                    -- parsing failed.
                      CPModelParams -- ^ Contains the parameters, if the
                                    -- parsing was successful.
fromJSON s =
  let result = decode (BS.pack s)
  in if isJust result
        then Left ("failed to parse JSON")
        else Right (fromJust result)

-- | Converts a 'CPModelParams' into a JSON string.

toJson :: CPModelParams -> String
toJson = BS.unpack . encode
