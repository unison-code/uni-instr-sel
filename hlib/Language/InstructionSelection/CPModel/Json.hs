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

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
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
  ( BBLabel (..)
  , Domset (..)
  , NodeID
  , fromNodeID
  , toNodeID
  )
import Language.InstructionSelection.Patterns.IDs
import Language.InstructionSelection.TargetMachine.IDs
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
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import Data.Maybe
  ( fromJust
  , isJust
  )
import Data.Scientific (Scientific)
import qualified Data.Text as T (unpack)



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
    <*> v .: "bb-labels"
    <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON FunctionGraphData where
  toJSON d =
    object [ "action-nodes" .= (funcActionNodes d)
           , "data-nodes"   .= (funcDataNodes d)
           , "state-nodes"  .= (funcStateNodes d)
           , "label-nodes"  .= (funcLabelDoms d)
           , "root-label"   .= (funcRootLabel d)
           , "bb-labels"    .= (funcBBLabels d)
           , "constraints"  .= (funcConstraints d)
           ]

instance FromJSON (Domset NodeID) where
  parseJSON (Object v) =
    Domset
    <$> v .: "node"
    <*> v .: "domset"
  parseJSON _ = mzero

instance ToJSON (Domset NodeID) where
  toJSON d =
    object [ "node"   .= (domNode d)
           , "domset" .= (domSet d)
           ]

instance FromJSON BBLabelData where
  parseJSON (Object v) =
    BBLabelData
    <$> v .: "node"
    <*> v .: "label"
  parseJSON _ = mzero

instance ToJSON BBLabelData where
  toJSON d =
    object [ "node" .= (labNode d)
           , "label" .= (labBB d)
           ]

instance FromJSON PatternInstanceData where
  parseJSON (Object v) =
    PatternInstanceData
    <$> v .: "instruction-id"
    <*> v .: "pattern-id"
    <*> v .: "pattern-instance-id"
    <*> v .: "action-nodes-covered"
    <*> v .: "data-nodes-defined"
    <*> v .: "data-nodes-used"
    <*> v .: "data-nodes-used-by-phis"
    <*> v .: "state-nodes-defined"
    <*> v .: "state-nodes-used"
    <*> v .: "label-nodes-referred"
    <*> v .: "constraints"
    <*> v .: "apply-use-def-dom-constraints"
    <*> v .: "code-size"
    <*> v .: "latency"
    <*> v .: "assembly-id-maps"
  parseJSON _ = mzero

instance ToJSON PatternInstanceData where
  toJSON d =
    object ([ "instruction-id"                .= (patInstructionID d)
            , "pattern-id"                    .= (patPatternID d)
            , "pattern-instance-id"           .= (patInstanceID d)
            , "action-nodes-covered"          .= (patActionNodesCovered d)
            , "data-nodes-defined"            .= (patDataNodesDefined d)
            , "data-nodes-used"               .= (patDataNodesUsed d)
            , "data-nodes-used-by-phis"       .= (patDataNodesUsedByPhis d)
            , "state-nodes-defined"           .= (patStateNodesDefined d)
            , "state-nodes-used"              .= (patStateNodesUsed d)
            , "label-nodes-referred"          .= (patLabelNodesReferred d)
            , "constraints"                   .= (patConstraints d)
            , "apply-use-def-dom-constraints" .= (patAUDDC d)
            , "code-size"                     .= (patCodeSize d)
            , "latency"                       .= (patLatency d)
            , "assembly-id-maps"              .= (patAssIDMaps d)
            ])

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
  parseJSON (String vs) =
    do let s = T.unpack vs
           res = fromLispExpr s
       when (isLeft res) $ fail $ fromLeft res
       return (fromRight res)
  parseJSON _ = mzero

instance ToJSON Constraint where
  toJSON = toJSON . toLispExpr

instance FromJSON NodeID where
  parseJSON (Number sn) = return $ toNodeID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON NodeID where
  toJSON nid = toJSON (fromNodeID nid)

instance FromJSON InstructionID where
  parseJSON (Number sn) = return $ toInstructionID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON InstructionID where
  toJSON iid = toJSON (fromInstructionID iid)

instance FromJSON PatternID where
  parseJSON (Number sn) = return $ toPatternID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON PatternID where
  toJSON iid = toJSON (fromPatternID iid)

instance FromJSON PatternInstanceID where
  parseJSON (Number sn) = return $ toPatternInstanceID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON PatternInstanceID where
  toJSON iid = toJSON (fromPatternInstanceID iid)

instance FromJSON RegisterID where
  parseJSON (Number sn) = return $ toRegisterID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON RegisterID where
  toJSON rid = toJSON (fromRegisterID rid)

instance FromJSON Natural where
  parseJSON (Number sn) = return $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)

instance FromJSON BBLabel where
  parseJSON (String s) = return $ (BBLabel $ T.unpack s)
  parseJSON _ = mzero

instance ToJSON BBLabel where
  toJSON (BBLabel s) = toJSON s

instance FromJSON RawCPSolutionData where
  parseJSON (Object v) =
    RawCPSolutionData
    <$> v .: "bb-allocated-for-pi"
    <*> v .: "is-pi-selected"
    <*> v .: "order-of-bbs"
    <*> v .: "has-dnode-reg"
    <*> v .: "reg-selected-for-dnode"
    <*> v .: "has-dnode-imm-value"
    <*> v .: "imm-value-of-dnode"
  parseJSON _ = mzero

instance FromJSON RawPostParams where
  parseJSON (Object v) =
    RawPostParams
    <$> v .: "model-params"
    <*> ((v .: "array-index-to-id-maps") >>= (.: "pattern-instances"))
    <*> ((v .: "array-index-to-id-maps") >>= (.: "label-nodes"))
    <*> ((v .: "array-index-to-id-maps") >>= (.: "data-nodes"))
  parseJSON _ = mzero



-------------
-- Functions
-------------

-- | Converts a scientific number to a natural number. If the number is not an
-- non-negative then an error occurs.

sn2nat :: Scientific -> Natural
sn2nat sn =
  let int_value = round sn
  in if fromInteger int_value /= sn
     then error "not an integer"
     else toNatural int_value

-- | Parses a JSON string into an entity.

fromJson :: FromJSON a =>
            String
            -> Either String -- ^ The error message, if the parsing failed.
                      a      -- ^ The entity, if the parsing was successful.
fromJson s =
  let result = decode (BS.pack s)
  in if isJust result
        then Right (fromJust result)
        else Left ("failed to parse JSON")

-- | Converts an entity into a JSON string.

toJson :: ToJSON a => a -> String
toJson = BS.unpack . encode
