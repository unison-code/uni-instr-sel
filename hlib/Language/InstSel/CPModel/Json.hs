--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.Json
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

module Language.InstSel.CPModel.Json
  ( fromJson
  , toJson
  )
where

import Language.InstSel.Constraints
import Language.InstSel.Constraints.SExpressions
import Language.InstSel.CPModel.Base
import Language.InstSel.Graphs
  ( Domset (..) )
import Language.InstSel.Graphs.IDs
import Language.InstSel.Patterns.IDs
import Language.InstSel.ProgramModules.IDs
import Language.InstSel.ProgramModules
  ( ExecFreq (..)
  , fromExecFreq
  , toExecFreq
  )
import Language.InstSel.TargetMachine.IDs
import Language.InstSel.Utils
  ( Natural
  , fromLeft
  , fromNatural
  , fromRight
  , isLeft
  , replace
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
import Data.Scientific
  ( Scientific )
import qualified Data.Text as T
  ( unpack )



------------------------
-- Type class instances
------------------------

instance FromJSON CPModelParams where
  parseJSON (Object v) =
    CPModelParams
      <$> v .: "function-data"
      <*> v .: "match-data"
      <*> v .: "machine-data"
  parseJSON _ = mzero

instance ToJSON CPModelParams where
  toJSON p =
    object [ "function-data" .= (functionData p)
           , "match-data"    .= (matchData p)
           , "machine-data"  .= (machineData p)
           ]

instance FromJSON FunctionGraphData where
  parseJSON (Object v) =
    FunctionGraphData
      <$> v .: "operation-nodes"
      <*> v .: "essential-op-nodes"
      <*> v .: "data-nodes"
      <*> v .: "state-nodes"
      <*> v .: "label-nodes"
      <*> v .: "def-place-edges"
      <*> v .: "root-label"
      <*> v .: "bb-data"
      <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON FunctionGraphData where
  toJSON d =
    object [ "operation-nodes"    .= (funcOpNodes d)
           , "essential-op-nodes" .= (funcEssentialOpNodes d)
           , "data-nodes"         .= (funcDataNodes d)
           , "state-nodes"        .= (funcStateNodes d)
           , "label-nodes"        .= (funcLabelNodes d)
           , "def-place-edges"    .= (funcDefPlaceEdges d)
           , "root-label"         .= (funcRootLabel d)
           , "bb-data"            .= (funcBasicBlockData d)
           , "constraints"        .= (funcConstraints d)
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

instance FromJSON BasicBlockData where
  parseJSON (Object v) =
    BasicBlockData
      <$> v .: "label"
      <*> v .: "label-node"
      <*> v .: "exec-frequency"
  parseJSON _ = mzero

instance ToJSON BasicBlockData where
  toJSON d =
    object [ "label"          .= (bbLabel d)
           , "label-node"     .= (bbLabelNode d)
           , "exec-frequency" .= (bbExecFrequency d)
           ]

instance FromJSON MatchData where
  parseJSON (Object v) =
    MatchData
      <$> v .: "instruction-id"
      <*> v .: "pattern-id"
      <*> v .: "match-id"
      <*> v .: "operation-nodes-covered"
      <*> v .: "data-nodes-defined"
      <*> v .: "data-nodes-used"
      <*> v .: "data-nodes-used-by-phis"
      <*> v .: "state-nodes-defined"
      <*> v .: "state-nodes-used"
      <*> v .: "root-label-node"
      <*> v .: "non-root-label-nodes"
      <*> v .: "constraints"
      <*> v .: "apply-def-dom-use-constraint"
      <*> v .: "has-control-nodes"
      <*> v .: "code-size"
      <*> v .: "latency"
      <*> v .: "assembly-id-maps"
  parseJSON _ = mzero

instance ToJSON MatchData where
  toJSON d =
    object [ "instruction-id"               .= (mInstructionID d)
           , "pattern-id"                   .= (mPatternID d)
           , "match-id"                     .= (mMatchID d)
           , "operation-nodes-covered"      .= (mOperationsCovered d)
           , "data-nodes-defined"           .= (mDataNodesDefined d)
           , "data-nodes-used"              .= (mDataNodesUsed d)
           , "data-nodes-used-by-phis"      .= (mDataNodesUsedByPhis d)
           , "state-nodes-defined"          .= (mStateNodesDefined d)
           , "state-nodes-used"             .= (mStateNodesUsed d)
           , "root-label-node"              .= (mRootLabelNode d)
           , "non-root-label-nodes"         .= (mNonRootLabelNodes d)
           , "constraints"                  .= (mConstraints d)
           , "apply-def-dom-use-constraint" .= (mADDUC d)
           , "has-control-nodes"            .= (mHasControlNodes d)
           , "code-size"                    .= (mCodeSize d)
           , "latency"                      .= (mLatency d)
           , "assembly-id-maps"             .= (mAssIDMaps d)
           ]

instance FromJSON MachineData where
  parseJSON (Object v) =
    MachineData
      <$> v .: "target-machine-id"
      <*> v .: "registers"
  parseJSON _ = mzero

instance ToJSON MachineData where
  toJSON d =
    object [ "target-machine-id" .= (machID d)
           , "registers" .= (machRegisters d)
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
  toJSON pid = toJSON (fromPatternID pid)

instance FromJSON MatchID where
  parseJSON (Number sn) = return $ toMatchID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON MatchID where
  toJSON mid = toJSON (fromMatchID mid)

instance FromJSON RegisterID where
  parseJSON (Number sn) = return $ toRegisterID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON RegisterID where
  toJSON rid = toJSON (fromRegisterID rid)

instance FromJSON TargetMachineID where
  parseJSON (String s) = return $ toTargetMachineID $ T.unpack s
  parseJSON _ = mzero

instance ToJSON TargetMachineID where
  toJSON tmid = toJSON (fromTargetMachineID tmid)

instance FromJSON Natural where
  parseJSON (Number sn) = return $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)

instance FromJSON ExecFreq where
  parseJSON (Number sn) = return $ toExecFreq $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON ExecFreq where
  toJSON i = toJSON (fromExecFreq i)

instance FromJSON BasicBlockLabel where
  parseJSON (String s) = return $ (BasicBlockLabel $ T.unpack s)
  parseJSON _ = mzero

instance ToJSON BasicBlockLabel where
  toJSON (BasicBlockLabel s) = toJSON s

instance FromJSON RawCPSolutionData where
  parseJSON (Object v) =
    RawCPSolutionData
      <$> v .: "bb-allocated-for-match"
      <*> v .: "is-match-selected"
      <*> v .: "order-of-bbs"
      <*> v .: "has-dnode-reg"
      <*> v .: "reg-selected-for-dnode"
      <*> v .: "has-dnode-imm-value"
      <*> v .: "imm-value-of-dnode"
      <*> v .: "cost"
  parseJSON _ = mzero

instance FromJSON RawPostParams where
  parseJSON (Object v) =
    RawPostParams
      <$> v .: "model-params"
      <*> ((v .: "array-index-to-id-maps") >>= (.: "matches"))
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
fromJson ::
  FromJSON a
  => String
  -> Either String a
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field the parsed entity (when parsing was successful).
fromJson s =
  let result = decode (BS.pack s)
  in if isJust result
     then Right (fromJust result)
     else Left ("failed to parse JSON")

-- | Converts an entity into a JSON string.
toJson :: ToJSON a => a -> String
toJson = unescape . BS.unpack . encode
  where unescape = replace "\\u003c" "<" . replace "\\u003e" ">"
        -- ^ For security reasons, Aeson will escape '<' and '>' when dumping
        -- JSON data to string, which is something we want to undo.
