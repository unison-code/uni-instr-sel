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

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs ( NodeId (..)
                                            , Matchset (..)
                                            )
import Language.InstructionSelection.Patterns (InstanceId (..))
import Language.InstructionSelection.Utils ( Natural (..)
                                           , fromNatural
                                           )
import Data.Aeson
import Data.ByteString.Lazy.Char8 ( pack
                                  , unpack
                                  )



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
           , "constraints"  .= (funcConstraints d)
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
           , "code-size"            .= (patCodeSize d)
           , "latency"              .= (patLatency d)
           , "constraints"          .= (patConstraints d)
           ]

instance ToJSON MachineData where
  toJSON d =
    object [ -- TODO: implement once MachineData has been implemented
           ]

instance ToJSON Constraint where
  toJSON (Constraint e) = toJSON $ boolExpr2Str e

instance ToJSON NodeId where
  toJSON (NodeId i) = toJSON i

instance ToJSON InstanceId where
  toJSON (InstanceId i) = toJSON i

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)



-------------
-- Functions
-------------

boolExpr2Str :: BoolExpr -> String
boolExpr2Str (EqExpr  lhs rhs) =
  "(== " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (NeqExpr lhs rhs) =
  "(!= " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (GTExpr  lhs rhs) =
  "(> " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (GEExpr  lhs rhs) =
  "(>= " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (LTExpr  lhs rhs) =
  "(< " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (LEExpr  lhs rhs) =
  "(<= " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
boolExpr2Str (AndExpr lhs rhs) =
  "(&& " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (OrExpr  lhs rhs) =
  "(|| " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (ImpExpr lhs rhs) =
  "(=> " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (EqvExpr lhs rhs) =
  "(<=> " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (NotExpr e) = "(! " ++ boolExpr2Str e ++ ")"

numExpr2Str :: NumExpr -> String
numExpr2Str (PlusExpr  lhs rhs) =
  "(+ " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (MinusExpr lhs rhs) =
  "(- " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (AnIntegerExpr i) = show i
numExpr2Str (NodeId2NumExpr e) =
  "(node-id-to-num " ++ nodeIdExpr2Str e ++ ")"
numExpr2Str (InstanceId2NumExpr e) =
  "(insta-id-to-num " ++ instanceIdExpr2Str e ++ ")"
numExpr2Str (InstructionId2NumExpr e) =
  "(instr-id-to-num " ++ instructionIdExpr2Str e ++ ")"
numExpr2Str (PatternId2NumExpr e) =
  "(pat-id-to-num " ++ patternIdExpr2Str e ++ ")"
numExpr2Str (LabelId2NumExpr e) =
  "(lab-id-to-num " ++ labelIdExpr2Str e ++ ")"
numExpr2Str (RegisterId2NumExpr e) =
  "(reg-id-to-num " ++ registerIdExpr2Str e ++ ")"

nodeIdExpr2Str :: NodeIdExpr -> String
nodeIdExpr2Str (ANodeIdExpr i) = show i

instanceIdExpr2Str :: InstanceIdExpr -> String
instanceIdExpr2Str (AnInstanceIdExpr i) = show i
instanceIdExpr2Str (CovererOfActionNodeExpr e) =
  "(cov-of-anode " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str (DefinerOfEntityNodeExpr e) =
  "(def-of-enode " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str ThisInstanceIdExpr = "this"

instructionIdExpr2Str :: InstructionIdExpr -> String
instructionIdExpr2Str (AnInstructionIdExpr i) = show i
instructionIdExpr2Str (InstructionIdOfPatternExpr e) =
  "(instr-of-pat " ++ patternIdExpr2Str e ++ ")"

patternIdExpr2Str :: PatternIdExpr -> String
patternIdExpr2Str (APatternIdExpr i) = show i
patternIdExpr2Str (PatternIdOfInstanceExpr e) =
  "(pat-of-insta " ++ instanceIdExpr2Str e ++ ")"

labelIdExpr2Str :: LabelIdExpr -> String
labelIdExpr2Str (LabelAllocatedToInstanceExpr e) =
  "(lab-alloc-to-insta " ++ instanceIdExpr2Str e ++ ")"
labelIdExpr2Str (LabelIdOfLabelNodeExpr e) =
  "(lab-id-of-node " ++ nodeIdExpr2Str e ++ ")"

registerIdExpr2Str :: RegisterIdExpr -> String
registerIdExpr2Str (ARegisterIdExpr i) = show i
registerIdExpr2Str (RegisterAllocatedToDataNodeExpr e) =
  "(reg-alloc-to-dnode " ++ nodeIdExpr2Str e ++ ")"
