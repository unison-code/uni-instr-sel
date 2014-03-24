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
           , "cost"                 .= (patCost d)
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
numExpr2Str (PluxExpr  lhs rhs) =
  "(+ " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (MinuxExpr lhs rhs) =
  "(- " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (IntExpr i) = show i
numExpr2Str (NodeId2NumExpr e) = nodeIdExpr2Str e
numExpr2Str (InstanceId2NumExpr e) = instanceIdExpr2Str e
numExpr2Str (InstructionId2NumExpr e) = instructionIdExpr2Str e
numExpr2Str (PatternId2NumExpr e) = patternIdExpr2Str e
numExpr2Str (LabelId2NumExpr e) = labelIdExpr2Str e
numExpr2Str (RegisterId2NumExpr e) = registerIdExpr2Str e

nodeIdExpr2Str :: NodeIdExpr -> String
nodeIdExpr2Str (NodeIdExpr i) = show i

instanceIdExpr2Str :: InstanceIdExpr -> String
instanceIdExpr2Str (InstanceIdExpr i) = show i
instanceIdExpr2Str (CovererOfActionExpr e) =
  "(coverer-of-action-node " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str (DefinerOfEntityExpr e) =
  "(definer-of-entity-node " ++ nodeIdExpr2Str e ++ ")"

instructionIdExpr2Str :: InstructionIdExpr -> String
instructionIdExpr2Str (InstructionIdExpr i) = show i
instructionIdExpr2Str (InstructionIdOfPatternExpr e) =
  "(instruction-of-pattern " ++ patternIdExpr2Str e ++ ")"

patternIdExpr2Str :: PatternIdExpr -> String
patternIdExpr2Str (PatternIdExpr i) = show i
patternIdExpr2Str (PatternIdOfInstanceExpr e) =
  "(pattern-of-instance " ++ instanceIdExpr2Str e ++ ")"

labelIdExpr2Str :: LabelIdExpr -> String
labelIdExpr2Str (LabelAllocatedToInstanceExpr e) =
  "(label-allocated-to-instance " ++ instanceIdExpr2Str e ++ ")"

registerIdExpr2Str :: RegisterIdExpr -> String
registerIdExpr2Str (RegisterIdExpr i) = show i
registerIdExpr2Str (RegisterAllocatedToData e) =
  "(register-allocated-to-data-node " ++ nodeIdExpr2Str e ++ ")"
