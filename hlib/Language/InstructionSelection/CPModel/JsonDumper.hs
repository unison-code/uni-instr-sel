--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.CPModel.JsonDumper
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Dumps a CP model parameter data structure in a JSON format.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstructionSelection.CPModel.JsonDumper (
  toJson
) where

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs ( NodeId
                                            , fromNodeId
                                            )
import Language.InstructionSelection.Patterns ( InstanceId
                                              , fromInstanceId
                                              )
import Language.InstructionSelection.TargetMachine ( RegisterId
                                                   , fromRegisterId
                                                   )
import Language.InstructionSelection.Utils ( Natural (..)
                                           , fromNatural
                                           )
import Data.Aeson
import Data.ByteString.Lazy.Char8 ( unpack
                                  )
import Data.List (intercalate)



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
    object ([ "instance-id"          .= (patInstanceId d)
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
  toJSON (BoolExprConstraint e) = toJSON $ boolExpr2Str e
  toJSON (IsIntConstantConstraint nid) =
    toJSON $ "(is-int-const " ++ show (fromNodeId nid) ++ ")"

instance ToJSON NodeId where
  toJSON nid = toJSON (fromNodeId nid)

instance ToJSON InstanceId where
  toJSON iid = toJSON (fromInstanceId iid)

instance ToJSON RegisterId where
  toJSON rid = toJSON (fromRegisterId rid)

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
  "(-> " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (EqvExpr lhs rhs) =
  "(<-> " ++ boolExpr2Str lhs ++ " " ++ boolExpr2Str rhs ++ ")"
boolExpr2Str (NotExpr e) = "(! " ++ boolExpr2Str e ++ ")"
boolExpr2Str (InSetExpr e set) =
  "(in-set " ++ setElemExpr2Str e ++ " " ++ setExpr2Str set ++ ")"

numExpr2Str :: NumExpr -> String
numExpr2Str (PlusExpr  lhs rhs) =
  "(+ " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (MinusExpr lhs rhs) =
  "(- " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (Int2NumExpr e) = intExpr2Str e
numExpr2Str (Bool2NumExpr e) =
  "(bool-to-num " ++ boolExpr2Str e ++ ")"
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
numExpr2Str (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  "(dist-pat-to-lab " ++ instanceIdExpr2Str pat_e ++
  " " ++ labelIdExpr2Str lab_e ++ ")"

intExpr2Str :: IntExpr -> String
intExpr2Str (AnIntegerExpr i) = show i
intExpr2Str (IntConstValueOfDataNodeExpr e) =
  "(int-const-val-of-dnode " ++ nodeIdExpr2Str e ++ ")"

nodeIdExpr2Str :: NodeIdExpr -> String
nodeIdExpr2Str (ANodeIdExpr nid) = show nid

instanceIdExpr2Str :: InstanceIdExpr -> String
instanceIdExpr2Str (AnInstanceIdExpr iid) = show iid
instanceIdExpr2Str (CovererOfActionNodeExpr e) =
  "(cov-of-anode " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str (DefinerOfDataNodeExpr e) =
  "(def-of-dnode " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str (DefinerOfStateNodeExpr e) =
  "(def-of-snode " ++ nodeIdExpr2Str e ++ ")"
instanceIdExpr2Str ThisInstanceIdExpr = "this"

instructionIdExpr2Str :: InstructionIdExpr -> String
instructionIdExpr2Str (AnInstructionIdExpr iid) = show iid
instructionIdExpr2Str (InstructionIdOfPatternExpr e) =
  "(instr-of-pat " ++ patternIdExpr2Str e ++ ")"

patternIdExpr2Str :: PatternIdExpr -> String
patternIdExpr2Str (APatternIdExpr nid) = show nid
patternIdExpr2Str (PatternIdOfInstanceExpr e) =
  "(pat-of-insta " ++ instanceIdExpr2Str e ++ ")"

labelIdExpr2Str :: LabelIdExpr -> String
labelIdExpr2Str (LabelIdAllocatedToInstanceExpr e) =
  "(lab-alloc-to-insta " ++ instanceIdExpr2Str e ++ ")"
labelIdExpr2Str (LabelIdOfLabelNodeExpr e) =
  "(lab-id-of-node " ++ nodeIdExpr2Str e ++ ")"

registerIdExpr2Str :: RegisterIdExpr -> String
registerIdExpr2Str (ARegisterIdExpr rid) = show rid
registerIdExpr2Str (RegisterIdAllocatedToDataNodeExpr e) =
  "(reg-alloc-to-dnode " ++ nodeIdExpr2Str e ++ ")"

setElemExpr2Str :: SetElemExpr -> String
setElemExpr2Str (LabelId2SetElemExpr e) =
  "(lab-id-to-set-elem " ++ labelIdExpr2Str e ++ ")"
setElemExpr2Str (RegisterId2SetElemExpr e) =
  "(reg-id-to-set-elem " ++ registerIdExpr2Str e ++ ")"

setExpr2Str :: SetExpr -> String
setExpr2Str (UnionSetExpr lhs rhs) =
  "(union " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (IntersectSetExpr lhs rhs) =
  "(intersect " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DiffSetExpr lhs rhs) =
  "(diff " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DomSetOfLabelIdExpr e) =
  "(domset-of " ++ labelIdExpr2Str e ++ ")"
setExpr2Str (RegisterClassExpr es) =
  "(reg-class (" ++ intercalate " " (map registerIdExpr2Str es) ++ "))"
