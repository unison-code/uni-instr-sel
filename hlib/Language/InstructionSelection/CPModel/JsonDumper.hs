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
import Language.InstructionSelection.Graphs ( NodeID
                                            , fromNodeID
                                            )
import Language.InstructionSelection.Patterns ( PatternInstanceID
                                              , fromPatternInstanceID
                                              )
import Language.InstructionSelection.TargetMachine ( RegisterID
                                                   , fromRegisterID
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
    object ([ "instance-id"          .= (patInstanceID d)
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
  toJSON (DataNodeIsIntConstantConstraint nid) =
    toJSON $ "(dnode-is-int-const " ++ show (fromNodeID nid) ++ ")"

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
numExpr2Str (Int2NumExpr e) =
  "(int-to-num " ++ intExpr2Str e ++ ")"
numExpr2Str (Bool2NumExpr e) =
  "(bool-to-num " ++ boolExpr2Str e ++ ")"
numExpr2Str (NodeID2NumExpr e) =
  "(node-id-to-num " ++ nodeIDExpr2Str e ++ ")"
numExpr2Str (PatternInstanceID2NumExpr e) =
  "(insta-id-to-num " ++ instanceIDExpr2Str e ++ ")"
numExpr2Str (InstructionID2NumExpr e) =
  "(instr-id-to-num " ++ instructionIDExpr2Str e ++ ")"
numExpr2Str (PatternID2NumExpr e) =
  "(pat-id-to-num " ++ patternIDExpr2Str e ++ ")"
numExpr2Str (LabelID2NumExpr e) =
  "(lab-id-to-num " ++ labelIDExpr2Str e ++ ")"
numExpr2Str (RegisterID2NumExpr e) =
  "(reg-id-to-num " ++ registerIDExpr2Str e ++ ")"
numExpr2Str (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  "(dist-pat-to-lab " ++ instanceIDExpr2Str pat_e ++
  " " ++ labelIDExpr2Str lab_e ++ ")"

intExpr2Str :: IntExpr -> String
intExpr2Str (AnIntegerExpr i) = show i
intExpr2Str (IntConstValueOfDataNodeExpr e) =
  "(int-const-val-of-dnode " ++ nodeIDExpr2Str e ++ ")"

nodeIDExpr2Str :: NodeIDExpr -> String
nodeIDExpr2Str (ANodeIDExpr nid) = show nid

instanceIDExpr2Str :: PatternInstanceIDExpr -> String
instanceIDExpr2Str (APatternInstanceIDExpr pid) = show pid
instanceIDExpr2Str (CovererOfActionNodeExpr e) =
  "(cov-of-anode " ++ nodeIDExpr2Str e ++ ")"
instanceIDExpr2Str (DefinerOfDataNodeExpr e) =
  "(def-of-dnode " ++ nodeIDExpr2Str e ++ ")"
instanceIDExpr2Str (DefinerOfStateNodeExpr e) =
  "(def-of-snode " ++ nodeIDExpr2Str e ++ ")"
instanceIDExpr2Str ThisPatternInstanceIDExpr = "this"

instructionIDExpr2Str :: InstructionIDExpr -> String
instructionIDExpr2Str (AnInstructionIDExpr iid) = show iid
instructionIDExpr2Str (InstructionIDOfPatternExpr e) =
  "(instr-of-pat " ++ patternIDExpr2Str e ++ ")"

patternIDExpr2Str :: PatternIDExpr -> String
patternIDExpr2Str (APatternIDExpr nid) = show nid
patternIDExpr2Str (PatternIDOfInstanceExpr e) =
  "(pat-of-insta " ++ instanceIDExpr2Str e ++ ")"

labelIDExpr2Str :: LabelIDExpr -> String
labelIDExpr2Str (LabelIDAllocatedToInstanceExpr e) =
  "(lab-alloc-to-insta " ++ instanceIDExpr2Str e ++ ")"
labelIDExpr2Str (LabelIDOfLabelNodeExpr e) =
  "(lab-id-of-node " ++ nodeIDExpr2Str e ++ ")"

registerIDExpr2Str :: RegisterIDExpr -> String
registerIDExpr2Str (ARegisterIDExpr rid) = show rid
registerIDExpr2Str (RegisterIDAllocatedToDataNodeExpr e) =
  "(reg-alloc-to-dnode " ++ nodeIDExpr2Str e ++ ")"

setElemExpr2Str :: SetElemExpr -> String
setElemExpr2Str (LabelID2SetElemExpr e) =
  "(lab-id-to-set-elem " ++ labelIDExpr2Str e ++ ")"
setElemExpr2Str (RegisterID2SetElemExpr e) =
  "(reg-id-to-set-elem " ++ registerIDExpr2Str e ++ ")"

setExpr2Str :: SetExpr -> String
setExpr2Str (UnionSetExpr lhs rhs) =
  "(union " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (IntersectSetExpr lhs rhs) =
  "(intersect " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DiffSetExpr lhs rhs) =
  "(diff " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DomSetOfLabelIDExpr e) =
  "(domset-of " ++ labelIDExpr2Str e ++ ")"
setExpr2Str (RegisterClassExpr es) =
  "(reg-class (" ++ intercalate " " (map registerIDExpr2Str es) ++ "))"
