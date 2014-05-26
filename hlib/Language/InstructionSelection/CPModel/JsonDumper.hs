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
  toJSON (BoolExprConstraint e) = toJSON $ boolExpr2Str e

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
boolExpr2Str (DataNodeIsAnIntConstantExpr e) =
  "(dnode-is-int-const " ++ nodeExpr2Str e ++ ")"

numExpr2Str :: NumExpr -> String
numExpr2Str (PlusExpr  lhs rhs) =
  "(+ " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (MinusExpr lhs rhs) =
  "(- " ++ numExpr2Str lhs ++ " " ++ numExpr2Str rhs ++ ")"
numExpr2Str (Int2NumExpr e) =
  "(int-to-num " ++ intExpr2Str e ++ ")"
numExpr2Str (Bool2NumExpr e) =
  "(bool-to-num " ++ boolExpr2Str e ++ ")"
numExpr2Str (Node2NumExpr e) =
  "(node-to-num " ++ nodeExpr2Str e ++ ")"
numExpr2Str (PatternInstance2NumExpr e) =
  "(pat-inst-to-num " ++ instanceExpr2Str e ++ ")"
numExpr2Str (Instruction2NumExpr e) =
  "(instr-to-num " ++ instructionExpr2Str e ++ ")"
numExpr2Str (Pattern2NumExpr e) =
  "(pat-to-num " ++ patternExpr2Str e ++ ")"
numExpr2Str (Label2NumExpr e) =
  "(lab-to-num " ++ labelExpr2Str e ++ ")"
numExpr2Str (Register2NumExpr e) =
  "(reg-to-num " ++ registerExpr2Str e ++ ")"
numExpr2Str (DistanceBetweenInstanceAndLabelExpr pat_e lab_e) =
  "(dist-pat-to-lab " ++ instanceExpr2Str pat_e ++
  " " ++ labelExpr2Str lab_e ++ ")"

intExpr2Str :: IntExpr -> String
intExpr2Str (AnIntegerExpr i) = show i
intExpr2Str (IntConstValueOfDataNodeExpr e) =
  "(int-const-val-of-dnode " ++ nodeExpr2Str e ++ ")"

nodeExpr2Str :: NodeExpr -> String
nodeExpr2Str (ANodeIDExpr nid) = "(id " ++ show nid ++ ")"

instanceExpr2Str :: PatternInstanceExpr -> String
instanceExpr2Str (APatternInstanceIDExpr pid) = "(id " ++ show pid ++ ")"
instanceExpr2Str (CovererOfActionNodeExpr e) =
  "(cov-of-anode " ++ nodeExpr2Str e ++ ")"
instanceExpr2Str (DefinerOfDataNodeExpr e) =
  "(def-of-dnode " ++ nodeExpr2Str e ++ ")"
instanceExpr2Str (DefinerOfStateNodeExpr e) =
  "(def-of-snode " ++ nodeExpr2Str e ++ ")"
instanceExpr2Str ThisPatternInstanceExpr = "this"

instructionExpr2Str :: InstructionExpr -> String
instructionExpr2Str (AnInstructionIDExpr iid) = "(id " ++ show iid ++ ")"
instructionExpr2Str (InstructionOfPatternExpr e) =
  "(instr-of-pat " ++ patternExpr2Str e ++ ")"

patternExpr2Str :: PatternExpr -> String
patternExpr2Str (APatternIDExpr pid) = "(id " ++ show pid ++ ")"
patternExpr2Str (PatternOfPatternInstanceExpr e) =
  "(pat-of-pat-inst " ++ instanceExpr2Str e ++ ")"

labelExpr2Str :: LabelExpr -> String
labelExpr2Str (LabelAllocatedToPatternInstanceExpr e) =
  "(lab-alloc-to-pat-inst " ++ instanceExpr2Str e ++ ")"
labelExpr2Str (LabelOfLabelNodeExpr e) =
  "(lab-of-lnode " ++ nodeExpr2Str e ++ ")"

registerExpr2Str :: RegisterExpr -> String
registerExpr2Str (ARegisterIDExpr rid) = "(id " ++ show rid ++ ")"
registerExpr2Str (RegisterAllocatedToDataNodeExpr e) =
  "(reg-alloc-to-dnode " ++ nodeExpr2Str e ++ ")"

setElemExpr2Str :: SetElemExpr -> String
setElemExpr2Str (Label2SetElemExpr e) =
  "(lab-to-set-elem " ++ labelExpr2Str e ++ ")"
setElemExpr2Str (Register2SetElemExpr e) =
  "(reg-to-set-elem " ++ registerExpr2Str e ++ ")"

setExpr2Str :: SetExpr -> String
setExpr2Str (UnionSetExpr lhs rhs) =
  "(union " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (IntersectSetExpr lhs rhs) =
  "(intersect " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DiffSetExpr lhs rhs) =
  "(diff " ++ setExpr2Str lhs ++ " " ++ setExpr2Str rhs ++ ")"
setExpr2Str (DomSetOfLabelExpr e) =
  "(domset-of " ++ labelExpr2Str e ++ ")"
setExpr2Str (RegisterClassExpr es) =
  "(reg-class (" ++ intercalate " " (map registerExpr2Str es) ++ "))"
