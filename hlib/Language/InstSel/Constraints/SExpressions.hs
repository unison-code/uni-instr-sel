--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Constraints.SExpressions
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Parsing of and conversion to lispian expressions.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstSel.Constraints.SExpressions
  ( fromLispExpr
  , toLispExpr
  )
where

import Language.InstSel.Constraints.Base
import Language.InstSel.Graphs
  ( MatchID (..)
  , NodeID (..)
  , toMatchID
  , toNodeID
  )
import Language.InstSel.Patterns.IDs
import Language.InstSel.TargetMachine.IDs
import Language.InstSel.Utils
  ( fromNatural )
import Control.Applicative
  ( (<|>) )
import Control.Monad
  ( mzero )
import Data.AttoLisp
import qualified Data.Attoparsec.Number as AP
  ( Number (..) )



------------------------
-- Type class instances
------------------------

instance FromLisp Constraint where
  parseLisp = wrapStruct BoolExprConstraint

instance ToLisp Constraint where
  toLisp (BoolExprConstraint e) = toLisp e

instance FromLisp BoolExpr where
  parseLisp e =
        struct "=="  EqExpr  e
    <|> struct "!="  NeqExpr e
    <|> struct ">"   GTExpr  e
    <|> struct ">="  GEExpr  e
    <|> struct "<"   LTExpr  e
    <|> struct "<="  LEExpr  e
    <|> struct "&&"  AndExpr e
    <|> struct "||"  OrExpr  e
    <|> struct "->"  ImpExpr e
    <|> struct "<->" EqvExpr e
    <|> struct "!"   NotExpr e
    <|> struct "in-set" InSetExpr e
    <|> struct "dnode-is-int-const" DataNodeIsAnIntConstantExpr e
    <|> struct "dnode-is-intermediate" DataNodeIsIntermediateExpr e

instance ToLisp BoolExpr where
  toLisp (EqExpr  lhs rhs) = mkStruct "==" [toLisp lhs, toLisp rhs]
  toLisp (NeqExpr lhs rhs) = mkStruct "!=" [toLisp lhs, toLisp rhs]
  toLisp (GTExpr  lhs rhs) = mkStruct ">"  [toLisp lhs, toLisp rhs]
  toLisp (GEExpr  lhs rhs) = mkStruct ">=" [toLisp lhs, toLisp rhs]
  toLisp (LTExpr  lhs rhs) = mkStruct "<"  [toLisp lhs, toLisp rhs]
  toLisp (LEExpr  lhs rhs) = mkStruct "<=" [toLisp lhs, toLisp rhs]
  toLisp (AndExpr lhs rhs) = mkStruct "&&" [toLisp lhs, toLisp rhs]
  toLisp (OrExpr  lhs rhs) = mkStruct "||" [toLisp lhs, toLisp rhs]
  toLisp (ImpExpr lhs rhs) = mkStruct "->" [toLisp lhs, toLisp rhs]
  toLisp (EqvExpr lhs rhs) = mkStruct "<->" [toLisp lhs, toLisp rhs]
  toLisp (NotExpr lhs) = mkStruct "!" [toLisp lhs]
  toLisp (InSetExpr lhs rhs) = mkStruct "in-set" [toLisp lhs, toLisp rhs]
  toLisp (DataNodeIsAnIntConstantExpr e) =
    mkStruct "dnode-is-int-const" [toLisp e]
  toLisp (DataNodeIsIntermediateExpr e) =
    mkStruct "dnode-is-intermediate" [toLisp e]

instance FromLisp NumExpr where
  parseLisp e =
        struct "+" PlusExpr e
    <|> struct "-" MinusExpr e
    <|> struct "int-to-num" Int2NumExpr e
    <|> struct "bool-to-num" Bool2NumExpr e
    <|> struct "node-to-num" Node2NumExpr e
    <|> struct "match-to-num" Match2NumExpr e
    <|> struct "instr-to-num" Instruction2NumExpr e
    <|> struct "pat-to-num" Pattern2NumExpr e
    <|> struct "lab-to-num" Label2NumExpr e
    <|> struct "reg-to-num" Register2NumExpr e
    <|> struct "dist-pat-to-lab" DistanceBetweenMatchAndLabelExpr e

instance ToLisp NumExpr where
  toLisp (PlusExpr  lhs rhs)         = mkStruct "+" [toLisp lhs, toLisp rhs]
  toLisp (MinusExpr lhs rhs)         = mkStruct "-" [toLisp lhs, toLisp rhs]
  toLisp (Int2NumExpr e)             = mkStruct "int-to-num" [toLisp e]
  toLisp (Bool2NumExpr e)            = mkStruct "bool-to-num" [toLisp e]
  toLisp (Node2NumExpr e)            = mkStruct "node-to-num" [toLisp e]
  toLisp (Match2NumExpr e) = mkStruct "match-to-num" [toLisp e]
  toLisp (Instruction2NumExpr e)     = mkStruct "instr-to-num" [toLisp e]
  toLisp (Pattern2NumExpr e)         = mkStruct "pat-to-num" [toLisp e]
  toLisp (Label2NumExpr e)           = mkStruct "lab-to-num" [toLisp e]
  toLisp (Register2NumExpr e)        = mkStruct "reg-to-num" [toLisp e]
  toLisp (DistanceBetweenMatchAndLabelExpr lhs rhs) =
    mkStruct "dist-pat-to-lab" [toLisp lhs, toLisp rhs]

instance FromLisp IntExpr where
  parseLisp e =
        struct "int" AnIntegerExpr e
    <|> struct "int-const-val-of-dnode" IntConstValueOfDataNodeExpr e

instance ToLisp IntExpr where
  toLisp (AnIntegerExpr i) = mkStruct "int" [toLisp i]
  toLisp (IntConstValueOfDataNodeExpr e) =
    mkStruct "int-const-val-of-dnode" [toLisp e]

instance FromLisp NodeExpr where
  parseLisp e = struct "id" ANodeIDExpr e

instance ToLisp NodeExpr where
  toLisp (ANodeIDExpr nid) = mkStruct "id" [toLisp nid]

instance FromLisp MatchExpr where
  parseLisp (Symbol "this") = return ThisMatchExpr
  parseLisp e =
        struct "id" AMatchIDExpr e
    <|> struct "cov-of-onode" CovererOfOperationNodeExpr e
    <|> struct "def-of-dnode" DefinerOfDataNodeExpr e
    <|> struct "def-of-snode" DefinerOfStateNodeExpr e

instance ToLisp MatchExpr where
  toLisp (AMatchIDExpr piid) = mkStruct "id" [toLisp piid]
  toLisp ThisMatchExpr = Symbol "this"
  toLisp (CovererOfOperationNodeExpr e) = mkStruct "cov-of-onode" [toLisp e]
  toLisp (DefinerOfDataNodeExpr e)  = mkStruct "def-of-dnode" [toLisp e]
  toLisp (DefinerOfStateNodeExpr e) = mkStruct "def-of-snode" [toLisp e]

instance FromLisp InstructionExpr where
  parseLisp e =
        struct "id" AnInstructionIDExpr e
    <|> struct "instr-of-pat" InstructionOfPatternExpr e

instance ToLisp InstructionExpr where
  toLisp (AnInstructionIDExpr iid) = mkStruct "id" [toLisp iid]
  toLisp (InstructionOfPatternExpr e) = mkStruct "instr-of-pat" [toLisp e]

instance FromLisp PatternExpr where
  parseLisp e =
        struct "id" APatternIDExpr e
    <|> struct "pat-of-match" PatternOfMatchExpr e

instance ToLisp PatternExpr where
  toLisp (APatternIDExpr iid) = mkStruct "id" [toLisp iid]
  toLisp (PatternOfMatchExpr e) =
    mkStruct "pat-of-match" [toLisp e]

instance FromLisp LabelExpr where
  parseLisp e =
        struct "lab-alloc-to-match" LabelAllocatedToMatchExpr e
    <|> struct "lab-of-lnode" LabelOfLabelNodeExpr e

instance ToLisp LabelExpr where
  toLisp (LabelAllocatedToMatchExpr e) =
    mkStruct "lab-alloc-to-match" [toLisp e]
  toLisp (LabelOfLabelNodeExpr e) = mkStruct "lab-of-lnode" [toLisp e]

instance FromLisp RegisterExpr where
  parseLisp e =
        struct "id" ARegisterIDExpr e
    <|> struct "reg-alloc-to-dnode" RegisterAllocatedToDataNodeExpr e

instance ToLisp RegisterExpr where
  toLisp (ARegisterIDExpr iid) = mkStruct "id" [toLisp iid]
  toLisp (RegisterAllocatedToDataNodeExpr e) =
    mkStruct "reg-alloc-to-dnode" [toLisp e]

instance FromLisp SetExpr where
  parseLisp e =
        struct "union" UnionSetExpr e
    <|> struct "intersect" IntersectSetExpr e
    <|> struct "diff" DiffSetExpr e
    <|> struct "domset-of-lab" DomSetOfLabelExpr e
    <|> struct "reg-class" RegisterClassExpr e

instance ToLisp SetExpr where
  toLisp (UnionSetExpr lhs rhs) =
    mkStruct "union" [toLisp lhs, toLisp rhs]
  toLisp (IntersectSetExpr lhs rhs) =
    mkStruct "intersect" [toLisp lhs, toLisp rhs]
  toLisp (DiffSetExpr lhs rhs) =
    mkStruct "diff" [toLisp lhs, toLisp rhs]
  toLisp (DomSetOfLabelExpr e) =
    mkStruct "domset-of-lab" [toLisp e]
  toLisp (RegisterClassExpr es) =
    mkStruct "reg-class" (map toLisp es)

instance FromLisp SetElemExpr where
  parseLisp e =
        struct "lab-to-set-elem" Label2SetElemExpr e
    <|> struct "reg-to-set-elem" Register2SetElemExpr e

instance ToLisp SetElemExpr where
  toLisp (Label2SetElemExpr e)    = mkStruct "lab-to-set-elem" [toLisp e]
  toLisp (Register2SetElemExpr e) = mkStruct "reg-to-set-elem" [toLisp e]

instance FromLisp NodeID where
  parseLisp (Number (AP.I n)) = return $ toNodeID n
  parseLisp _ = mzero

instance ToLisp NodeID where
  toLisp (NodeID nid) = Number (AP.I (fromNatural nid))

instance FromLisp InstructionID where
  parseLisp (Number (AP.I n)) = return $ toInstructionID n
  parseLisp _ = mzero

instance ToLisp InstructionID where
  toLisp (InstructionID nid) = Number (AP.I (fromNatural nid))

instance FromLisp MatchID where
  parseLisp (Number (AP.I n)) = return $ toMatchID n
  parseLisp _ = mzero

instance ToLisp MatchID where
  toLisp (MatchID nid) = Number (AP.I (fromNatural nid))

instance FromLisp PatternID where
  parseLisp (Number (AP.I n)) = return $ toPatternID n
  parseLisp _ = mzero

instance ToLisp PatternID where
  toLisp (PatternID nid) = Number (AP.I (fromNatural nid))

instance FromLisp RegisterID where
  parseLisp (Number (AP.I n)) = return $ toRegisterID n
  parseLisp _ = mzero

instance ToLisp RegisterID where
  toLisp (RegisterID nid) = Number (AP.I (fromNatural nid))



-------------
-- Functions
-------------

-- | Parses a lispian expression into a 'Constraint'.
fromLispExpr ::
     String
  -> Either String Constraint
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field contains the constraint (if parsing succeeded).
fromLispExpr = fromLispExprStr

-- | Converts a 'Constraint' into a lispian expression.
toLispExpr :: Constraint -> String
toLispExpr = toLispExprStr
