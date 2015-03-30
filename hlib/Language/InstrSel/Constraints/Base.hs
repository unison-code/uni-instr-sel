--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Constraints.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing instruction selection constraints.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.Constraints.Base
  ( BoolExpr (..)
  , Constraint (..)
  , InstructionExpr (..)
  , IntExpr (..)
  , LabelExpr (..)
  , MatchExpr (..)
  , NodeExpr (..)
  , NumExpr (..)
  , LocationExpr (..)
  , SetElemExpr (..)
  , SetExpr (..)
  , fromLispExpr
  , toLispExpr
  )
where

import Language.InstrSel.ConstraintModels.IDs
import Language.InstrSel.Graphs.IDs
import Language.InstrSel.TargetMachines.IDs
import Language.InstrSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstrSel.Utils.Lisp
  hiding
  ( Lisp (..) )
import qualified Language.InstrSel.Utils.Lisp as Lisp
  ( Lisp (..) )
import Language.InstrSel.Utils.JSON
  hiding
  ( Value (..) )
import qualified Language.InstrSel.Utils.JSON as JSON
  ( Value (..) )



--------------
-- Data types
--------------

data Constraint
    -- | A constraint represented as a Boolean expression.
  = BoolExprConstraint { boolExpr :: BoolExpr }
  deriving (Show)

-- | Boolean expressions. For binary operations the first argument is always the
-- left-hand side and the second argument is always the right-hand side.
data BoolExpr
    -- | Equals.
  = EqExpr  NumExpr  NumExpr
    -- | Not equals.
  | NEqExpr NumExpr  NumExpr
    -- | Greater than.
  | GTExpr  NumExpr  NumExpr
    -- | Greater than or equals.
  | GEExpr  NumExpr  NumExpr
    -- | Less than.
  | LTExpr  NumExpr  NumExpr
    -- | Less than or equals.
  | LEExpr  NumExpr  NumExpr
  | AndExpr BoolExpr BoolExpr
  | OrExpr  BoolExpr BoolExpr
    -- | Implication.
  | ImpExpr BoolExpr BoolExpr
    -- | Equivalence.
  | EqvExpr BoolExpr BoolExpr
  | NotExpr BoolExpr
  | InSetExpr SetElemExpr SetExpr
  deriving (Show)

-- | Numerical expressions. For binary operations the first argument is always
-- the left-hand side and the second argument is always the right-hand side.

data NumExpr
  = PlusExpr  NumExpr NumExpr
  | MinusExpr NumExpr NumExpr
    -- | Converts an integer value to a numerical expression.
  | Int2NumExpr IntExpr
    -- | Converts a Boolean value to a numerical expression.
  | Bool2NumExpr BoolExpr
    -- | Converts a node to a numerical expression.
  | Node2NumExpr NodeExpr
    -- | Converts a match to a numerical expression.
  | Match2NumExpr MatchExpr
    -- | Converts an instruction to a numerical expression.
  | Instruction2NumExpr InstructionExpr
    -- | Converts a pattern to a numerical expression.
  | Label2NumExpr LabelExpr
    -- | Converts a location to a numerical expression.
  | Location2NumExpr LocationExpr
    -- | Represents the distance between a match and a label. The distance
    -- starts from the end of the instruction represented by the pattern and
    -- stops at the beginning of the first instruction within the basic block
    -- represented by the label. The distance is negative if the label appears
    -- before the pattern.
  | DistanceBetweenMatchAndLabelExpr MatchExpr LabelExpr
  deriving (Show)

-- | Integer value expressions.
data IntExpr
    -- | Introduces an integer value.
  = AnIntegerExpr Integer
    -- | Retrieves the value of a data node which represents an integer
    -- constant. This expression *must* be used together with
    -- 'DataNodeIsIntConstantConstraint'!
  | IntConstValueOfDataNodeExpr NodeExpr
  deriving (Show)

-- | Node expressions.
data NodeExpr
    -- | Introduces the ID of a node.
  = ANodeIDExpr NodeID
    -- | Introduces an array index of a node.
  | ANodeArrayIndexExpr ArrayIndex
  deriving (Show)

-- | Match expressions.
data MatchExpr
    -- | Introduces the ID of a match.
  = AMatchIDExpr MatchID
    -- | Introduces the array index of a match.
  | AMatchArrayIndexExpr ArrayIndex
    -- | Retrieves the match in which this expression appears.
  | ThisMatchExpr
  deriving (Show)

-- | Instruction expressions.
data InstructionExpr
    -- | Introduces the ID of an instruction.
  = AnInstructionIDExpr InstructionID
    -- | Introduces the array index of an instruction.
  | AnInstructionArrayIndexExpr ArrayIndex
    -- | Retrieves the instruction from which a match has been derived.
  | InstructionOfMatchExpr MatchExpr
  deriving (Show)

-- | Label expressions.
data LabelExpr
    -- | Retrieves the of the label to which a match has been moved.
  = LabelToWhereMatchIsMovedExpr MatchExpr
    -- | Retrieves the label associated with a label node.
  | LabelOfLabelNodeExpr NodeExpr
  deriving (Show)

-- | Location expressions.
data LocationExpr
    -- | Introduces the ID of a location.
  = ALocationIDExpr LocationID
    -- | Introduces the array index of a location.
  | ALocationArrayIndexExpr ArrayIndex
    -- | Retrieves the of the location of a data node.
  | LocationOfDataNodeExpr NodeExpr
    -- | Denotes the null location.
  | TheNullLocationExpr
  deriving (Show)

-- | Set construction expressions.
data SetExpr =
    UnionSetExpr SetExpr SetExpr
  | IntersectSetExpr SetExpr SetExpr
    -- | @A@ `diff` @B@. The first field represents @A@ and the second field
    -- @B@.
  | DiffSetExpr SetExpr SetExpr
    -- | Retrieves the dominator set of a label.
  | DomSetOfLabelExpr LabelExpr
    -- | Retrieves a location class (which is expressed as a set of individual
    -- locations belonging to that class).
  | LocationClassExpr [LocationExpr]
  deriving (Show)

-- | Set element expressions.
data SetElemExpr
    -- | Converts a label to a set element expression.
  = Label2SetElemExpr LabelExpr
    -- | Converts a location to a set element expression.
  | Location2SetElemExpr LocationExpr
  deriving (Show)



-------------------------------------
-- JSON-realted type class instances
-------------------------------------

instance FromJSON Constraint where
  parseJSON (JSON.String vs) =
    do let s = unpack vs
           res = fromLispExpr s
       when (isLeft res) $ fail $ fromLeft res
       return (fromRight res)
  parseJSON _ = mzero

instance ToJSON Constraint where
  toJSON = toJSON . toLispExpr



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp Constraint where
  parseLisp = wrapStruct BoolExprConstraint

instance ToLisp Constraint where
  toLisp (BoolExprConstraint e) = toLisp e

instance FromLisp BoolExpr where
  parseLisp e =
        struct "=="  EqExpr  e
    <|> struct "!="  NEqExpr e
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

instance ToLisp BoolExpr where
  toLisp (EqExpr  lhs rhs) = mkStruct "==" [toLisp lhs, toLisp rhs]
  toLisp (NEqExpr lhs rhs) = mkStruct "!=" [toLisp lhs, toLisp rhs]
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

instance FromLisp NumExpr where
  parseLisp e =
        struct "+" PlusExpr e
    <|> struct "-" MinusExpr e
    <|> struct "int-to-num" Int2NumExpr e
    <|> struct "bool-to-num" Bool2NumExpr e
    <|> struct "node-to-num" Node2NumExpr e
    <|> struct "match-to-num" Match2NumExpr e
    <|> struct "instr-to-num" Instruction2NumExpr e
    <|> struct "lab-to-num" Label2NumExpr e
    <|> struct "loc-to-num" Location2NumExpr e
    <|> struct "dist-match-to-lab" DistanceBetweenMatchAndLabelExpr e

instance ToLisp NumExpr where
  toLisp (PlusExpr  lhs rhs)     = mkStruct "+" [toLisp lhs, toLisp rhs]
  toLisp (MinusExpr lhs rhs)     = mkStruct "-" [toLisp lhs, toLisp rhs]
  toLisp (Int2NumExpr e)         = mkStruct "int-to-num" [toLisp e]
  toLisp (Bool2NumExpr e)        = mkStruct "bool-to-num" [toLisp e]
  toLisp (Node2NumExpr e)        = mkStruct "node-to-num" [toLisp e]
  toLisp (Match2NumExpr e)       = mkStruct "match-to-num" [toLisp e]
  toLisp (Instruction2NumExpr e) = mkStruct "instr-to-num" [toLisp e]
  toLisp (Label2NumExpr e)       = mkStruct "lab-to-num" [toLisp e]
  toLisp (Location2NumExpr e)    = mkStruct "loc-to-num" [toLisp e]
  toLisp (DistanceBetweenMatchAndLabelExpr lhs rhs) =
    mkStruct "dist-match-to-lab" [toLisp lhs, toLisp rhs]

instance FromLisp IntExpr where
  parseLisp e =
        struct "int" AnIntegerExpr e
    <|> struct "int-const-val-of-dnode" IntConstValueOfDataNodeExpr e

instance ToLisp IntExpr where
  toLisp (AnIntegerExpr i) = mkStruct "int" [toLisp i]
  toLisp (IntConstValueOfDataNodeExpr e) =
    mkStruct "int-const-val-of-dnode" [toLisp e]

instance FromLisp NodeExpr where
  parseLisp e =
        struct "id" ANodeIDExpr e
    <|> struct "ai" ANodeArrayIndexExpr e

instance ToLisp NodeExpr where
  toLisp (ANodeIDExpr nid) = mkStruct "id" [toLisp nid]
  toLisp (ANodeArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]

instance FromLisp MatchExpr where
  parseLisp (Lisp.Symbol "this") = return ThisMatchExpr
  parseLisp e =
        struct "id" AMatchIDExpr e
    <|> struct "ai" AMatchArrayIndexExpr e

instance ToLisp MatchExpr where
  toLisp (AMatchIDExpr piid) = mkStruct "id" [toLisp piid]
  toLisp (AMatchArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp ThisMatchExpr = Lisp.Symbol "this"

instance FromLisp InstructionExpr where
  parseLisp e =
        struct "id" AnInstructionIDExpr e
    <|> struct "ai" AnInstructionArrayIndexExpr e
    <|> struct "instr-of-match" InstructionOfMatchExpr e

instance ToLisp InstructionExpr where
  toLisp (AnInstructionIDExpr iid) = mkStruct "id" [toLisp iid]
  toLisp (AnInstructionArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp (InstructionOfMatchExpr e) = mkStruct "instr-of-match" [toLisp e]

instance FromLisp LabelExpr where
  parseLisp e =
        struct "lab-of-match" LabelToWhereMatchIsMovedExpr e
    <|> struct "lab-of-lnode" LabelOfLabelNodeExpr e

instance ToLisp LabelExpr where
  toLisp (LabelToWhereMatchIsMovedExpr e) =
    mkStruct "lab-of-match" [toLisp e]
  toLisp (LabelOfLabelNodeExpr e) = mkStruct "lab-of-lnode" [toLisp e]

instance FromLisp LocationExpr where
  parseLisp (Lisp.Symbol "null") = return TheNullLocationExpr
  parseLisp e =
        struct "id" ALocationIDExpr e
    <|> struct "ai" ALocationArrayIndexExpr e
    <|> struct "loc-of-dnode" LocationOfDataNodeExpr e

instance ToLisp LocationExpr where
  toLisp TheNullLocationExpr = Lisp.Symbol "null"
  toLisp (ALocationIDExpr rid) = mkStruct "id" [toLisp rid]
  toLisp (ALocationArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp (LocationOfDataNodeExpr e) =
    mkStruct "loc-of-dnode" [toLisp e]

instance FromLisp SetExpr where
  parseLisp e =
        struct "union" UnionSetExpr e
    <|> struct "intersect" IntersectSetExpr e
    <|> struct "diff" DiffSetExpr e
    <|> struct "domset-of-lab" DomSetOfLabelExpr e
    <|> struct "loc-class" LocationClassExpr e

instance ToLisp SetExpr where
  toLisp (UnionSetExpr lhs rhs) =
    mkStruct "union" [toLisp lhs, toLisp rhs]
  toLisp (IntersectSetExpr lhs rhs) =
    mkStruct "intersect" [toLisp lhs, toLisp rhs]
  toLisp (DiffSetExpr lhs rhs) =
    mkStruct "diff" [toLisp lhs, toLisp rhs]
  toLisp (DomSetOfLabelExpr e) =
    mkStruct "domset-of-lab" [toLisp e]
  toLisp (LocationClassExpr es) =
    mkStruct "loc-class" [Lisp.List (map toLisp es)]

instance FromLisp SetElemExpr where
  parseLisp e =
        struct "lab-to-set-elem" Label2SetElemExpr e
    <|> struct "loc-to-set-elem" Location2SetElemExpr e

instance ToLisp SetElemExpr where
  toLisp (Label2SetElemExpr e)    = mkStruct "lab-to-set-elem" [toLisp e]
  toLisp (Location2SetElemExpr e) = mkStruct "loc-to-set-elem" [toLisp e]



-------------
-- Functions
-------------

-- | Parses a lispian expression into a 'Constraint'.
fromLispExpr
  :: String
  -> Either String Constraint
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field contains the constraint (if parsing succeeded).
fromLispExpr = fromLispExprStr

-- | Converts a 'Constraint' into a lispian expression.
toLispExpr :: Constraint -> String
toLispExpr = toLispExprStr
