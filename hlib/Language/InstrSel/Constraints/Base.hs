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
  , RegisterExpr (..)
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
    -- | An expression indicating that a particular data node represents a
    -- constant integer value.
  | DataNodeIsAnIntConstantExpr NodeExpr
    -- | An expression indicating that a particular data node represents an
    -- intermediate data value, meaning that its value cannot be reused by
    -- another match.
  | DataNodeIsIntermediateExpr NodeExpr
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
    -- | Converts a register to a numerical expression.
  | Register2NumExpr RegisterExpr
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
    -- | Retrieves the match which covers a certain operation node.
  | CovererOfOperationNodeExpr NodeExpr
    -- | Retrieves the match which defines a certain data node.
  | DefinerOfDataNodeExpr NodeExpr
    -- | Retrieves the match which defines a certain state node.
  | DefinerOfStateNodeExpr NodeExpr
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
    -- | Retrieves the of the label to which a match has been allocated.
  = LabelAllocatedToMatchExpr MatchExpr
    -- | Retrieves the label associated with a label node.
  | LabelOfLabelNodeExpr NodeExpr
  deriving (Show)

-- | Register expressions.
data RegisterExpr
    -- | Introduces the ID of a register.
  = ARegisterIDExpr RegisterID
    -- | Introduces the array index of a register.
  | ARegisterArrayIndexExpr ArrayIndex
    -- | Retrieves the of the register to which a data node has been allocated.
  | RegisterAllocatedToDataNodeExpr NodeExpr
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
    -- | Retrieves a register class (which is expressed as a set of individual
    -- registers belonging to that class).
  | RegisterClassExpr [RegisterExpr]
  deriving (Show)

-- | Set element expressions.
data SetElemExpr
    -- | Converts a label to a set element expression.
  = Label2SetElemExpr LabelExpr
    -- | Converts a register to a set element expression.
  | Register2SetElemExpr RegisterExpr
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
    <|> struct "dnode-is-int-const" DataNodeIsAnIntConstantExpr e
    <|> struct "dnode-is-intermediate" DataNodeIsIntermediateExpr e

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
    <|> struct "lab-to-num" Label2NumExpr e
    <|> struct "reg-to-num" Register2NumExpr e
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
  toLisp (Register2NumExpr e)    = mkStruct "reg-to-num" [toLisp e]
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
    <|> struct "cov-of-onode" CovererOfOperationNodeExpr e
    <|> struct "def-of-dnode" DefinerOfDataNodeExpr e
    <|> struct "def-of-snode" DefinerOfStateNodeExpr e

instance ToLisp MatchExpr where
  toLisp (AMatchIDExpr piid) = mkStruct "id" [toLisp piid]
  toLisp (AMatchArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp ThisMatchExpr = Lisp.Symbol "this"
  toLisp (CovererOfOperationNodeExpr e) = mkStruct "cov-of-onode" [toLisp e]
  toLisp (DefinerOfDataNodeExpr e)  = mkStruct "def-of-dnode" [toLisp e]
  toLisp (DefinerOfStateNodeExpr e) = mkStruct "def-of-snode" [toLisp e]

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
        struct "lab-alloc-to-match" LabelAllocatedToMatchExpr e
    <|> struct "lab-of-lnode" LabelOfLabelNodeExpr e

instance ToLisp LabelExpr where
  toLisp (LabelAllocatedToMatchExpr e) =
    mkStruct "lab-alloc-to-match" [toLisp e]
  toLisp (LabelOfLabelNodeExpr e) = mkStruct "lab-of-lnode" [toLisp e]

instance FromLisp RegisterExpr where
  parseLisp e =
        struct "id" ARegisterIDExpr e
    <|> struct "ai" ARegisterArrayIndexExpr e
    <|> struct "reg-alloc-to-dnode" RegisterAllocatedToDataNodeExpr e

instance ToLisp RegisterExpr where
  toLisp (ARegisterIDExpr rid) = mkStruct "id" [toLisp rid]
  toLisp (ARegisterArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
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
    mkStruct "reg-class" [Lisp.List (map toLisp es)]

instance FromLisp SetElemExpr where
  parseLisp e =
        struct "lab-to-set-elem" Label2SetElemExpr e
    <|> struct "reg-to-set-elem" Register2SetElemExpr e

instance ToLisp SetElemExpr where
  toLisp (Label2SetElemExpr e)    = mkStruct "lab-to-set-elem" [toLisp e]
  toLisp (Register2SetElemExpr e) = mkStruct "reg-to-set-elem" [toLisp e]



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
