{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.Constraints.Base
  ( BoolExpr (..)
  , Constraint (..)
  , InstructionExpr (..)
  , IntExpr (..)
  , BlockExpr (..)
  , MatchExpr (..)
  , NodeExpr (..)
  , OperandExpr (..)
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
    -- | Denotes a fall-through constraint between a given match and a given
    -- block. In other words, the block in which the match is placed must be
    -- appear immediately before the other block of the generated code.
  | FallThroughFromMatchToBlockExpr MatchExpr BlockExpr
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
    -- | Converts a block to a numerical expression.
  | Block2NumExpr BlockExpr
    -- | Converts a location to a numerical expression.
  | Location2NumExpr LocationExpr
  deriving (Show)

-- | Integer value expressions.
data IntExpr
    -- | Introduces an integer value.
  = AnIntegerExpr Integer
  deriving (Show)

-- | Node expressions.
data NodeExpr
    -- | Introduces the ID of a node.
  = ANodeIDExpr NodeID
    -- | Introduces an array index of a node.
  | ANodeArrayIndexExpr ArrayIndex
    -- | Retrieves the node selected for an operand.
  | NodeSelectedForOperandExpr OperandExpr
  deriving (Show)

-- | Operand expressions.
data OperandExpr
    -- | Introduces the ID of an operand.
  = AnOperandIDExpr OperandID
    -- | Introduces an array index of an operand.
  | AnOperandArrayIndexExpr ArrayIndex
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

-- | Block expressions.
data BlockExpr
    -- | Retrieves the block in which a match has been placed.
  = BlockWhereinMatchIsPlacedExpr MatchExpr
    -- | Retrieves the block in which the data of a value node has been defined.
  | BlockWhereinDataIsDefinedExpr NodeExpr
    -- | Retrieves the block associated with a block node.
  | BlockOfBlockNodeExpr NodeExpr
  deriving (Show)

-- | Location expressions.
data LocationExpr
    -- | Introduces the ID of a location.
  = ALocationIDExpr LocationID
    -- | Introduces the array index of a location.
  | ALocationArrayIndexExpr ArrayIndex
    -- | Retrieves the of the location of a value node.
  | LocationOfValueNodeExpr NodeExpr
    -- | Denotes the null location.
  | TheNullLocationExpr
  deriving (Show)

-- | Set construction expressions.
data SetExpr =
    UnionSetExpr SetExpr SetExpr
  | IntersectSetExpr SetExpr SetExpr
    -- | @A diff B@. The first field represents @A@ and the second field
    -- @B@.
  | DiffSetExpr SetExpr SetExpr
    -- | Converts a list of locations into a set.
  | LocationClassExpr [LocationExpr]
  deriving (Show)

-- | Set element expressions.
data SetElemExpr
    -- | Converts a block to a set element expression.
  = Block2SetElemExpr BlockExpr
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
    <|> struct "fall-through" FallThroughFromMatchToBlockExpr e

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
  toLisp (FallThroughFromMatchToBlockExpr lhs rhs) =
    mkStruct "fall-through" [toLisp lhs, toLisp rhs]

instance FromLisp NumExpr where
  parseLisp e =
        struct "+" PlusExpr e
    <|> struct "-" MinusExpr e
    <|> struct "int-to-num" Int2NumExpr e
    <|> struct "bool-to-num" Bool2NumExpr e
    <|> struct "node-to-num" Node2NumExpr e
    <|> struct "match-to-num" Match2NumExpr e
    <|> struct "instr-to-num" Instruction2NumExpr e
    <|> struct "block-to-num" Block2NumExpr e
    <|> struct "loc-to-num" Location2NumExpr e

instance ToLisp NumExpr where
  toLisp (PlusExpr  lhs rhs)     = mkStruct "+" [toLisp lhs, toLisp rhs]
  toLisp (MinusExpr lhs rhs)     = mkStruct "-" [toLisp lhs, toLisp rhs]
  toLisp (Int2NumExpr e)         = mkStruct "int-to-num" [toLisp e]
  toLisp (Bool2NumExpr e)        = mkStruct "bool-to-num" [toLisp e]
  toLisp (Node2NumExpr e)        = mkStruct "node-to-num" [toLisp e]
  toLisp (Match2NumExpr e)       = mkStruct "match-to-num" [toLisp e]
  toLisp (Instruction2NumExpr e) = mkStruct "instr-to-num" [toLisp e]
  toLisp (Block2NumExpr e)       = mkStruct "block-to-num" [toLisp e]
  toLisp (Location2NumExpr e)    = mkStruct "loc-to-num" [toLisp e]

instance FromLisp IntExpr where
  parseLisp e =
        struct "int" AnIntegerExpr e

instance ToLisp IntExpr where
  toLisp (AnIntegerExpr i) = mkStruct "int" [toLisp i]

instance FromLisp NodeExpr where
  parseLisp e =
        struct "id" ANodeIDExpr e
    <|> struct "ai" ANodeArrayIndexExpr e
    <|> struct "node-selected-for-op" NodeSelectedForOperandExpr e

instance ToLisp NodeExpr where
  toLisp (ANodeIDExpr nid) = mkStruct "id" [toLisp nid]
  toLisp (ANodeArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp (NodeSelectedForOperandExpr e) =
    mkStruct "node-selected-for-op" [toLisp e]

instance FromLisp OperandExpr where
  parseLisp e =
        struct "id" AnOperandIDExpr e
    <|> struct "ai" AnOperandArrayIndexExpr e

instance ToLisp OperandExpr where
  toLisp (AnOperandIDExpr nid) = mkStruct "id" [toLisp nid]
  toLisp (AnOperandArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]

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

instance FromLisp BlockExpr where
  parseLisp e =
        struct "block-wherein-match-is-placed" BlockWhereinMatchIsPlacedExpr e
    <|> struct "block-wherein-data-is-defined" BlockWhereinDataIsDefinedExpr e
    <|> struct "block-of-bnode" BlockOfBlockNodeExpr e

instance ToLisp BlockExpr where
  toLisp (BlockWhereinMatchIsPlacedExpr e) =
    mkStruct "block-wherein-match-is-placed" [toLisp e]
  toLisp (BlockWhereinDataIsDefinedExpr e) =
    mkStruct "block-wherein-data-is-defined" [toLisp e]
  toLisp (BlockOfBlockNodeExpr e) = mkStruct "block-of-bnode" [toLisp e]

instance FromLisp LocationExpr where
  parseLisp (Lisp.Symbol "null") = return TheNullLocationExpr
  parseLisp e =
        struct "id" ALocationIDExpr e
    <|> struct "ai" ALocationArrayIndexExpr e
    <|> struct "loc-of-dnode" LocationOfValueNodeExpr e

instance ToLisp LocationExpr where
  toLisp TheNullLocationExpr = Lisp.Symbol "null"
  toLisp (ALocationIDExpr rid) = mkStruct "id" [toLisp rid]
  toLisp (ALocationArrayIndexExpr ai) = mkStruct "ai" [toLisp ai]
  toLisp (LocationOfValueNodeExpr e) =
    mkStruct "loc-of-dnode" [toLisp e]

instance FromLisp SetExpr where
  parseLisp e =
        struct "union" UnionSetExpr e
    <|> struct "intersect" IntersectSetExpr e
    <|> struct "diff" DiffSetExpr e
    <|> struct "loc-class" LocationClassExpr e

instance ToLisp SetExpr where
  toLisp (UnionSetExpr lhs rhs) =
    mkStruct "union" [toLisp lhs, toLisp rhs]
  toLisp (IntersectSetExpr lhs rhs) =
    mkStruct "intersect" [toLisp lhs, toLisp rhs]
  toLisp (DiffSetExpr lhs rhs) =
    mkStruct "diff" [toLisp lhs, toLisp rhs]
  toLisp (LocationClassExpr es) =
    mkStruct "loc-class" [Lisp.List (map toLisp es)]

instance FromLisp SetElemExpr where
  parseLisp e =
        struct "block-to-set-elem" Block2SetElemExpr e
    <|> struct "loc-to-set-elem" Location2SetElemExpr e

instance ToLisp SetElemExpr where
  toLisp (Block2SetElemExpr e)    = mkStruct "block-to-set-elem" [toLisp e]
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
