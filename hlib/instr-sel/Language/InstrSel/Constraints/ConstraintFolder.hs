{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Constraints.ConstraintFolder
  ( Folder (..)
  , mkDefaultFolder
  , apply
  )
where

import Language.InstrSel.Constraints.Base



--------------
-- Data types
--------------

-- | Contains all the folder functions. The idea is that each function will
-- recursively call the appropriate reconstruct function on every argument of
-- the part, thus traversing the entire constraint and folding it from the
-- bottom up.
--
-- See also the 'ConstraintReconstructor' module.
data Folder a
  = Folder
      { foldConstraintF      :: Folder a -> Constraint -> a
      , foldBoolExprF        :: Folder a -> BoolExpr -> a
      , foldNumExprF         :: Folder a -> NumExpr -> a
      , foldIntExprF         :: Folder a -> IntExpr -> a
      , foldNodeExprF        :: Folder a -> NodeExpr -> a
      , foldOperandExprF     :: Folder a -> OperandExpr -> a
      , foldMatchExprF       :: Folder a -> MatchExpr -> a
      , foldInstructionExprF :: Folder a -> InstructionExpr -> a
      , foldBlockExprF       :: Folder a -> BlockExpr -> a
      , foldSetExprF         :: Folder a -> SetExpr -> a
      , foldSetElemExprF     :: Folder a -> SetElemExpr -> a
      , foldDefValue         :: a
      , foldValuesF          :: a -> a -> a
      }



-------------
-- Functions
-------------

-- | Creates a 'Folder' that traverses all parts of a constraint, returns the
-- default value for each leaf, and folds all returned values as post-step.
mkDefaultFolder
  :: a
     -- ^ Default fold value.
  -> (a -> a -> a)
     -- ^ Folding function.
  -> Folder a
mkDefaultFolder def_v fold_f =
  let foldConstraint f (BoolExprConstraint expr) =
        (foldBoolExprF f) f expr
      foldBoolExpr f (EqExpr  lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (NEqExpr lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (GTExpr  lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (GEExpr  lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (LTExpr  lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (LEExpr  lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldBoolExpr f (AndExpr lhs rhs) =
        (foldValuesF f) ((foldBoolExprF f) f lhs) ((foldBoolExprF f) f rhs)
      foldBoolExpr f (OrExpr  lhs rhs) =
        (foldValuesF f) ((foldBoolExprF f) f lhs) ((foldBoolExprF f) f rhs)
      foldBoolExpr f (ImpExpr lhs rhs) =
        (foldValuesF f) ((foldBoolExprF f) f lhs) ((foldBoolExprF f) f rhs)
      foldBoolExpr f (EqvExpr lhs rhs) =
        (foldValuesF f) ((foldBoolExprF f) f lhs) ((foldBoolExprF f) f rhs)
      foldBoolExpr f (NotExpr expr) =
        (foldBoolExprF f) f expr
      foldBoolExpr f (InSetExpr lhs rhs) =
        (foldValuesF f) ((foldSetElemExprF f) f lhs) ((foldSetExprF f) f rhs)
      foldBoolExpr f (FallThroughFromMatchToBlockExpr expr) =
        (foldBlockExprF f) f expr
      foldNumExpr f (PlusExpr lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldNumExpr f (MinusExpr lhs rhs) =
        (foldValuesF f) ((foldNumExprF f) f lhs) ((foldNumExprF f) f rhs)
      foldNumExpr f (Int2NumExpr expr) =
        (foldIntExprF f) f expr
      foldNumExpr f (Bool2NumExpr expr) =
        (foldBoolExprF f) f expr
      foldNumExpr f (Node2NumExpr expr) =
        (foldNodeExprF f) f expr
      foldNumExpr f (Match2NumExpr expr) =
        (foldMatchExprF f) f expr
      foldNumExpr f (Instruction2NumExpr expr) =
        (foldInstructionExprF f) f expr
      foldNumExpr f (Block2NumExpr expr) =
        (foldBlockExprF f) f expr
      foldIntExpr f (AnIntegerExpr _) =
        foldDefValue f
      foldNodeExpr f (ANodeIDExpr _) =
        foldDefValue f
      foldNodeExpr f (ANodeArrayIndexExpr _) =
        foldDefValue f
      foldNodeExpr f (NodeSelectedForOperandExpr expr) =
        (foldOperandExprF f) f expr
      foldOperandExpr f (AnOperandIDExpr _) =
        foldDefValue f
      foldOperandExpr f (AnOperandArrayIndexExpr _) =
        foldDefValue f
      foldMatchExpr f (AMatchIDExpr _) =
        foldDefValue f
      foldMatchExpr f (AMatchArrayIndexExpr _) =
        foldDefValue f
      foldMatchExpr f (ThisMatchExpr) =
        foldDefValue f
      foldInstructionExpr f (AnInstructionIDExpr _) =
        foldDefValue f
      foldInstructionExpr f (AnInstructionArrayIndexExpr _) =
        foldDefValue f
      foldInstructionExpr f (InstructionOfMatchExpr expr) =
        (foldMatchExprF f) f expr
      foldBlockExpr f (BlockOfBlockNodeExpr expr) =
        (foldNodeExprF f) f expr
      foldSetExpr f (UnionSetExpr lhs rhs) =
        (foldValuesF f) ((foldSetExprF f) f lhs) ((foldSetExprF f) f rhs)
      foldSetExpr f (IntersectSetExpr lhs rhs) =
        (foldValuesF f) ((foldSetExprF f) f lhs) ((foldSetExprF f) f rhs)
      foldSetExpr f (DiffSetExpr lhs rhs) =
        (foldValuesF f) ((foldSetExprF f) f lhs) ((foldSetExprF f) f rhs)
      foldSetElemExpr f (Block2SetElemExpr expr) =
        (foldBlockExprF f) f expr
  in Folder
       { foldConstraintF = foldConstraint
       , foldBoolExprF = foldBoolExpr
       , foldNumExprF = foldNumExpr
       , foldIntExprF = foldIntExpr
       , foldNodeExprF = foldNodeExpr
       , foldOperandExprF = foldOperandExpr
       , foldMatchExprF = foldMatchExpr
       , foldInstructionExprF = foldInstructionExpr
       , foldBlockExprF = foldBlockExpr
       , foldSetExprF = foldSetExpr
       , foldSetElemExprF = foldSetElemExpr
       , foldDefValue = def_v
       , foldValuesF = fold_f
       }

-- | Applies a folder on a given constraint.
apply :: Folder a -> Constraint -> a
apply f = (foldConstraintF f) f
