--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Constraints.ConstraintReconstructor
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides functions for reconstructing a constraint by traversing over all of
-- its parts and applying a specific function for reconstructing each part.
--
--------------------------------------------------------------------------------

module Language.InstSel.Constraints.ConstraintReconstructor
  ( Reconstructor (..)
  , mkDefaultReconstructor
  , apply
  )
where

import Language.InstSel.Constraints.Base



--------------
-- Data types
--------------

-- | Contains all the reconstruction functions. The idea is that each function
-- will recursively call the appropriate reconstruct function on every argument
-- of the part, thus traversing the entire constraint and reconstructing it from
-- the bottom up.
data Reconstructor
  = Reconstructor
      { mkConstraintF      :: Reconstructor -> Constraint -> Constraint
      , mkBoolExprF        :: Reconstructor -> BoolExpr -> BoolExpr
      , mkNumExprF         :: Reconstructor -> NumExpr -> NumExpr
      , mkIntExprF         :: Reconstructor -> IntExpr -> IntExpr
      , mkNodeExprF        :: Reconstructor -> NodeExpr -> NodeExpr
      , mkMatchExprF       :: Reconstructor -> MatchExpr -> MatchExpr
      , mkInstructionExprF :: Reconstructor
                           -> InstructionExpr
                           -> InstructionExpr
      , mkLabelExprF       :: Reconstructor -> LabelExpr -> LabelExpr
      , mkRegisterExprF    :: Reconstructor -> RegisterExpr -> RegisterExpr
      , mkSetExprF         :: Reconstructor -> SetExpr -> SetExpr
      , mkSetElemExprF     :: Reconstructor -> SetElemExpr -> SetElemExpr
      }



-------------
-- Functions
-------------

-- | Creates a @Reconstructor@ that traverses all parts of a constraint, but
-- reconstructs the exact same constraint. In other words, it essentially
-- applies @id@ on every part.
mkDefaultReconstructor :: Reconstructor
mkDefaultReconstructor =
  let mkConstraint r (BoolExprConstraint expr) =
        BoolExprConstraint ((mkBoolExprF r) r expr)
      mkBoolExpr r (EqExpr  lhs rhs) =
        EqExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (NEqExpr lhs rhs) =
        NEqExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (GTExpr  lhs rhs) =
        GTExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (GEExpr  lhs rhs) =
        GEExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (LTExpr  lhs rhs) =
        LTExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (LEExpr  lhs rhs) =
        LEExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkBoolExpr r (AndExpr lhs rhs) =
        AndExpr ((mkBoolExprF r) r lhs) ((mkBoolExprF r) r rhs)
      mkBoolExpr r (OrExpr  lhs rhs) =
        OrExpr ((mkBoolExprF r) r lhs) ((mkBoolExprF r) r rhs)
      mkBoolExpr r (ImpExpr lhs rhs) =
        ImpExpr ((mkBoolExprF r) r lhs) ((mkBoolExprF r) r rhs)
      mkBoolExpr r (EqvExpr lhs rhs) =
        EqvExpr ((mkBoolExprF r) r lhs) ((mkBoolExprF r) r rhs)
      mkBoolExpr r (NotExpr expr) =
        NotExpr ((mkBoolExprF r) r expr)
      mkBoolExpr r (InSetExpr lhs rhs) =
        InSetExpr ((mkSetElemExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkBoolExpr r (DataNodeIsAnIntConstantExpr expr) =
        DataNodeIsAnIntConstantExpr ((mkNodeExprF r) r expr)
      mkBoolExpr r (DataNodeIsIntermediateExpr expr) =
        DataNodeIsIntermediateExpr ((mkNodeExprF r) r expr)
      mkNumExpr r (PlusExpr lhs rhs) =
        PlusExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkNumExpr r (MinusExpr lhs rhs) =
        MinusExpr ((mkNumExprF r) r lhs) ((mkNumExprF r) r rhs)
      mkNumExpr r (Int2NumExpr expr) =
        Int2NumExpr ((mkIntExprF r) r expr)
      mkNumExpr r (Bool2NumExpr expr) =
        Bool2NumExpr ((mkBoolExprF r) r expr)
      mkNumExpr r (Node2NumExpr expr) =
        Node2NumExpr ((mkNodeExprF r) r expr)
      mkNumExpr r (Match2NumExpr expr) =
        Match2NumExpr ((mkMatchExprF r) r expr)
      mkNumExpr r (Instruction2NumExpr expr) =
        Instruction2NumExpr ((mkInstructionExprF r) r expr)
      mkNumExpr r (Label2NumExpr expr) =
        Label2NumExpr ((mkLabelExprF r) r expr)
      mkNumExpr r (Register2NumExpr expr) =
        Register2NumExpr ((mkRegisterExprF r) r expr)
      mkNumExpr r (DistanceBetweenMatchAndLabelExpr match_expr label_expr) =
        DistanceBetweenMatchAndLabelExpr
          ((mkMatchExprF r) r match_expr)
          ((mkLabelExprF r) r label_expr)
      mkIntExpr _ expr@(AnIntegerExpr _) =
        expr
      mkIntExpr r (IntConstValueOfDataNodeExpr expr) =
        IntConstValueOfDataNodeExpr ((mkNodeExprF r) r expr)
      mkNodeExpr _ expr@(ANodeIDExpr _) =
        expr
      mkNodeExpr _ expr@(ANodeArrayIndexExpr _) =
        expr
      mkMatchExpr _ expr@(AMatchIDExpr _) =
        expr
      mkMatchExpr _ expr@(AMatchArrayIndexExpr _) =
        expr
      mkMatchExpr _ expr@(ThisMatchExpr) =
        expr
      mkMatchExpr r (CovererOfOperationNodeExpr expr) =
        CovererOfOperationNodeExpr ((mkNodeExprF r) r expr)
      mkMatchExpr r (DefinerOfDataNodeExpr expr) =
        DefinerOfDataNodeExpr ((mkNodeExprF r) r expr)
      mkMatchExpr r (DefinerOfStateNodeExpr expr) =
        DefinerOfStateNodeExpr ((mkNodeExprF r) r expr)
      mkInstructionExpr _ expr@(AnInstructionIDExpr _) =
        expr
      mkInstructionExpr _ expr@(AnInstructionArrayIndexExpr _) =
        expr
      mkInstructionExpr r (InstructionOfMatchExpr expr) =
        InstructionOfMatchExpr ((mkMatchExprF r) r expr)
      mkLabelExpr r (LabelAllocatedToMatchExpr expr) =
        LabelAllocatedToMatchExpr ((mkMatchExprF r) r expr)
      mkLabelExpr r (LabelOfLabelNodeExpr expr) =
        LabelOfLabelNodeExpr ((mkNodeExprF r) r expr)
      mkRegisterExpr _ expr@(ARegisterIDExpr _) =
        expr
      mkRegisterExpr _ expr@(ARegisterArrayIndexExpr _) =
        expr
      mkRegisterExpr r (RegisterAllocatedToDataNodeExpr expr) =
        RegisterAllocatedToDataNodeExpr ((mkNodeExprF r) r expr)
      mkSetExpr r (UnionSetExpr lhs rhs) =
        UnionSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (IntersectSetExpr lhs rhs) =
        IntersectSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (DiffSetExpr lhs rhs) =
        DiffSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (DomSetOfLabelExpr expr) =
        DomSetOfLabelExpr ((mkLabelExprF r) r expr)
      mkSetExpr r (RegisterClassExpr exprs) =
        RegisterClassExpr (map ((mkRegisterExprF r) r) exprs)
      mkSetElemExpr r (Label2SetElemExpr expr) =
        Label2SetElemExpr ((mkLabelExprF r) r expr)
      mkSetElemExpr r (Register2SetElemExpr expr) =
        Register2SetElemExpr ((mkRegisterExprF r) r expr)
  in Reconstructor
       { mkConstraintF = mkConstraint
       , mkBoolExprF = mkBoolExpr
       , mkNumExprF = mkNumExpr
       , mkIntExprF = mkIntExpr
       , mkNodeExprF = mkNodeExpr
       , mkMatchExprF = mkMatchExpr
       , mkInstructionExprF = mkInstructionExpr
       , mkLabelExprF = mkLabelExpr
       , mkRegisterExprF = mkRegisterExpr
       , mkSetExprF = mkSetExpr
       , mkSetElemExprF = mkSetElemExpr
       }

-- | Applies a reconstructor on a given constraint.
apply :: Reconstructor -> Constraint -> Constraint
apply r = (mkConstraintF r) r
