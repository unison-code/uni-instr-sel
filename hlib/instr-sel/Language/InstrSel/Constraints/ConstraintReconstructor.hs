{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Constraints.ConstraintReconstructor
  ( Reconstructor (..)
  , mkDefaultReconstructor
  , apply
  )
where

import Language.InstrSel.Constraints.Base



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
      , mkOperandExprF     :: Reconstructor -> OperandExpr -> OperandExpr
      , mkMatchExprF       :: Reconstructor -> MatchExpr -> MatchExpr
      , mkInstructionExprF :: Reconstructor
                           -> InstructionExpr
                           -> InstructionExpr
      , mkBlockExprF       :: Reconstructor -> BlockExpr -> BlockExpr
      , mkLocationExprF    :: Reconstructor -> LocationExpr -> LocationExpr
      , mkSetExprF         :: Reconstructor -> SetExpr -> SetExpr
      , mkSetElemExprF     :: Reconstructor -> SetElemExpr -> SetElemExpr
      }



-------------
-- Functions
-------------

-- | Creates a 'Reconstructor' that traverses all parts of a constraint, but
-- reconstructs the exact same constraint. In other words, it essentially
-- applies 'id' on every part.
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
      mkBoolExpr r (FallThroughFromMatchToBlockExpr e) =
        FallThroughFromMatchToBlockExpr ((mkBlockExprF r) r e)
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
      mkNumExpr r (Block2NumExpr expr) =
        Block2NumExpr ((mkBlockExprF r) r expr)
      mkNumExpr r (Location2NumExpr expr) =
        Location2NumExpr ((mkLocationExprF r) r expr)
      mkIntExpr _ expr@(AnIntegerExpr _) =
        expr
      mkNodeExpr _ expr@(ANodeIDExpr _) =
        expr
      mkNodeExpr _ expr@(ANodeArrayIndexExpr _) =
        expr
      mkNodeExpr r (NodeSelectedForOperandExpr expr) =
        NodeSelectedForOperandExpr ((mkOperandExprF r) r expr)
      mkOperandExpr _ expr@(AnOperandIDExpr _) =
        expr
      mkOperandExpr _ expr@(AnOperandArrayIndexExpr _) =
        expr
      mkMatchExpr _ expr@(AMatchIDExpr _) =
        expr
      mkMatchExpr _ expr@(AMatchArrayIndexExpr _) =
        expr
      mkMatchExpr _ expr@(ThisMatchExpr) =
        expr
      mkInstructionExpr _ expr@(AnInstructionIDExpr _) =
        expr
      mkInstructionExpr _ expr@(AnInstructionArrayIndexExpr _) =
        expr
      mkInstructionExpr r (InstructionOfMatchExpr expr) =
        InstructionOfMatchExpr ((mkMatchExprF r) r expr)
      mkBlockExpr r (BlockOfBlockNodeExpr expr) =
        BlockOfBlockNodeExpr ((mkNodeExprF r) r expr)
      mkLocationExpr _ expr@(ALocationIDExpr _) =
        expr
      mkLocationExpr _ expr@(ALocationArrayIndexExpr _) =
        expr
      mkLocationExpr r (LocationOfValueNodeExpr expr) =
        LocationOfValueNodeExpr ((mkNodeExprF r) r expr)
      mkLocationExpr _ expr@(TheNullLocationExpr) =
        expr
      mkSetExpr r (UnionSetExpr lhs rhs) =
        UnionSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (IntersectSetExpr lhs rhs) =
        IntersectSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (DiffSetExpr lhs rhs) =
        DiffSetExpr ((mkSetExprF r) r lhs) ((mkSetExprF r) r rhs)
      mkSetExpr r (LocationClassExpr exprs) =
        LocationClassExpr (map ((mkLocationExprF r) r) exprs)
      mkSetElemExpr r (Block2SetElemExpr expr) =
        Block2SetElemExpr ((mkBlockExprF r) r expr)
      mkSetElemExpr r (Location2SetElemExpr expr) =
        Location2SetElemExpr ((mkLocationExprF r) r expr)
  in Reconstructor
       { mkConstraintF = mkConstraint
       , mkBoolExprF = mkBoolExpr
       , mkNumExprF = mkNumExpr
       , mkIntExprF = mkIntExpr
       , mkNodeExprF = mkNodeExpr
       , mkOperandExprF = mkOperandExpr
       , mkMatchExprF = mkMatchExpr
       , mkInstructionExprF = mkInstructionExpr
       , mkBlockExprF = mkBlockExpr
       , mkLocationExprF = mkLocationExpr
       , mkSetExprF = mkSetExpr
       , mkSetElemExprF = mkSetElemExpr
       }

-- | Applies a reconstructor on a given constraint.
apply :: Reconstructor -> Constraint -> Constraint
apply r = (mkConstraintF r) r
