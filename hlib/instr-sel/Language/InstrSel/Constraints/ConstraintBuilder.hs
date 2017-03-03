{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Constraints.ConstraintBuilder
  ( addFallThroughConstraints
  , mkFallThroughConstraints
  , replaceNodeID
  )
where

import Language.InstrSel.Constraints.Base
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures



-------------
-- Functions
-------------

-- | Creates constraints using 'mkFallThroughConstraints' and adds these (if
-- any) to the given 'OpStructure'.
addFallThroughConstraints :: NodeID -> OpStructure -> OpStructure
addFallThroughConstraints l os =
  addConstraints os (mkFallThroughConstraints l)

-- | Creates constraints for enforcing a fall-through from a match to a block.
mkFallThroughConstraints
  :: NodeID
     -- ^ A block node.
  -> [Constraint]
mkFallThroughConstraints l =
  [ FallThroughFromMatchToBlockConstraint $
    BlockOfBlockNodeExpr $
    ANodeIDExpr l
  ]

-- | Replaces every occurrance of a node ID in the given constraint with another
-- node ID.
replaceNodeID
  :: NodeID
     -- ^ Node ID to replace.
  -> NodeID
     -- ^ Node ID to replace with.
  -> Constraint
  -> Constraint
replaceNodeID old_n new_n c =
  let def_r = mkDefaultReconstructor
      mkNodeExpr _ e@(ANodeIDExpr n) =
        if n == old_n then ANodeIDExpr new_n else e
      mkNodeExpr r expr = (mkNodeExprF def_r) r expr
      new_r = def_r { mkNodeExprF = mkNodeExpr }
  in apply new_r c
