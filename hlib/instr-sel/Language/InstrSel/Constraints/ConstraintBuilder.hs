{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
  )
where

import Language.InstrSel.Constraints.Base
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
  [ BoolExprConstraint $
    FallThroughFromMatchToBlockExpr $
    BlockOfBlockNodeExpr $
    ANodeIDExpr l
  ]
