--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.Constraints.PCBuilder
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Creates pattern constraints.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Constraints.PCBuilder (
  mkBBAllocConstraints
) where

import Language.InstructionSelection.Constraints.Base
import Language.InstructionSelection.Graphs
import Data.Maybe



-------------
-- Functions
-------------

-- | Creates basic block allocation constraints for a pattern such that the
-- pattern instance must be allocated to the root label if there is such a node;
-- otherwise an empty list is returned.

mkBBAllocConstraints :: Graph -> [Constraint]
mkBBAllocConstraints g =
  let root_label = rootInCFG $ extractCFG g
  in if isJust root_label
        then [ BoolExprConstraint $
               EqExpr
               (
                 LabelID2NumExpr $
                 LabelIDOfLabelNodeExpr $
                 ANodeIDExpr (nodeID $ fromJust root_label)
               )
               (
                 LabelID2NumExpr $
                 LabelIDAllocatedToInstanceExpr $
                 ThisPatternInstanceIDExpr
               )
             ]
        else []
