--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Constraints.ConstraintBuilder
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides functions for creating various constraints to be used as part of the
-- function graph or pattern graphs.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Constraints.ConstraintBuilder
  ( addFallThroughConstraints
  , addNewDataLocConstraints
  , addSameDataLocConstraints
  , addNoDataReuseConstraints
  , mkFallThroughConstraints
  , mkNewDataLocConstraints
  , mkSameDataLocConstraints
  , mkNoDataReuseConstraints
  )
where

import Language.InstrSel.Constraints.Base
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.TargetMachines.IDs
  ( LocationID )



-------------
-- Functions
-------------

-- | Creates constraints using 'mkNewDataLocConstraints' and adds these (if any)
--  to the given 'OpStructure'.
addNewDataLocConstraints
  :: [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A value node.
  -> OpStructure
     -- ^ The old structure.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addNewDataLocConstraints regs d os =
  addConstraints os (mkNewDataLocConstraints regs d)

-- | Creates location constraints such that the data of a particular value node
-- must be in one of a particular set of locations.
mkNewDataLocConstraints
  :: [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A value node.
  -> [Constraint]
mkNewDataLocConstraints [reg] d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfValueNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ ALocationIDExpr reg
             )
  ]
mkNewDataLocConstraints regs d =
  [ BoolExprConstraint
    $ InSetExpr ( Location2SetElemExpr
                  $ LocationOfValueNodeExpr
                  $ ANodeIDExpr d
                )
                ( LocationClassExpr
                  $ map ALocationIDExpr regs
                )
  ]

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
  [ BoolExprConstraint
    $ FallThroughFromMatchToBlockExpr ThisMatchExpr
                                      ( BlockOfBlockNodeExpr
                                        $ ANodeIDExpr l
                                      )
  ]

-- | Creates constraints using 'mkNoDataReuseConstraints' and adds these (if
-- any) to the given 'OpStructure'.
addNoDataReuseConstraints :: NodeID -> OpStructure -> OpStructure
addNoDataReuseConstraints d os =
  addConstraints os (mkNoDataReuseConstraints d)

-- | Creates no-reuse constraints for a pattern graph such that the location of
-- the given value node must be in the null location.
mkNoDataReuseConstraints :: NodeID -> [Constraint]
mkNoDataReuseConstraints d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfValueNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ TheNullLocationExpr
             )
  ]

-- | Creates constraints, using 'mkSameDataLocConstraints', that force the
-- locations of a given list of value nodes to be the same, and adds these
-- constraints to the given 'OpStructure'.
addSameDataLocConstraints :: [NodeID] -> OpStructure -> OpStructure
addSameDataLocConstraints ns os =
  addConstraints os (mkSameDataLocConstraints ns)

-- | Creates constraints that force the locations of a list of value nodes to be
-- the same.
mkSameDataLocConstraints :: [NodeID] -> [Constraint]
mkSameDataLocConstraints [] = []
mkSameDataLocConstraints [_] = []
mkSameDataLocConstraints ns =
  map (mkC $ head ns) (tail ns)
  where mkC n1 n2 = BoolExprConstraint
                    $ EqExpr ( Location2NumExpr
                                $ LocationOfValueNodeExpr
                                $ ANodeIDExpr n1
                              )
                              ( Location2NumExpr
                                $ LocationOfValueNodeExpr
                                $ ANodeIDExpr n2
                              )
