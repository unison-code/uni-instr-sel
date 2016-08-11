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
  ( addDataDefinitionConstraints
  , addFallThroughConstraints
  , addNewDataLocConstraints
  , addSameDataLocConstraints
  , mkDataDefinitionConstraints
  , mkFallThroughConstraints
  , mkNewDataLocConstraints
  , mkSameDataLocConstraints
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

-- | Creates constraints using 'mkDataDefinitionConstraints' and adds these (if
-- any) to the given 'OpStructure'.
addDataDefinitionConstraints
  :: NodeID
     -- ^ A value node.
  -> NodeID
     -- ^ A block node.
  -> OpStructure
     -- ^ The old structure.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addDataDefinitionConstraints d b os =
  addConstraints os (mkDataDefinitionConstraints d b)

-- | Creates data placment constraints such that the data of a particular value
-- node must be defined in a specific block.
mkDataDefinitionConstraints
  :: NodeID
     -- ^ A value node.
  -> NodeID
     -- ^ A block node.
  -> [Constraint]
mkDataDefinitionConstraints d b =
  [ BoolExprConstraint $
    EqExpr ( Block2NumExpr $
             BlockWhereinDataIsDefinedExpr $
             ANodeIDExpr d
           )
           ( Block2NumExpr $
             BlockOfBlockNodeExpr $
             ANodeIDExpr b
           )
  ]

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
  [ BoolExprConstraint $
    EqExpr ( Location2NumExpr $
             LocationOfValueNodeExpr $
             ANodeIDExpr d
           )
           ( Location2NumExpr $
             ALocationIDExpr reg
           )
  ]
mkNewDataLocConstraints regs d =
  [ BoolExprConstraint $
    InSetExpr ( Location2SetElemExpr $
                LocationOfValueNodeExpr $
                ANodeIDExpr d
              )
              ( LocationClassExpr $
                map ALocationIDExpr regs
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
  [ BoolExprConstraint $
    FallThroughFromMatchToBlockExpr ThisMatchExpr
                                    ( BlockOfBlockNodeExpr $
                                      ANodeIDExpr l
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
  where mkC n1 n2 = BoolExprConstraint $
                    EqExpr ( Location2NumExpr $
                             LocationOfValueNodeExpr $
                             ANodeIDExpr n1
                           )
                           ( Location2NumExpr $
                             LocationOfValueNodeExpr $
                             ANodeIDExpr n2
                           )
