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
  ( addMatchToBlockMovementConstraints
  , addFallThroughConstraints
  , addDataLocConstraints
  , addNoDataReuseConstraints
  , mkMatchPlacementConstraints
  , mkFallThroughConstraints
  , mkDataLocConstraints
  , mkNoDataReuseConstraints
  )
where

import Language.InstrSel.Constraints.Base
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
import Language.InstrSel.TargetMachines.IDs
  ( LocationID )

import Data.Maybe



-------------
-- Functions
-------------

-- | Creates constraints using 'mkMatchPlacementConstraints' and adds
-- these (if any) to the given 'OpStructure'.
addMatchToBlockMovementConstraints :: OpStructure -> OpStructure
addMatchToBlockMovementConstraints os =
  addConstraints os (mkMatchPlacementConstraints $ osGraph os)

-- | Creates constraints for a pattern graph such that the match must be placed
-- in entry block of the pattern graph (if it has such a block).
mkMatchPlacementConstraints :: Graph -> [Constraint]
mkMatchPlacementConstraints g =
  let entry_block = rootInCFG $ extractCFG g
  in if isJust entry_block
     then [ BoolExprConstraint
            $ EqExpr ( Block2NumExpr
                       $ BlockOfBlockNodeExpr
                       $ ANodeIDExpr (getNodeID $ fromJust entry_block)
                     )
                     ( Block2NumExpr
                       $ BlockWhereinMatchIsPlacedExpr
                       $ ThisMatchExpr
                     )
          ]
     else []

-- | Creates constraints using 'mkDataLocConstraints' and adds these (if any)
-- are added to the given 'OpStructure'.
addDataLocConstraints
  :: [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A value node.
  -> OpStructure
     -- ^ The old structure.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addDataLocConstraints regs d os =
  addConstraints os (mkDataLocConstraints regs d)

-- | Creates location constraints such that the data of a particular value node
-- must be in one of a particular set of locations.
mkDataLocConstraints
  :: [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A value node.
  -> [Constraint]
mkDataLocConstraints [reg] d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfValueNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ ALocationIDExpr reg
             )
  ]
mkDataLocConstraints regs d =
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
