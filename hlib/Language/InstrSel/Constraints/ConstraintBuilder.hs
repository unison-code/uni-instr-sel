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
  , mkMatchToBlockMovementConstraints
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

-- | Creates constraints using 'mkMatchToBlockMovementConstraints' and adds
-- these (if any) to the given 'OpStructure'.
addMatchToBlockMovementConstraints :: OpStructure -> OpStructure
addMatchToBlockMovementConstraints os =
  addConstraints os (mkMatchToBlockMovementConstraints $ osGraph os)

-- | Creates block movement constraints for a pattern graph such that the match
-- must be moved to the block of the entry label node (if there is such a node).
mkMatchToBlockMovementConstraints :: Graph -> [Constraint]
mkMatchToBlockMovementConstraints g =
  let entry_label = rootInCFG $ extractCFG g
  in if isJust entry_label
     then [ BoolExprConstraint
            $ EqExpr ( Block2NumExpr
                       $ BlockOfLabelNodeExpr
                       $ ANodeIDExpr (getNodeID $ fromJust entry_label)
                     )
                     ( Block2NumExpr
                       $ BlockToWhereMatchIsMovedExpr
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
     -- ^ A data node.
  -> OpStructure
     -- ^ The old structure.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addDataLocConstraints regs d os =
  addConstraints os (mkDataLocConstraints regs d)

-- | Creates location constraints such that the data of a particular data node
-- must be in one of a particular set of locations.
mkDataLocConstraints
  :: [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A data node.
  -> [Constraint]
mkDataLocConstraints [reg] d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfDataNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ ALocationIDExpr reg
             )
  ]
mkDataLocConstraints regs d =
  [ BoolExprConstraint
    $ InSetExpr ( Location2SetElemExpr
                  $ LocationOfDataNodeExpr
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
     -- ^ A label node.
  -> [Constraint]
mkFallThroughConstraints l =
  [ BoolExprConstraint
    $ FallThroughFromMatchToBlockExpr ThisMatchExpr
                                      ( BlockOfLabelNodeExpr
                                        $ ANodeIDExpr l
                                      )
  ]

-- | Creates constraints using 'mkNoDataReuseConstraints' and adds these (if
-- any) to the given 'OpStructure'.
addNoDataReuseConstraints :: NodeID -> OpStructure -> OpStructure
addNoDataReuseConstraints d os =
  addConstraints os (mkNoDataReuseConstraints d)

-- | Creates no-reuse constraints for a pattern graph such that the location of
-- the given data node must be in the null location.
mkNoDataReuseConstraints :: NodeID -> [Constraint]
mkNoDataReuseConstraints d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfDataNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ TheNullLocationExpr
             )
  ]
