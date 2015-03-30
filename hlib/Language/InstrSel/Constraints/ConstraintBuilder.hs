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
  ( addBBMoveConstraints
  , addFallthroughConstraints
  , addDataLocConstraints
  , addNoReuseConstraints
  , mkBBMoveConstraints
  , mkFallthroughConstraints
  , mkDataLocConstraints
  , mkNoReuseConstraints
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

-- | Creates basic block movement constraints (see `mkBBMoveConstraints`) and
-- adds these (if any) to the existing 'OpStructure'.
addBBMoveConstraints :: OpStructure -> OpStructure
addBBMoveConstraints os =
  addConstraints os (mkBBMoveConstraints $ osGraph os)

-- | Creates basic block movement constraints for a pattern graph such that the
-- match must be moved to the entry label if there is such a node.
mkBBMoveConstraints :: Graph -> [Constraint]
mkBBMoveConstraints g =
  let entry_label = rootInCFG $ extractCFG g
  in if isJust entry_label
     then [ BoolExprConstraint
            $ EqExpr ( Label2NumExpr
                       $ LabelOfLabelNodeExpr
                       $ ANodeIDExpr (getNodeID $ fromJust entry_label)
                     )
                     ( Label2NumExpr
                       $ LabelToWhereMatchIsMovedExpr
                       $ ThisMatchExpr
                     )
          ]
     else []

-- | Creates location allocation constraints (see `mkDataLocConstraints`) and
-- adds these (if any) are added to the existing 'OpStructure'.
addDataLocConstraints
  :: OpStructure
     -- ^ The old structure.
  -> [LocationID]
     -- ^ List of locations to which the data can be allocated.
  -> NodeID
     -- ^ A data node.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addDataLocConstraints os regs d =
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

-- | Creates fallthrough constraints (see `mkFallThroughConstraints`) and adds
-- these (if any) to the existing 'OpStructure'.
addFallthroughConstraints :: OpStructure -> NodeID -> OpStructure
addFallthroughConstraints os l =
  addConstraints os (mkFallthroughConstraints l)

-- | Creates constraints for enforcing branch fallthrough, meaning that the
-- distance between the basic block to which to jump and the basic block from
-- which to jump must be zero.
mkFallthroughConstraints
  :: NodeID
     -- ^ A label node.
  -> [Constraint]
mkFallthroughConstraints l =
  [ BoolExprConstraint
    $ EqExpr ( DistanceBetweenMatchAndLabelExpr
                 ThisMatchExpr
                 ( LabelOfLabelNodeExpr
                   $ ANodeIDExpr l
                 )
             )
             ( Int2NumExpr
               $ AnIntegerExpr 0
             )
  ]

-- | Creates no-reuse constraints (see `mkNoReuseConstraints`) and adds these
-- (if any) to the existing 'OpStructure'.
addNoReuseConstraints :: OpStructure -> NodeID -> OpStructure
addNoReuseConstraints os d =
  addConstraints os (mkNoReuseConstraints d)

-- | Creates no-reuse constraints for a pattern graph such that the location of
-- the given data node must be in the null location.
mkNoReuseConstraints :: NodeID -> [Constraint]
mkNoReuseConstraints d =
  [ BoolExprConstraint
    $ EqExpr ( Location2NumExpr
               $ LocationOfDataNodeExpr
               $ ANodeIDExpr d
             )
             ( Location2NumExpr
               $ TheNullLocationExpr
             )
  ]
