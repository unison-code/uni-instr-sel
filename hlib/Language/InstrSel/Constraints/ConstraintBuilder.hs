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
  , addInterDataValConstraints
  , addDataLocConstraints
  , mkBBMoveConstraints
  , mkFallthroughConstraints
  , mkInterDataValConstraints
  , mkDataLocConstraints
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

-- | Creates intermediate data value constraints (see
-- `mkInterDataValConstraints`) and adds these (if any) are added to the
-- existing 'OpStructure'.
addInterDataValConstraints
  :: OpStructure
     -- ^ The old structure.
  -> [NodeID]
     -- ^ List of data nodes which are specified as output.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addInterDataValConstraints os outs =
  addConstraints os (mkInterDataValConstraints (osGraph os) outs)

-- | Creates intermediate data value constraints for a pattern graph which
-- contains data nodes which are both defined and used by the pattern but are
-- not specified as output nodes. Hence such data values are not accessible from
-- outside the pattern, and must be prevented from being used by other
-- patterns.
mkInterDataValConstraints
  :: Graph
     -- ^ The pattern graph.
  -> [NodeID]
     -- ^ List of data nodes which are specified as output.
  -> [Constraint]
mkInterDataValConstraints g outs =
  let d_ns = filter isDataNode $ getAllNodes g
      d_use_def_ns = getNodeIDs $ [ n | n <- d_ns
                                      , hasAnyPredecessors g n
                                      , hasAnySuccessors g n
                                  ]
      inter_data_val_ns = filter (`notElem` outs) d_use_def_ns
      makeC n = BoolExprConstraint $ DataNodeIsIntermediateExpr (ANodeIDExpr n)
  in map makeC inter_data_val_ns

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
