--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Constraints.PCBuilder
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

module Language.InstSel.Constraints.PCBuilder
  ( addBBAllocConstraints
  , addIntConstConstraints
  , addIntRangeConstraints
  , addInterDataValConstraints
  , addRegAllocConstraints
  , mkBBAllocConstraints
  , mkIntConstConstraints
  , mkIntRangeConstraints
  , mkInterDataValConstraints
  , mkRegAllocConstraints
  )
where

import Language.InstSel.Constraints.Base
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import Language.InstSel.TargetMachine.IDs
  ( RegisterID )
import Language.InstSel.Utils
  ( Range (..) )
import Data.Maybe



-------------
-- Functions
-------------

-- | Creates basic block allocation constraints (see `mkBBAllocConstraints`) and
-- adds these (if any) to the existing 'OpStructure'.
addBBAllocConstraints :: OpStructure -> OpStructure
addBBAllocConstraints os =
  addConstraints os (mkBBAllocConstraints $ osGraph os)

-- | Creates basic block allocation constraints for a pattern graph such that
-- the match must be allocated to the root label if there is such a node.
mkBBAllocConstraints :: Graph -> [Constraint]
mkBBAllocConstraints g =
  let root_label = rootInCFG $ extractCFG g
  in if isJust root_label
     then [ BoolExprConstraint $
              EqExpr
                ( Label2NumExpr $
                    LabelOfLabelNodeExpr $
                      ANodeIDExpr (getNodeID $ fromJust root_label)
                )
                ( Label2NumExpr $
                    LabelAllocatedToMatchExpr $
                      ThisMatchExpr
                )
          ]
     else []

-- | Creates intermediate data value constraints (see
-- `mkInterDataValConstraints`) and adds these (if any) are added to the
-- existing 'OpStructure'.
addInterDataValConstraints ::
     OpStructure
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
mkInterDataValConstraints ::
     Graph
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

-- | Creates register allocation constraints (see `mkRegAllocConstraints`) and
-- adds these (if any) are added to the existing 'OpStructure'.
addRegAllocConstraints ::
     OpStructure
     -- ^ The old structure.
  -> NodeID
     -- ^ A data node.
  -> [RegisterID]
     -- ^ List of registers to which the data can be allocated.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addRegAllocConstraints os d regs =
  addConstraints os (mkRegAllocConstraints d regs)

-- | Creates register allocation constraints such that the data of a particular
-- data node must be allocated one of a particular set of registers.
mkRegAllocConstraints ::
     NodeID
     -- ^ A data node.
  -> [RegisterID]
     -- ^ List of registers to which the data can be allocated.
  -> [Constraint]
mkRegAllocConstraints d regs =
  [ BoolExprConstraint $
      InSetExpr
        ( Register2SetElemExpr $
            RegisterAllocatedToDataNodeExpr $
              ANodeIDExpr d
        )
        ( RegisterClassExpr $
            map ARegisterIDExpr regs
        )
  ]

-- | Creates integer constant constraints (see 'mkIntConstConstraints'), and
-- adds (if any) these to the existing 'OpStructure'.
addIntConstConstraints ::
     OpStructure
     -- ^ The old structure.
  -> NodeID
     -- ^ A data node.
  -> Integer
     -- ^ A signed integer constant.
  -> OpStructure
     -- ^ The new structure.
addIntConstConstraints os d v =
  addConstraints os (mkIntConstConstraints d v)

-- | Creates constraints such that the value of particular data node is a
-- constant integer value.
mkIntConstConstraints ::
     NodeID
     -- ^ A data node.
  -> Integer
     -- ^ A signed integer constant.
  -> [Constraint]
mkIntConstConstraints d v =
  [ BoolExprConstraint $
      DataNodeIsAnIntConstantExpr $
        ANodeIDExpr d
  , BoolExprConstraint $
      EqExpr
        ( Int2NumExpr $
            AnIntegerExpr v
        )
        ( Int2NumExpr $
            IntConstValueOfDataNodeExpr $
              ANodeIDExpr d
        )
  ]

-- | Creates integer value range constraints (see 'mkIntRangeConstraints'), and
-- adds (if any) these to the existing 'OpStructure'.
addIntRangeConstraints ::
     OpStructure
     -- ^ The old structure.
  -> NodeID
     -- ^ A data node.
  -> Range Integer
     -- ^ An inclusive signed integer range.
  -> OpStructure
     -- ^ The new structure.
addIntRangeConstraints os d v =
  addConstraints os (mkIntRangeConstraints d v)

-- | Creates constraints such that the value of particular data node is an
-- integer value that is within a given range.
mkIntRangeConstraints ::
     NodeID
     -- ^ A data node.
  -> Range Integer
     -- ^ An inclusive signed integer range.
  -> [Constraint]
mkIntRangeConstraints d (Range { lowerBound = low_v, upperBound = up_v }) =
  [ BoolExprConstraint $
      DataNodeIsAnIntConstantExpr $
        ANodeIDExpr d
  , BoolExprConstraint $
      AndExpr
        ( LEExpr
            ( Int2NumExpr $
                AnIntegerExpr low_v
            )
            ( Int2NumExpr $
                IntConstValueOfDataNodeExpr $
                  ANodeIDExpr d
            )
        )
        ( LEExpr
            ( Int2NumExpr $
                IntConstValueOfDataNodeExpr $
                  ANodeIDExpr d
            )
            ( Int2NumExpr $
                AnIntegerExpr up_v
            )
        )
  ]
