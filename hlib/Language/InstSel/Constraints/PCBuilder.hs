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
  , addInterDataValConstraints
  )
where

import Language.InstSel.Constraints.Base
import Language.InstSel.Graphs
import Language.InstSel.OpStructures
import Data.Maybe



-------------
-- Functions
-------------

-- | Creates basic block allocation constraints for a pattern graph such that
-- the pattern instance must be allocated to the root label if there is such a
-- node. The produced constraints (if any) are added to the existing
-- 'OpStructure'.
addBBAllocConstraints :: OpStructure -> OpStructure
addBBAllocConstraints os =
  let g = osGraph os
      root_label = rootInCFG $ extractCFG g
  in if isJust root_label
     then let new_cs = [ BoolExprConstraint $
                           EqExpr
                           ( Label2NumExpr $
                               LabelOfLabelNodeExpr $
                                 ANodeIDExpr (getNodeID $ fromJust root_label)
                           )
                           ( Label2NumExpr $
                               LabelAllocatedToPatternInstanceExpr $
                                 ThisPatternInstanceExpr
                           )
                       ]
          in addConstraints os new_cs
     else os

-- | Creates intermediate data value constraints for a pattern graph which
-- contains data nodes which are both defined and used by the pattern but are
-- not specified as output nodes. Hence such data values are not accessible from
-- outside the pattern, and must be prevented from being used by other
-- patterns. The produced constraints (if any) are added to the existing
-- 'OpStructure'.

addInterDataValConstraints ::
     OpStructure
     -- ^ The old structure.
  -> [NodeID]
     -- ^ List of data nodes which are specified as output.
  -> OpStructure
     -- ^ The new structure, with the produced constraints (may be the same
     -- structure).
addInterDataValConstraints os outs =
  let g = osGraph os
      d_ns = filter isDataNode $ getAllNodes g
      d_use_def_ns = getNodeIDs $ [ n | n <- d_ns
                                      , hasAnyPredecessors g n
                                      , hasAnySuccessors g n
                                  ]
      inter_data_val_ns = filter (`notElem` outs) d_use_def_ns
      makeC n = BoolExprConstraint $ DataNodeIsIntermediateExpr (ANodeIDExpr n)
      new_cs = map makeC inter_data_val_ns
  in addConstraints os new_cs
