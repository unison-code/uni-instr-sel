--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Constraints.ConstraintQuerier
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides functions for making various queries on constraints.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Constraints.ConstraintQuerier
  ( extractDataLocsInConstraints )
where

import Language.InstrSel.Constraints.Base
import Language.InstrSel.Graphs.IDs
  ( NodeID )
import Language.InstrSel.TargetMachines.IDs
  ( LocationID )

import Data.Maybe
  ( mapMaybe )



-------------
-- Functions
-------------

-- | Extracts the locations that participate in the constraints which state that
-- the data of a particular value node must be in one of a particular set of
-- locations. It is assumed that such constraints have been constructed using
-- the 'mkNewDataLocConstraints' function in the 'ConstraintBuilder' module.
extractDataLocsInConstraints
  :: NodeID
     -- ^ The value node for which to extract the location requirements.
  -> [Constraint]
  -> [LocationID]
extractDataLocsInConstraints n cs =
  concatMap f cs
  where f ( BoolExprConstraint
              ( InSetExpr ( Location2SetElemExpr
                            ( LocationOfValueNodeExpr
                              ( ANodeIDExpr n' )
                            )
                          )
                          ( LocationClassExpr exprs )
              )
          ) = if n' == n then mapMaybe g exprs else []
        f _ = []
        g (ALocationIDExpr l) = Just l
        g _ = Nothing
