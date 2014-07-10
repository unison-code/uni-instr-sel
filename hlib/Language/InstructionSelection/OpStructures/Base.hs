--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstructionSelection.OpStructures.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types and functions for representing the operation
-- structures that constitute the input programs and patterns.
--
-- Both constants and immediate symbols are mapped to a constant node on which a
-- 'ConstantValueConstraint' applies to restrict the value. The same goes for
-- temporaries (which will later be allocated to a register) and data nodes
-- whose value must be allocated to a specific register.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.OpStructures.Base
  ( OpStructure (..)
  , addConstraint
  , addConstraints
  , mkEmpty
  )
where

import Language.InstructionSelection.Constraints
import qualified Language.InstructionSelection.Graphs as G



--------------
-- Data types
--------------

data OpStructure
    = OpStructure {
          osGraph :: G.Graph
        , osConstraints :: [Constraint]
      }
    deriving (Show)



-------------
-- Functions
-------------

-- | Creates an empty operation structure.

mkEmpty :: OpStructure
mkEmpty = OpStructure G.mkEmpty []

addConstraint :: OpStructure -> Constraint -> OpStructure
addConstraint os c = os { osConstraints = osConstraints os ++ [c] }

addConstraints :: OpStructure -> [Constraint] -> OpStructure
addConstraints = foldl addConstraint
