--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.OperationStructures.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
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

module Language.InstructionSelection.OperationStructures.Base where

import qualified Language.InstructionSelection.Graphs as G
import Language.InstructionSelection.Utils (Range (..))


data Register
    = Register String
    deriving (Show,Eq)

data RegisterFlag
    = RegisterFlag String Register
    deriving (Show,Eq)

data Constant
    = IntConstant Integer
    deriving (Show,Eq)

data Constraint
    = Constraint
    | AllocateInRegisterConstraint G.NodeId [Register]
    | ConstantValueConstraint G.NodeId [Range Constant]
    | AliasConstraint [G.NodeId]
    | RegFlagConstraint RegisterFlag [Range Constant]
    deriving (Show,Eq)

isAliasConstraint (AliasConstraint _) = True
isAliasConstraint _ = False

data OpStructure
    = OpStructure { graph :: G.Graph
                  , constraints :: [Constraint]
                  } deriving (Show)

empty :: OpStructure
empty = OpStructure G.empty []

updateGraph :: OpStructure -> G.Graph -> OpStructure
updateGraph (OpStructure _ cs) g = OpStructure g cs

addConstraint :: OpStructure -> Constraint -> OpStructure
addConstraint (OpStructure g cs) c = OpStructure g (cs ++ [c])
