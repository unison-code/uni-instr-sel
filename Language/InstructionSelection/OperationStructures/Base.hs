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

module Language.InstructionSelection.OperationStructures.Base (
  Register (..)
, RegisterFlag (..)
, Constant (..)
, Constraint (..)
, isAliasConstraint
, fromAliasConstraint
, OpStructure (..)
, empty
, updateGraph
, addConstraint
, updateConstraints
, resolveAliases
) where

import qualified Language.InstructionSelection.Graphs as G
import Language.InstructionSelection.Utils (Range (..))
import Data.List


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

fromAliasConstraint (AliasConstraint ns) = ns

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

updateConstraints :: OpStructure -> [Constraint] -> OpStructure
updateConstraints (OpStructure g _) cs = OpStructure g cs

-- | Resolves and removes all alias constraints.

resolveAliases :: OpStructure -> OpStructure
resolveAliases (OpStructure g cs) =
  let alias_cs = filter isAliasConstraint cs
      all_cs_but_alias = filter (not . isAliasConstraint) cs
      aliases = map fromAliasConstraint alias_cs
  in foldl resolveAliases' (OpStructure g all_cs_but_alias) aliases
resolveAliases' os [] = os
resolveAliases' os (_:[]) = os
resolveAliases' os ns =
  let node_to_replace_with = head ns
      nodes_to_replace = tail ns
      combinations = zip (repeat node_to_replace_with) nodes_to_replace
  in foldl resolveAliases'' os combinations
resolveAliases'' os (n1, n2) =
  let g = graph os
      new_g = G.copyNodeLabel n1 n2 g
      cs = constraints os
      new_cs = map (updateNodeInConstraint n1 n2) cs
  in updateConstraints (updateGraph os new_g) new_cs

updateNodeInConstraint n1 n2 con@(AllocateInRegisterConstraint id regs)
  | n2 == id = AllocateInRegisterConstraint n1 regs
  | otherwise = con
updateNodeInConstraint n1 n2 con@(ConstantValueConstraint id ranges)
  | n2 == id = ConstantValueConstraint n1 ranges
  | otherwise = con
updateNodeInConstraint _ _ con = con

-- | Merges all nodes that have the same node ID and label to a single node.

mergeIdenticalNodes :: OpStructure -> OpStructure
mergeIdenticalNodes os =
  let unique_nodes = nubBy G.haveSameNodeIdsAndLabels (G.nodes $ graph os)
  in updateGraph os
     $ foldr (G.mergeNodes G.haveSameNodeIdsAndLabels) (graph os) unique_nodes

-- | Merges data nodes which are adjacent (between two nodes, the parent is
-- kept).

mergeAdjacentDataNodes :: OpStructure -> OpStructure
mergeAdjacentDataNodes g = g
-- TODO: implement
