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
, OpStructure (..)
, empty
, updateGraph
, addConstraint
, updateConstraints
, resolveAliases
, normalize
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
    = AllocateInRegisterConstraint G.NodeId [Register]
    | ConstantValueConstraint G.NodeId [Range Constant]
    | AliasConstraint [G.NodeId]
    | RegFlagConstraint RegisterFlag [Range Constant]
    deriving (Show,Eq)

isAliasConstraint :: Constraint -> Bool
isAliasConstraint (AliasConstraint _) = True
isAliasConstraint _ = False

fromAliasConstraint :: Constraint -> [G.NodeId]
fromAliasConstraint (AliasConstraint ns) = ns
fromAliasConstraint s = error $ "Cannot be invoked on " ++ (show s)

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

resolveAliases' :: OpStructure -> [G.NodeId] -> OpStructure
resolveAliases' os [] = os
resolveAliases' os (_:[]) = os
resolveAliases' os is =
  let i_to_resolve_to = head is
      is_to_resolve = tail is
  in foldr (resolveAliases'' i_to_resolve_to) os is_to_resolve

resolveAliases'' :: G.NodeId       -- ^ Node ID to resolve to.
                    -> G.NodeId    -- ^ Node ID to replace.
                    -> OpStructure
                    -> OpStructure
resolveAliases'' i_to i_from os =
  let new_g = copyNodeLabels i_to i_from (graph os)
      new_cs = map (updateNodeInConstraint i_to i_from) (constraints os)
  in OpStructure new_g new_cs

copyNodeLabels :: G.NodeId -> G.NodeId -> G.Graph -> G.Graph
copyNodeLabels i_to i_from g =
  let n_to = head $ G.nodesByNodeId i_to g
      ns_to_resolve = G.nodesByNodeId i_from g
  in foldr (G.copyNodeLabel n_to) g ns_to_resolve

updateNodeInConstraint :: G.NodeId -> G.NodeId -> Constraint -> Constraint
updateNodeInConstraint i_to i_from c@(AllocateInRegisterConstraint i regs)
  | i_from == i = AllocateInRegisterConstraint i_to regs
  | otherwise = c
updateNodeInConstraint i_to i_from c@(ConstantValueConstraint i ranges)
  | i_from == i = ConstantValueConstraint i_to ranges
  | otherwise = c
updateNodeInConstraint _ _ c = c

-- | Normalizes an operation structure by merging all nodes which have the same
-- node ID and label, and then merges all adjacent data nodes (the parent is
-- kept).

normalize :: OpStructure -> OpStructure
normalize = mergeAdjacentDataNodes . mergeIdenticalNodes

-- | Merges all nodes that have the same node ID and label to a single node.

mergeIdenticalNodes :: OpStructure -> OpStructure
mergeIdenticalNodes os =
  let nCompare n1 n2 = G.haveSameNodeIds n1 n2 && G.haveSameBBLabels n1 n2
      unique_nodes = nubBy nCompare (G.nodes $ graph os)
      is_and_labels = map (\n -> (G.nodeId n, G.bbLabel n)) unique_nodes
  in updateGraph os
     $ foldr mergeNodesWithSameNodeIdAndLabel (graph os) is_and_labels

mergeNodesWithSameNodeIdAndLabel :: (G.NodeId, G.BBLabel) -> G.Graph -> G.Graph
mergeNodesWithSameNodeIdAndLabel (i, l) g =
  let nCompare n = G.hasSameNodeId i n && G.hasSameBBLabel l n
      same_ns = filter nCompare (G.nodes g)
      new_g = foldr (G.mergeNodes (head same_ns)) g same_ns
  in new_g

-- | Merges data nodes which are adjacent (between two nodes, the parent is
-- kept). Constraints are updated accordingly.

mergeAdjacentDataNodes :: OpStructure -> OpStructure
mergeAdjacentDataNodes os =
  foldl mergeAdjacentDataNodes' os (reverse $ G.topSort (graph os))

mergeAdjacentDataNodes' :: OpStructure -> G.Node -> OpStructure
mergeAdjacentDataNodes' os n
  | isDataNode n = let g = graph os
                       data_cs = filter isDataNode $ G.children n g
                   in foldr (mergeAdjacentDataNodes'' n) os data_cs
  | otherwise = os

mergeAdjacentDataNodes'' :: G.Node -> G.Node -> OpStructure -> OpStructure
mergeAdjacentDataNodes'' n_to n_from os =
  let new_g = G.mergeNodes n_to n_from (graph os)
      new_cs = map (updateNodeInConstraint (G.nodeId n_to) (G.nodeId n_from))
               (constraints os)
  in OpStructure new_g new_cs

isDataNode :: G.Node -> Bool
isDataNode n = G.isDataNodeType $ G.nodeType n