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
  Constant (..)
, Constraint (..)
, OpStructure (..)
, Register (..)
, RegisterFlag (..)
, addConstraint
, isAliasConstraint
, mkEmpty
, normalizeNodeIds
, resolveAliases
, updateConstraints
, updateGraph
, updateNodeIdInConstraint
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
    | AssignSameRegisterConstraint G.NodeId G.NodeId
    deriving (Show,Eq)

isAliasConstraint :: Constraint -> Bool
isAliasConstraint (AliasConstraint _) = True
isAliasConstraint _ = False

getNodesFromAliasConstraint :: Constraint -> [G.NodeId]
getNodesFromAliasConstraint (AliasConstraint ns) = ns
getNodesFromAliasConstraint s = error $ "Cannot be invoked on " ++ (show s)

data OpStructure
    = OpStructure {
          graph :: G.Graph
          , constraints :: [Constraint]
      }
    deriving (Show)

-- | Creates an empty operation structure.

mkEmpty :: OpStructure
mkEmpty = OpStructure G.empty []

updateGraph :: OpStructure -> G.Graph -> OpStructure
updateGraph (OpStructure _ cs) g = OpStructure g cs

addConstraint :: OpStructure -> Constraint -> OpStructure
addConstraint (OpStructure g cs) c = OpStructure g (cs ++ [c])

updateConstraints :: OpStructure -> [Constraint] -> OpStructure
updateConstraints (OpStructure g _) cs = OpStructure g cs

-- | Merges all aliased nodes, updates the affected constraints, and removes all
-- alias constraints.

resolveAliases :: OpStructure -> OpStructure
resolveAliases (OpStructure g cs) =
  let alias_cs = filter isAliasConstraint cs
      all_cs_but_alias = filter (not . isAliasConstraint) cs
      aliases = map getNodesFromAliasConstraint alias_cs
  in foldl resolveAliases' (OpStructure g all_cs_but_alias) aliases

-- | Resolves an alias by merging all nodes in the tail of list with the first
-- ID of the list. Any loops caused by this will be removed.

resolveAliases' :: OpStructure -> [G.NodeId] -> OpStructure
resolveAliases' os [] = os
resolveAliases' os (_:[]) = os
resolveAliases' os ids = foldr (resolveAliases'' (head ids)) os (tail ids)

resolveAliases'' :: G.NodeId       -- ^ Node ID to resolve to.
                    -> G.NodeId    -- ^ Node ID to replace.
                    -> OpStructure
                    -> OpStructure
resolveAliases'' to_id from_id os =
  let g = graph os
      from_n = head $ G.fromNodeId g from_id
      to_n = head $ G.fromNodeId g to_id
      new_g = foldl (flip G.delEdge) (G.mergeNodes to_n from_n g)
              (G.edges g to_n to_n)
      new_cs = map (updateNodeIdInConstraint to_id from_id) (constraints os)
  in OpStructure new_g new_cs

-- | Updates a node ID appearing within a constraint.

updateNodeIdInConstraint :: G.NodeId    -- ^ New node Id.
                            -> G.NodeId -- ^ Node Id to change.
                            -> Constraint
                            -> Constraint
updateNodeIdInConstraint to_id from_id c@(AllocateInRegisterConstraint id regs)
  | from_id == id = AllocateInRegisterConstraint to_id regs
  | otherwise = c
updateNodeIdInConstraint to_id from_id c@(ConstantValueConstraint id ranges)
  | from_id == id = ConstantValueConstraint to_id ranges
  | otherwise = c
updateNodeIdInConstraint to_id from_id (AliasConstraint ids) =
  AliasConstraint (map (\id -> if id == from_id then to_id else id) ids)
updateNodeIdInConstraint _ _ c = c

-- | Normalizes the node IDs such that the use of IDs is contingent, starting
-- from 0.

normalizeNodeIds :: OpStructure -> OpStructure
normalizeNodeIds o =
  -- TODO: implement
  o
