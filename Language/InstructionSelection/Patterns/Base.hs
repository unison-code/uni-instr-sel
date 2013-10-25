--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Patterns.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data types and records for representing patterns. This is the
-- format on which subsequent preparation for instruction selection will build
-- on (i.e. other pattern forms, such as those based on LLVM, will be converted
-- into this format).
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.Patterns.Base where

import Language.InstructionSelection.Graphs
import Language.InstructionSelection.OperationStructures
import Language.InstructionSelection.Patterns.AssemblyString
import Language.InstructionSelection.Utils (Range (..))



data Instruction
    = Instruction

          -- | Assembly string to produce upon code emission.

          AssemblyString

          -- | Patterns which correspond to the instruction. There must be at
          -- least one pattern.

          [OpStructure]

    deriving (Show)



--resolveAliases (OS.Pattern g cons) =
--  let alias_cons = filter OS.isAliasConstraint cons
--      all_cons_but_alias = filter (not . OS.isAliasConstraint) cons
--      (new_g, new_cons) = foldl resolveAliases' (g, all_cons_but_alias)
--                                alias_cons
--  in OS.Pattern new_g new_cons
--
--resolveAliases' (OS.AliasConstraint []) t = t
--resolveAliases' (OS.AliasConstraint (_:[])) t = t
--resolveAliases' (OS.AliasConstraint nodes) t =
--  let node_to_replace_with = head nodes
--      nodes_to_replace = tail nodes
--      combinations = zip (repeat node_to_replace_with) nodes_to_replace
--  in foldl resolveAliases'' t combinations
--resolveAliases'' (n1, n2) (g, cons) =
--  let new_g = copyNodeLabel n1 n2 g
--      new_cons = map (updateNodeInConstraint n1 n2) cons
--  in (new_g, new_cons)
--
--updateNodeInConstraint n1 n2 con@(OS.AllocateInRegisterConstraint id regs)
--  | n2 == id = OS.AllocateInRegisterConstraint n1 regs
--  | otherwise = con
--updateNodeInConstraint n1 n2 con@(OS.ConstantValueConstraint id ranges)
--  | n2 == id = OS.ConstantValueConstraint n1 ranges
--  | otherwise = con
--updateNodeInConstraint _ _ con = con
--
--mergeIdenticalNodes :: OS.Pattern -> OS.Pattern
--mergeIdenticalNodes (OS.Pattern g cons) =
--  let isSame n1 n2 = haveSameNodeIds n1 n2 && haveSameLabels n1 n2
--      unique_nodes = nubBy isSame (nodes g)
--  in (OS.Pattern (foldl (mergeNodes isSame) g unique_nodes) cons)
--
--mergeAdjacentDataNodes :: OS.Pattern -> OS.Pattern
--mergeAdjacentDataNodes g = g
---- TODO: implement
