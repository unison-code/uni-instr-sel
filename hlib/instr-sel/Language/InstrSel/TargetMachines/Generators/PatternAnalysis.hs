{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.TargetMachines.Generators.PatternAnalysis
  ( isInstructionCopy
  , isInstructionPhi
  , isInstructionSimd
  , isInstructionUncondBranch
  , isInstructionCondBranch
  , isInstructionCondBranchWithFallthrough
  )
where

import Language.InstrSel.Constraints
  ( isFallThroughConstraint )
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Graphalyze
import Language.InstrSel.OpStructures
import Language.InstrSel.TargetMachines
  hiding
  ( isInstructionCopy
  , isInstructionNull
  , isInstructionPhi
  , isInstructionSimd
  )



-------------
-- Functions
-------------

-- | Checks whether the instruction's pattern graph contains exactly one copy
-- node.
isInstructionCopy :: Instruction -> Bool
isInstructionCopy i =
  let os = instrOS i
      g = osGraph os
      op_nodes = filter isOperationNode $ getAllNodes g
  in length op_nodes == 1 && isCopyNode (head op_nodes)

-- | Checks whether the instruction's pattern graph contains exactly one phi
-- node.
isInstructionPhi :: Instruction -> Bool
isInstructionPhi i =
  let os = instrOS i
      g = osGraph os
      op_nodes = filter isOperationNode $ getAllNodes g
  in length op_nodes == 1 && isPhiNode (head op_nodes)

-- | Checks whether the instruction's pattern graph consists of disjoint
-- subgraphs which are isomorphic to each other.
isInstructionSimd :: Instruction -> Bool
isInstructionSimd i =
  let os = instrOS i
      g = osGraph os
      cs = weakComponentsOf g
  in length cs > 1 && all (areGraphsIsomorphic (head cs)) (tail cs)

-- | Checks whether the instruction's pattern graph contains exactly one
-- unconditional branch.
isInstructionUncondBranch :: Instruction -> Bool
isInstructionUncondBranch i =
  let os = instrOS i
      g = osGraph os
      op_nodes = filter isOperationNode $ getAllNodes g
  in length op_nodes == 1 && isBrControlNode (head op_nodes)

-- | Checks whether the instruction's pattern graph contains any control
-- operations, and if so, these must consist of exactly one conditional branch
-- with at least one exit block.
isInstructionCondBranch :: Instruction -> Bool
isInstructionCondBranch i =
  let os = instrOS i
      g = osGraph os
      pointsAtExits n =
        let bs = getSuccessors g n
        in all (\b -> null $ getCtrlFlowOutEdges g b) bs
      ctrl_nodes = filter isCondBrControlNode $ getAllNodes g
  in length ctrl_nodes == 1 && any pointsAtExits ctrl_nodes

-- | Checks whether the instruction's pattern graph contains exactly one
-- conditional branch with fall-through constraint.
isInstructionCondBranchWithFallthrough :: Instruction -> Bool
isInstructionCondBranchWithFallthrough i =
  let os = instrOS i
      cs = osConstraints os
  in isInstructionCondBranch i && length (filter isFallThroughConstraint cs) > 0
