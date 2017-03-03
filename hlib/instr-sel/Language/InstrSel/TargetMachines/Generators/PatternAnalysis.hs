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
  ( arePatternsCopy
  , arePatternsNull
  , arePatternsPhi
  , arePatternsSimd
  , arePatternsUncondBranch
  , arePatternsCondBranch
  , arePatternsCondBranchWithFallthrough
  )
where

import Language.InstrSel.Constraints
  ( isFallThroughConstraint )
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Graphalyze
import Language.InstrSel.OpStructures
import Language.InstrSel.TargetMachines



-------------
-- Functions
-------------

-- ^ Checks whether all patterns contain at least operation node and does not
-- emit anything during code emission.
arePatternsNull :: [InstrPattern] -> Bool
arePatternsNull pats =
  let isNullPattern p =
        let os = patOS p
            g = osGraph os
            op_nodes = filter isOperationNode $ getAllNodes g
            emit_str = patEmitString p
        in length op_nodes > 0 && length (emitStrParts emit_str) == 0
  in all isNullPattern pats

-- ^ Checks whether all patterns contain exactly one copy node.
arePatternsCopy :: [InstrPattern] -> Bool
arePatternsCopy pats =
  let isCopyPattern p =
        let os = patOS p
            g = osGraph os
            op_nodes = filter isOperationNode $ getAllNodes g
        in length op_nodes == 1 && isCopyNode (head op_nodes)
  in all isCopyPattern pats

-- ^ Checks whether all patterns contain exactly one phi node.
arePatternsPhi :: [InstrPattern] -> Bool
arePatternsPhi pats =
  let isPhiPattern p =
        let os = patOS p
            g = osGraph os
            op_nodes = filter isOperationNode $ getAllNodes g
        in length op_nodes == 1 && isPhiNode (head op_nodes)
  in all isPhiPattern pats

-- ^ Checks whether all pattern graphs consist of disjoint subgraphs which are
-- isomorphic to each other.
arePatternsSimd :: [InstrPattern] -> Bool
arePatternsSimd pats =
  let isSimdPattern p =
        let os = patOS p
            g = osGraph os
            cs = weakComponentsOf g
        in length cs > 1 && all (areGraphsIsomorphic (head cs)) (tail cs)
  in all isSimdPattern pats

-- ^ Checks whether all patterns contain exactly one unconditional branch.
arePatternsUncondBranch :: [InstrPattern] -> Bool
arePatternsUncondBranch pats =
  let isUncondBranchPattern p =
        let os = patOS p
            g = osGraph os
            op_nodes = filter isOperationNode $ getAllNodes g
        in length op_nodes == 1 && isBrControlNode (head op_nodes)
  in all isUncondBranchPattern pats

-- ^ Checks whether all patterns contain exactly one conditional branch.
arePatternsCondBranch :: [InstrPattern] -> Bool
arePatternsCondBranch pats =
  let isCondBranchPattern p =
        let os = patOS p
            g = osGraph os
            pointsAtExits n =
              let bs = getSuccessors g n
              in all (\b -> null $ getCtrlFlowOutEdges g b) bs
            ctrl_nodes = filter isCondBrControlNode $ getAllNodes g
        in length ctrl_nodes > 0 && any pointsAtExits ctrl_nodes
  in all isCondBranchPattern pats

-- ^ Checks whether all patterns contain exactly one conditional branch with
-- fall-through constraint.
arePatternsCondBranchWithFallthrough :: [InstrPattern] -> Bool
arePatternsCondBranchWithFallthrough pats =
  let hasFallthrough p =
        let os = patOS p
            cs = osConstraints os
        in length (filter isFallThroughConstraint cs) > 0
  in arePatternsCondBranch pats &&
     all hasFallthrough pats
