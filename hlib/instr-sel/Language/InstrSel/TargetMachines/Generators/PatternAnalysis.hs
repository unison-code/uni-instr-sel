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
  )
where

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
