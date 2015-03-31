-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a set of transformation functions which can be applied on a given
-- target machine.
--
--------------------------------------------------------------------------------

module Language.InstrSel.TargetMachines.Transformations
  ( copyExtend )
where

import Language.InstrSel.TargetMachines.Base
import Language.InstrSel.Constraints.ConstraintBuilder
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Transformations
import Language.InstrSel.OpStructures
  ( OpStructure (..) )



-------------
-- Functions
-------------

-- | Copy-extends the given graph along every eligable data flow edge, except
-- those edges where the data node has no definition (that is, no parents)
-- unless the data node represents a constant value.
copyExtendGraph :: Graph -> Graph
copyExtendGraph =
  copyExtendWhen
    ( \g e ->
      let src = getSourceNode g e
      in length (getDtFlowInEdges g src) > 0 || isDataNodeWithConstValue src
    )
    (\_ -> AnyType)

-- | Copy-extends every instruction in the given target machine.
copyExtend :: TargetMachine -> TargetMachine
copyExtend tm =
  let copyExtendOS os =
        let old_g = osGraph os
            old_data_ns = filter isDataNode $ getAllNodes old_g
            new_g = copyExtendGraph old_g
            new_data_ns = filter (\n -> isDataNode n && n `notElem` old_data_ns)
                                 (getAllNodes old_g)
            old_cs = osConstraints os
            new_cs = old_cs ++ concatMap mkNoReuseConstraints
                                         (map getNodeID new_data_ns)
        in os { osGraph = new_g, osConstraints = new_cs }
      copyExtendPat p = p { patOS = copyExtendOS $ patOS p }
      copyExtendInstr i = i { instrPatterns =
                                map copyExtendPat (instrPatterns i)
                            }
      new_instrs = map copyExtendInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }
