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
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Transformations
import Language.InstrSel.OpStructures
  ( OpStructure (..) )



-------------
-- Functions
-------------

-- | Copy-extends the given graph along every eligable data flow edge, except
-- those edges where the data node has no definition (that is, no parents). The
-- data nodes of such edges represent the external input to the pattern, and
-- should therefore not be copy-extended.
copyExtendGraph :: Graph -> Graph
copyExtendGraph =
  copyExtendWhen (\g e -> length (getDtFlowInEdges g (getSourceNode g e)) > 0)

-- | Copy-extends every instruction in the given target machine.
copyExtend :: TargetMachine -> TargetMachine
copyExtend tm =
  let copyExtendOS os = os { osGraph = copyExtendGraph $ osGraph os }
      copyExtendPat p = p { patOS = copyExtendOS $ patOS p }
      copyExtendInstr i = i { instrPatterns =
                                map copyExtendPat (instrPatterns i)
                            }
      new_instrs = map copyExtendInstr (tmInstructions tm)
  in tm { tmInstructions = new_instrs }
