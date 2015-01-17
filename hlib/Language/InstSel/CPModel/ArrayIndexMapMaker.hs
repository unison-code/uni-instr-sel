--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.CPModel.ArrayIndexMapMaker
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Constructs the array index mapping information which is necessary both to
-- simplify the input to the CP solver and to interpret the subsequent solution
-- of the CP model.
--
--------------------------------------------------------------------------------

module Language.InstSel.CPModel.ArrayIndexMapMaker
  ( mkArrayIndexMapInfo )
where

import Language.InstSel.CPModel.Base
import Language.InstSel.Graphs
import Language.InstSel.Functions
  ( Function (..) )
import Language.InstSel.OpStructures
  ( OpStructure (..) )
import Language.InstSel.TargetMachines.PatternMatching
  ( MatchData (..)
  , MatchsetInfo (..)
  )



-------------
-- Functions
-------------

mkArrayIndexMapInfo :: Function -> MatchsetInfo -> ArrayIndexMapInfo
mkArrayIndexMapInfo function matchset =
  let match_ids = map mdMatchID (msiMatches matchset)
      g = osGraph $ functionOS function
      nodes = getAllNodes g
      l_nodes = filter isLabelNode nodes
      d_nodes = filter isDataNode nodes
  in ArrayIndexMapInfo
       { ai2MatchIDs = match_ids
       , ai2LabelNodeIDs = map getNodeID l_nodes
       , ai2DataNodeIDs = map getNodeID d_nodes
       }
