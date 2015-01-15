-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Functions.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a set of transformation functions that can be applied on a given
-- function.
--------------------------------------------------------------------------------

module Language.InstSel.Functions.Transformations
  ( branchExtend
  , copyExtend
  )
where

import Language.InstSel.Functions.Base
import Language.InstSel.Functions.IDs
import Language.InstSel.Graphs
import Language.InstSel.Graphs.Transformations
import Language.InstSel.OpStructures
  ( OpStructure (..) )

import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Applies a transformation on the graph in a given function.
getGraph :: Function -> Graph
getGraph = osGraph . functionOS

-- | Replaces the current graph in a function with a new graph.
updateGraph :: Graph -> Function -> Function
updateGraph new_g f =
  let os = functionOS f
      new_os = os { osGraph = new_g }
      new_f = f { functionOS = new_os }
  in new_f

-- | Replaces the current execution frequencies in a function with new ones.
updateExecFreqs :: [(BasicBlockLabel, ExecFreq)] -> Function -> Function
updateExecFreqs new_fs f = f { functionBBExecFreq = new_fs }

-- | Copy-extends the given graph along every eligable data flow edge.
copyExtend :: Function -> Function
copyExtend f =
  let g = getGraph f
      new_g = copyExtendWhen (\_ _ -> True) g
      new_f = updateGraph new_g f
  in new_f

-- | Inserts a new label node and jump control node along each outbound control
-- edge from every conditional jump control node. This is used to handle special
-- cases where a conditional jump control node cannot be covered by patterns
-- that performs a fall-through to one of the destination labels. These
-- situations often occur in loops where it is not possible to do a fall-through
-- out of the loop (and, naturally, it is never possible to do a fall-through to
-- the head of the loop).
--
-- After branch extension, there will be labels which has no execution
-- frequency. These will be set to have the same frequency as its preceding
-- label in the CFG (at this point we know for sure that each new label has only
-- one preceding label).
branchExtend :: Function -> Function
branchExtend f =
  let g = getGraph f
      new_g = branchExtendWhen (\_ _ -> True) g
      cfg = extractCFG new_g
      new_freqs =
        map
          ( \n ->
              let l = bbLabel $ getNodeType n
                  freqs = functionBBExecFreq f
                  l_freq = lookup l freqs
              in if isJust l_freq
                 then (l, fromJust l_freq)
                 else let prec_n = getSourceNode
                                     cfg
                                     (head $ getCFInEdges cfg n)
                          prec_l = bbLabel $ getNodeType prec_n
                          prec_freq = fromJust $ lookup prec_l freqs
                      in (l, prec_freq)
          )
          (getAllNodes cfg)
  in updateExecFreqs new_freqs (updateGraph new_g f)
