-------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Functions.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Provides a set of transformation functions that can be applied on a given
-- function.
--------------------------------------------------------------------------------

module Language.InstrSel.Functions.Transformations
  ( branchExtend
  , copyExtend
  )
where

import Language.InstrSel.Functions.Base
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Transformations
import Language.InstrSel.OpStructures
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
--
-- TODO: update the def-placement edges when phi nodes are involved!
branchExtend :: Function -> Function
branchExtend =
  assignMissingExecFreqs . assignMissinBasicBlockLabels . extend
  where extend f =
          let g = getGraph f
              new_g = branchExtendWhen (\_ _ -> True) g
              new_f = updateGraph new_g f
          in new_f

-- | Assigns a unique basic block label to every label node that currently has
-- an empty label (which will be the case after branch extension).
assignMissinBasicBlockLabels :: Function -> Function
assignMissinBasicBlockLabels f =
  let g = getGraph f
      nodes = filter isLabelNode (getAllNodes g)
      label_node_pairs = map (\n -> (bbLabel (getNodeType n), n)) nodes
      no_label_nodes = map snd (filter (isBBLabelEmpty . fst) label_node_pairs)
      existing_labels = map fst label_node_pairs
      ok_labels = filter (`notElem` existing_labels)
                         ( map (\i -> BasicBlockLabel $ "bb" ++ show i)
                               ([0..] :: [Integer])
                                -- ^ The type cast is to inhibit a compilation
                                -- warning
                         )
      new_g =
        foldl (\g' (l, n) -> updateNodeType (LabelNode { bbLabel = l }) n g')
              g
              (zip ok_labels no_label_nodes)
  in updateGraph new_g f

-- | Assigns an execution frequency to every basic block that currently does not
-- have one (which will be the case after branch extension). These labels will
-- be set to have the same frequency as its preceding label in the CFG (we know
-- at this point that each new label has exactly one preceding label).
assignMissingExecFreqs :: Function -> Function
assignMissingExecFreqs f =
  let g = getGraph f
      cfg = extractCFG g
      new_freqs =
        map ( \n -> let l = bbLabel $ getNodeType n
                        freqs = functionBBExecFreq f
                        l_freq = lookup l freqs
                    in if isJust l_freq
                       then (l, fromJust l_freq)
                       else let prec_n = getSourceNode
                                           cfg
                                           (head $ getCtrlFlowInEdges cfg n)
                                prec_l = bbLabel $ getNodeType prec_n
                                prec_freq = fromJust $ lookup prec_l freqs
                            in (l, prec_freq)
            )
            (getAllNodes cfg)
  in (updateGraph g f) { functionBBExecFreq = new_freqs }
