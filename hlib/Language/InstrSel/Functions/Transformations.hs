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
  , combineConstants
  )
where

import Language.InstrSel.Functions.Base
import Language.InstrSel.Functions.IDs
import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintReconstructor
import Language.InstrSel.DataTypes
import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.Transformations
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.Utils.Range

import Data.Maybe
  ( fromJust
  , isJust
  )
import Data.List
  ( partition )



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
      mkNewDataType d@(IntTempType {}) = d
      mkNewDataType (IntConstType { intConstNumBits = b }) =
        if isJust b
        then IntTempType { intTempNumBits = fromJust b }
        else error $ "copyExtend: 'IntConstType' cannot have 'Nothing' as "
                     ++ "'intConstNumBits'"
      -- TODO: restore
      mkNewDataType d = d
      --mkNewDataType d = error $ "copyExtend: DataType '" ++ show d ++ "' not "
      --                          ++ "supported"
      new_g = copyExtendWhen (\_ _ -> True) mkNewDataType g
      new_f = updateGraph new_g f
  in new_f

-- | Inserts a new block node and jump control node along each outbound control
-- edge from every conditional jump control node. This is used to handle special
-- cases where a conditional jump control node cannot be covered by patterns
-- that performs a fall-through to one of the destination blocks. These
-- situations often occur in loops where it is not possible to do a fall-through
-- out of the loop (and, naturally, it is never possible to do a fall-through to
-- the head of the loop).
--
-- After branch extension, there will be blocks which has no execution
-- frequency. These will be set to have the same frequency as its preceding
-- block in the CFG (at this point we know for sure that each new block has only
-- one preceding block).
branchExtend :: Function -> Function
branchExtend =
  assignMissingBlockExecFreqs . assignMissingBlockNames . extend
  where extend f =
          let g = getGraph f
              new_g = branchExtendWhen (\_ _ -> True) g
              new_f = updateGraph new_g f
          in new_f

-- | Assigns a unique block name to every block node that currently has an empty
-- block name (which will be the case after branch extension).
assignMissingBlockNames :: Function -> Function
assignMissingBlockNames f =
  let g = getGraph f
      nodes = filter isBlockNode (getAllNodes g)
      block_node_pairs = map (\n -> (nameOfBlock (getNodeType n), n)) nodes
      no_block_nodes =
        map snd (filter (isBlockNameEmpty . fst) block_node_pairs)
      existing_names = map fst block_node_pairs
      ok_names = filter (`notElem` existing_names)
                         ( map (\i -> BlockName $ "bb" ++ show i)
                               ([0..] :: [Integer])
                                -- ^ The type cast is to inhibit a compilation
                                -- warning
                         )
      new_g =
        foldl ( \g' (l, n) ->
                updateNodeType (BlockNode { nameOfBlock = l }) n g'
              )
              g
              (zip ok_names no_block_nodes)
  in updateGraph new_g f

-- | Assigns an execution frequency to every block that currently does not have
-- one (which will be the case after branch extension). These blocks will be set
-- to have the same frequency as its preceding block in the CFG (we know at this
-- point that each new block has exactly one preceding block).
assignMissingBlockExecFreqs :: Function -> Function
assignMissingBlockExecFreqs f =
  let g = getGraph f
      cfg = extractCFG g
      new_freqs =
        map ( \n -> let l = nameOfBlock $ getNodeType n
                        freqs = functionBBExecFreq f
                        l_freq = lookup l freqs
                    in if isJust l_freq
                       then (l, fromJust l_freq)
                       else let prec_n = getSourceNode
                                           cfg
                                           (head $ getCtrlFlowInEdges cfg n)
                                prec_l = nameOfBlock $ getNodeType prec_n
                                prec_freq = fromJust $ lookup prec_l freqs
                            in (l, prec_freq)
            )
            (getAllNodes cfg)
  in (updateGraph g f) { functionBBExecFreq = new_freqs }

-- | Combines value nodes in the function graph that represent the same
-- constant.
combineConstants :: Function -> Function
combineConstants f =
  let g = getGraph f
      const_ns = filter isValueNodeWithConstValue (getAllNodes g)
      isSameConstant (IntConstType { intConstValue = r1 })
                     (IntConstType { intConstValue = r2 }) =
        isRangeSingleton r1 && isRangeSingleton r2 && r1 == r2
      isSameConstant _ _ = False
      haveSameConstants n1 n2 =
        (getDataTypeOfValueNode n1) `isSameConstant` (getDataTypeOfValueNode n2)
      partitionNodes [] = []
      partitionNodes [n] = [[n]]
      partitionNodes (n:ns) =
        let (eq_n, neq_n) = partition (haveSameConstants n) ns
        in [n:eq_n] ++ partitionNodes neq_n
      partitioned_ns = partitionNodes const_ns
  in foldl combineValueNodes f partitioned_ns

combineValueNodes :: Function -> [Node] -> Function
combineValueNodes f [_] = f
combineValueNodes f ns =
  let mkNewDataType IntConstType { intConstValue = r } =
        IntConstType { intConstValue = r, intConstNumBits = Nothing }
      mkNewDataType d = error $ "combineValueNodes: unsupported data type "
                                ++ show d
      g0 = getGraph f
      nt = ValueNode (mkNewDataType $ getDataTypeOfValueNode $ head ns) Nothing
      (g1, new_n) = addNewNode nt g0
      entry_node = head
                   $ findNodesWithNodeID g1
                   $ fromJust
                   $ osEntryBlockNode
                   $ functionOS f
      (g2, _) = addNewEdge DataFlowEdge (entry_node, new_n) g1
  in foldr (replaceValueNode new_n) (updateGraph g2 f) ns

-- | Replaces a value node in the function graph with another value node, but
-- all inbound edges to the value node to be removed will be ignored (that is,
-- these edges will disappear).
replaceValueNode
  :: Node
     -- ^ The new node.
  -> Node
     -- ^ The old node.
  -> Function
  -> Function
replaceValueNode new_n old_n f =
  let old_os = functionOS f
      old_g = osGraph old_os
      new_g = delNode old_n $ redirectOutEdges new_n old_n old_g
      def_recon = mkDefaultReconstructor
      mkNodeExpr _ expr@(ANodeIDExpr n) =
        if n == getNodeID old_n
        then ANodeIDExpr $ getNodeID new_n
        else expr
      mkNodeExpr r expr = (mkNodeExprF def_recon) r expr
      recon = mkDefaultReconstructor { mkNodeExprF = mkNodeExpr }
      new_cs = map (apply recon) (osConstraints old_os)
      new_os = old_os { osGraph = new_g, osConstraints = new_cs }
  in f { functionOS = new_os }
