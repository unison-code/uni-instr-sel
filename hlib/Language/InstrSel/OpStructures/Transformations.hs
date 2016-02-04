--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.OpStructures.Transformations
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Takes an 'OpStructure' and performs various transformations on it.
--
--------------------------------------------------------------------------------

module Language.InstrSel.OpStructures.Transformations
  ( canonicalizeCopies )
where

import Language.InstrSel.Graphs
import Language.InstrSel.Graphs.PatternMatching.VF2
import Language.InstrSel.DataTypes
import Language.InstrSel.OpTypes
import Language.InstrSel.OpStructures.Base
import Language.InstrSel.Utils
  ( rangeFromSingleton )



-------------
-- Functions
-------------

-- | Finds computations that are synonymous with a copy operation, and replaces
-- such computation nodes with 'CopyNode's.
canonicalizeCopies :: OpStructure -> OpStructure
canonicalizeCopies os =
  let mkTempValueNode =
        ValueNode { typeOfValue = AnyType
                  , originOfValue = Nothing
                  }
      mkIntConstValueNode c_val =
        ValueNode { typeOfValue = IntConstType
                                    { intConstValue = rangeFromSingleton c_val
                                    , intConstNumBits = Nothing
                                    }
                  , originOfValue = Nothing
                  }
      mkCompNode op = ComputationNode { compOp = CompArithOp op }
      mkPat op c_val swap_ops =
        mkGraph
          ( map Node
                [ ( 0, NodeLabel 0 (mkCompNode op) )
                , ( 1, NodeLabel 1 mkTempValueNode )
                , ( 2, NodeLabel 2 (mkIntConstValueNode c_val) )
                , ( 3, NodeLabel 3 mkTempValueNode )
                ]
          )
          ( map Edge
                ( [ ( 0, 3, EdgeLabel DataFlowEdge 0 0 ) ]
                  ++
                  if not swap_ops
                  then [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                       , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                       ]
                  else [ ( 1, 0, EdgeLabel DataFlowEdge 0 1 )
                       , ( 2, 0, EdgeLabel DataFlowEdge 0 0 )
                       ]
                )
          )
      cp_patterns = concatMap (\(op, c) -> [mkPat op c False, mkPat op c True])
                              [ (IntOp Add,  0)
                              , (IntOp Mul,  1)
                              , (IntOp  Or,  0)
                              , (IntOp And, -1)
                              ]
      fg = osGraph os
      matches = concatMap (findMatches fg) cp_patterns
      process m os =
        let g0 = osGraph os
            f_ns = map fNode $ fromMatch m
            comp_n = head $ filter isComputationNode f_ns
            const_n = head $ filter isValueNodeWithConstValue f_ns
            -- There is only one value node with constant value in each pattern,
            -- so we expected there to be exactly one such node in the
            -- match. Same goes for the computation node
            g1 = foldr delEdge g0 $ getEdgesBetween g0 const_n comp_n
            g2 = updateNodeType CopyNode comp_n g1
            dt_es = getDtFlowOutEdges g2 const_n
            g3 = if length dt_es == 0 then delNode const_n g2 else g2
        in os { osGraph = g3 }
  in foldr process os matches
