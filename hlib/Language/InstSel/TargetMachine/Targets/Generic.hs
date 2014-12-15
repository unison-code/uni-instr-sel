--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstSel.TargetMachine.Targets.Generic
-- Copyright   :  (c) Gabriel Hjort Blindell 2013-2014
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for creating generic instructions.
--
--------------------------------------------------------------------------------

module Language.InstSel.TargetMachine.Targets.Generic
  ( mkGenericPhiInstructions )
where

import Language.InstSel.Constraints
import Language.InstSel.DataTypes
  ( DataType (..) )
import Language.InstSel.Graphs
import qualified Language.InstSel.OpStructures as OS
import Language.InstSel.TargetMachine.Base



-------------
-- Functions
-------------

-- | Creates a set of instructions for handling the generic cases where
-- 'PhiNode's appear.
mkGenericPhiInstructions :: [Instruction]
mkGenericPhiInstructions =
  let mkDataNode = DataNode { dataType = AnyType, dataOrigin = Nothing }
      g = mkGraph
            ( map
                Node
                [ ( 0, NodeLabel 0 PhiNode )
                , ( 1, NodeLabel 1 mkDataNode )
                , ( 2, NodeLabel 2 mkDataNode )
                , ( 3, NodeLabel 3 mkDataNode )
                ]
            )
            ( map
                Edge
                [ ( 1, 0, EdgeLabel DataFlowEdge 0 0 )
                , ( 2, 0, EdgeLabel DataFlowEdge 0 1 )
                , ( 0, 3, EdgeLabel DataFlowEdge 0 0 )
                ]
            )
      cs = [ BoolExprConstraint $
               AndExpr
                 ( EqExpr
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 1
                     )
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 2
                     )
                 )
                 ( EqExpr
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 2
                     )
                     ( Register2NumExpr $
                         RegisterAllocatedToDataNodeExpr $
                           ANodeIDExpr 3
                     )
                 )
           ]
      pat = InstPattern
              { patID = 0
              , patOS = OS.OpStructure g cs
              , patOutputDataNodes = [3]
              , patADDUC = False
              , patAssIDMaps = [3, 1, 2]
              }
  in [ Instruction
         { instID = 0
         , instPatterns = [pat]
         , instProps = ( InstProperties { instCodeSize = 0, instLatency = 0 } )
         , instAssemblyStr = ( AssemblyString
                                 [ AssemblyVerbatim "phi "
                                 , AssemblyRegisterOf 0
                                 , AssemblyVerbatim " ("
                                 , AssemblyRegisterOf 1
                                 , AssemblyVerbatim ", "
                                 , AssemblyBBLabelOf 1
                                 , AssemblyVerbatim ") ("
                                 , AssemblyRegisterOf 2
                                 , AssemblyVerbatim ", "
                                 , AssemblyBBLabelOf 2
                                 , AssemblyVerbatim ")"
                                 ]
                             )
         }
     ]
