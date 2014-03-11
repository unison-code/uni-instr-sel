--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data structures representing the data for the CP model.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.Base where

import Language.InstructionSelection.Graphs ( MatchsetId
                                            , NodeId
                                            , NodeIdMatchset
                                            )
import Language.InstructionSelection.OpStructures (Constraint)
import Language.InstructionSelection.Patterns (PatternId)
import Language.InstructionSelection.PrettyPrint



data CPModelParams
    = CPModelParams FunctionGraphData [PatternGraphData] MachineData
    deriving (Show)

-- | Describes the necessary function graph data.

data FunctionGraphData
    = FunctionGraphData {

          -- | The nodes in the function graph.

          funcNodes :: NodePartition

          -- | The dominator set for the basic blocks, which are represented by
          -- the label nodes.

        , funcLabelDoms :: [( NodeId   -- ^ The dominated node.
                            , [NodeId] -- ^ The dominator set.
                            )]

          -- | The function constraints, if any.

        , funcConstraints :: [Constraint]

      }
    deriving (Show)

-- | Describes the necessary pattern graph data.

data PatternGraphData
    = PatternGraphData {

          -- | The pattern ID.

          patId :: PatternId

          -- | Instruction code size (in bytes).

        , patCodeSize :: Integer

          -- | Instruction latency (in cycles).

        , patLatency :: Integer

          -- | The nodes in the pattern graph.

        , patNodes :: NodePartition

          -- | The 'use' and 'def' nodes of type Data.

        , patDataUseDefs :: UseDefNodes

          -- | The 'use' and 'def' nodes of type Label.

        , patLabelUseDefs :: UseDefNodes

          -- | The 'use' and 'def' nodes of type State.

        , patStateUseDefs :: UseDefNodes

          -- | The pattern constraints, if any.

        , patConstraints :: [Constraint]

          -- | Matches found for this pattern.

        , patMatchsets :: [(NodeIdMatchset, MatchsetId)]

      }
    deriving (Show)

-- | Contains all node IDs within a graph, partitioned after node type.

data NodePartition
    = NodePartition {

          -- | Unique IDs of the computation nodes.

          computationNodes :: [NodeId]

          -- | Unique IDs of the control nodes.

        , controlNodes :: [NodeId]

          -- | Unique IDs of the data nodes.

        , dataNodes :: [NodeId]

          -- | Unique IDs of the label nodes.

        , labelNodes :: [NodeId]

          -- | Unique IDs of the phi nodes.

        , phiNodes :: [NodeId]

          -- | Unique IDs of the state nodes.

        , stateNodes :: [NodeId]

          -- | Unique IDs of the transfer nodes.

        , transferNodes :: [NodeId]

      }
    deriving (Show)

-- | Contains the 'use' and 'def' nodes of a particular node type.

data UseDefNodes
    = UseDefNodes {

          -- | Unique IDs of the 'use' nodes.

          useNodes :: [NodeId]

          -- | Unique IDs of the 'def' nodes.

        , defNodes :: [NodeId]

      }
    deriving (Show)

-- | Describes the necessary target machine data.

data MachineData
    = MachineData {

          -- TODO: implement

      }
    deriving (Show)



------------------------
-- Type class instances
------------------------

instance PrettyPrint CPModelParams where
  prettyShow (CPModelParams func pats m) =
    "CPModelParams:\n\n"
    ++ prettyShow func ++ "\n"
    ++ concatMap (\p -> prettyShow p ++ "\n") pats ++ "\n"
    ++ prettyShow m

instance PrettyPrint FunctionGraphData where
  prettyShow p =
    "FunctionGraphData:\n"
    ++ prettyShow (funcNodes p) ++ "\n"
    ++ "Label DOMs: " ++ show (funcLabelDoms p) ++ "\n"
    ++ "TODO: pretty-print constraints" ++ "\n"

instance PrettyPrint PatternGraphData where
  prettyShow p =
    "PatternGraphData (ID " ++ show (patId p) ++ "):\n"
    ++ prettyShow (patNodes p) ++ "\n"
    ++ "Data " ++ prettyShow (patDataUseDefs p) ++ "\n"
    ++ "Label " ++ prettyShow (patLabelUseDefs p) ++ "\n"
    ++ "State " ++ prettyShow (patStateUseDefs p) ++ "\n"
    ++ "TODO: pretty-print constraints\n"
    ++ "Matchsets:\n" ++ concatMap (\m -> show m ++ "\n") (patMatchsets p)

instance PrettyPrint NodePartition where
  prettyShow np =
    "Computation nodes: " ++ show (computationNodes np) ++ "\n"
    ++ "Control nodes: " ++ show (controlNodes np) ++ "\n"
    ++ "Data nodes: " ++ show (dataNodes np) ++ "\n"
    ++ "Label nodes: " ++ show (labelNodes np) ++ "\n"
    ++ "Phi nodes: " ++ show (phiNodes np) ++ "\n"
    ++ "State nodes: " ++ show (stateNodes np) ++ "\n"
    ++ "Transfer nodes: " ++ show (transferNodes np)

instance PrettyPrint UseDefNodes where
  prettyShow ns =
    "Use-defs:\n"
    ++ "Uses: " ++ show (useNodes ns) ++ "\n"
    ++ "Defs: " ++ show (defNodes ns)

instance PrettyPrint MachineData where
  prettyShow m = "MachineData"
