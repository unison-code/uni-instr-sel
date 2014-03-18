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
import Language.InstructionSelection.PrettyPrint



-- | Wrapper for all model parameters.

data CPModelParams
    = CPModelParams {
          funcData :: FunctionGraphData
        , patInstData :: [PatternInstanceData]
        , machData :: MachineData
      }
    deriving (Show)

-- | Describes the necessary function graph data.

data FunctionGraphData
    = FunctionGraphData {

          -- | The action nodes in the function graph.

          funcActionNodes :: [NodeId]

          -- | The entity nodes in the function graph.

        , funcEntityNodes :: [NodeId]

          -- | The label nodes in the function graph, along with their dominator
          -- sets.

        , funcLabelDoms :: [( NodeId   -- ^ The dominated label node.
                            , [NodeId] -- ^ The dominator set.
                            )]

          -- | The function constraints, if any.

        , funcConstraints :: [Constraint]

      }
    deriving (Show)

-- | Describes the necessary pattern instance data.

data PatternInstanceData
    = PatternInstanceData {

          -- | The matchset ID.

          patMatchsetId :: MatchsetId

          -- | The action nodes in the function graph which are covered by this
          -- pattern instance.

        , patCoveredActionNodes :: [NodeId]

          -- | The entity nodes in the function graph which are defined by this
          -- pattern instance.

        , patDefinedEntityNodes :: [NodeId]

          -- | The entity nodes in the function graph which are used by this
          -- pattern instance.

        , patUsedEntityNodes :: [NodeId]

          -- | The pattern-specific constraints, if any. All node IDs used in
          -- the patterns refer to nodes in the function graph (not the pattern
          -- graph).

        , patConstraints :: [Constraint]

          -- | The latency of the instruction associated with this pattern
          -- instance.

        , patLatency :: Integer

          -- | The code size of the instruction associated with this pattern
          -- instance.

        , patCodeSize :: Integer

      }
    deriving (Show)

-- | Contains the necessary target machine data.

data MachineData
    = MachineData {

          -- TODO: implement

      }
    deriving (Show)



------------------------
-- Type class instances
------------------------

instance PrettyPrint CPModelParams where
  prettyShow p =
    "CPModelParams:\n\n"
    ++ prettyShow (funcData p) ++ "\n"
    ++ concatMap (\d -> prettyShow d ++ "\n") (patInstData p) ++ "\n"
    ++ prettyShow (machData p)

instance PrettyPrint FunctionGraphData where
  prettyShow d =
    "FunctionGraphData:\n"
    ++ "Action nodes: " ++ show (funcActionNodes d) ++ "\n"
    ++ "Entity nodes: " ++ show (funcEntityNodes d) ++ "\n"
    ++ "Label nodes: " ++ show (funcLabelDoms d) ++ "\n"
    ++ "TODO: pretty-print constraints" ++ "\n"

instance PrettyPrint PatternInstanceData where
  prettyShow d =
    "PatternInstanceData (Matchset ID: " ++ show (patMatchsetId d) ++ "):\n"
    ++ "Covered action nodes: " ++ show (patCoveredActionNodes d) ++ "\n"
    ++ "Defined entity nodes: " ++ show (patDefinedEntityNodes d) ++ "\n"
    ++ "Used entity nodes: " ++ show (patUsedEntityNodes d) ++ "\n"
    ++ "Code size: " ++ show (patCodeSize d) ++ "\n"
    ++ "Latency: " ++ show (patLatency d) ++ "\n"
    ++ "TODO: pretty-print constraints\n"

instance PrettyPrint MachineData where
  prettyShow d = "MachineData"
