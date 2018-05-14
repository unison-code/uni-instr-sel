{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.OpStructures.Base
  ( OpStructure (..)
  , addConstraints
  , mkEmpty
  )
where

import Language.InstrSel.Constraints
import qualified Language.InstrSel.Graphs as G
  ( Graph
  , NodeID
  , mkEmpty
  )
import Language.InstrSel.TargetMachines.IDs
  ( LocationID )
import Language.InstrSel.Utils.JSON



--------------
-- Data types
--------------

-- | Contains a 'G.Graph' and various information about the 'G.Graph'.
data OpStructure
  = OpStructure
      { osGraph :: G.Graph
      , osValidLocations :: [(G.NodeID, [LocationID])]
        -- ^ The first element represents the ID of a value node.
      , osSameLocations :: [(G.NodeID, G.NodeID)]
        -- ^ Pairs of value nodes that must be given the same location.
      , osConstraints :: [Constraint]
      }
  deriving (Show)



--------------------------
-- JSON-related instances
--------------------------

instance FromJSON OpStructure where
  parseJSON (Object v) =
    OpStructure
      <$> v .: "graph"
      <*> v .: "valid-locs"
      <*> v .: "same-locs"
      <*> v .: "constraints"
  parseJSON _ = mzero

instance ToJSON OpStructure where
  toJSON os =
    object [ "graph"            .= (osGraph os)
           , "valid-locs"       .= (osValidLocations os)
           , "same-locs"        .= (osSameLocations os)
           , "constraints"      .= (osConstraints os)
           ]



-------------
-- Functions
-------------

-- | Creates an empty 'OpStructure'.
mkEmpty :: OpStructure
mkEmpty = OpStructure { osGraph = G.mkEmpty
                      , osValidLocations = []
                      , osSameLocations = []
                      , osConstraints = []
                      }

-- | Adds a list of 'Constraint's to an 'OpStructure'.
addConstraints :: OpStructure -> [Constraint] -> OpStructure
addConstraints os cs = os { osConstraints = osConstraints os ++ cs }
