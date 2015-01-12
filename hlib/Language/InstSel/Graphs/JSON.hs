--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs.JSON
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Converts the graph data structures in and out of JSON format.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstSel.Graphs.JSON where

import Language.InstSel.Graphs.Base
import Language.InstSel.Utils.JSON
import qualified Data.Text as T
  ( unpack )



------------------------
-- Type class instances
------------------------

instance FromJSON NodeID where
  parseJSON (Number sn) = return $ toNodeID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON NodeID where
  toJSON nid = toJSON (fromNodeID nid)

instance FromJSON (Domset NodeID) where
  parseJSON (Object v) =
    Domset
      <$> v .: "node"
      <*> v .: "domset"
  parseJSON _ = mzero

instance ToJSON (Domset NodeID) where
  toJSON d =
    object [ "node"   .= (domNode d)
           , "domset" .= (domSet d)
           ]
