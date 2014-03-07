--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.CPModel.JsonDumper
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Dumps a CP model parameter data structure in a JSON format.
--
--------------------------------------------------------------------------------

module Language.InstructionSelection.CPModel.JsonDumper (
  toJson
) where

import Language.InstructionSelection.CPModel.Base
import Language.InstructionSelection.Graphs (NodeId)
import Language.InstructionSelection.Patterns (PatternId)



-------------
-- Functions
-------------

toJson :: CPModelParams -> String
toJson m =
  -- TODO: implement
  ""
