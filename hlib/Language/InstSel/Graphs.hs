--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Graphs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
--------------------------------------------------------------------------------

module Language.InstSel.Graphs
  ( module Language.InstSel.Graphs.Base
  , module Language.InstSel.Graphs.IDs
  , module Language.InstSel.Functions.IDs
  )
where

import Language.InstSel.Graphs.Base
import Language.InstSel.Graphs.IDs
import Language.InstSel.Functions.IDs
  ( BasicBlockLabel (..) )