--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Graphs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
--------------------------------------------------------------------------------

module Language.InstrSel.Graphs
  ( module Language.InstrSel.Graphs.Base
  , module Language.InstrSel.Graphs.IDs
  , module Language.InstrSel.Functions.IDs
  )
where

import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.IDs
import Language.InstrSel.Functions.IDs
  ( BasicBlockLabel (..) )