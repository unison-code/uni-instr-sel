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

import Language.InstrSel.OpStructures.Base



-------------
-- Functions
-------------

-- | Finds computations that are synonymous with a copy operation, and replaces
-- such computation nodes with 'CopyNode's.
canonicalizeCopies :: OpStructure -> OpStructure
canonicalizeCopies =
  -- TODO: implement
  undefined
