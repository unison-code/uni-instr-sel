--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.ProgramModules.IDs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing various IDs.
--
--------------------------------------------------------------------------------

module Language.InstSel.ProgramModules.IDs
  ( BasicBlockLabel (..) )
where



--------------
-- Data types
--------------

-- | Represents a basic block label identifier.
newtype BasicBlockLabel =
    BasicBlockLabel String
  deriving (Eq)

instance Show BasicBlockLabel where
  show (BasicBlockLabel str) = str
