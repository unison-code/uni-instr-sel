--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Misc.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Contains generic data and types.
-- 
--------------------------------------------------------------------------------

module Language.InstructionSelection.Misc.Base where

-- | Record for representing a range.

data Range t
     = Range {
          -- | Smallest possible value (i.e. inclusive).

          lowerBound :: t
          
          -- | Largest possible value (i.e. inclusive).

        , upperBound :: t

       }
     deriving (Show, Eq)
