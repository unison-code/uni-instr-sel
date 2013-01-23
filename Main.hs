--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Program for invoking the instruction pattern parser.
-- 
--------------------------------------------------------------------------------

module Main (
    main
) where

import Language.LLVM.Patterns.Parser

main = do
  contents <- getContents
  putStr "\n"
  putStr $ show (parsePatterns contents)
  putStr "\n"
