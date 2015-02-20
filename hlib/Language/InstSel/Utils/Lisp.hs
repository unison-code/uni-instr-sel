--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Utils.Lisp
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Extends Data.AttoLisp with some additional, useful functions.
--
--------------------------------------------------------------------------------

module Language.InstSel.Utils.Lisp
  ( module Data.AttoLisp
  , module Control.Applicative
  , module Control.Monad
  , module Data.Attoparsec.Number
  , fromLispExprStr
  , toLispExprStr
  , wrapStruct,
  )
where

import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isRight
  )

import Data.AttoLisp
  hiding
  ( fromLispExpr )
import qualified Data.Attoparsec.ByteString as BS
  ( parseOnly )
import qualified Data.ByteString.Char8 as BC
  ( pack )
import Data.Attoparsec.Number
  ( Number (..) )

import Control.Applicative
  ( (<|>) )
import Control.Monad
  ( mzero )



-------------
-- Functions
-------------

-- | Parses a lispian expression string into an entity.
fromLispExprStr
  :: FromLisp a
  => String
  -> Either String a
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field contains the entity (when parsing succeeded).
fromLispExprStr s =
  let res = BS.parseOnly lisp (BC.pack s)
  in if isRight res
     then parseEither parseLisp (fromRight res)
     else Left (fromLeft res)

-- | Converts an entity into a lispian expression string.
toLispExprStr :: ToLisp a => a -> String
toLispExprStr = show . toLisp

-- | Creates a parser for an entity which just wraps another entity and does not
-- require any parsing of its own.
wrapStruct :: FromLisp b => (b -> a) -> Lisp -> Parser a
wrapStruct f e =
  let v = parseEither parseLisp e
  in if isRight v
     then return $ f (fromRight v)
     else mzero
