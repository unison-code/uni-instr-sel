--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Utils.JSON
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Extends Data.Aeson with some additional, useful functions.
--
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstSel.Utils.JSON
  ( module Data.Aeson
  , module Control.Applicative
  , module Control.Monad
  , fromJson
  , pack
  , sn2nat
  , toJson
  , unpack
  )
where

import Language.InstSel.Utils
  ( Natural
  , fromNatural
  , replace
  , toNatural
  )
import Data.Aeson
import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Control.Monad
  ( mzero
  , when
  )
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import Data.Maybe
  ( fromJust
  , isJust
  )
import Data.Scientific
  ( Scientific )
import qualified Data.Text as T
  ( Text
  , pack
  , unpack
  )



-------------
-- Functions
-------------

-- | Converts a scientific number to a natural number. If the number is not an
-- non-negative then an error occurs.
sn2nat :: Scientific -> Natural
sn2nat sn =
  let int_value = round sn
  in if fromInteger int_value /= sn
     then error "not an integer"
     else toNatural int_value

-- | Parses a JSON string into an entity.
fromJson ::
  FromJSON a
  => String
  -> Either String a
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field the parsed entity (when parsing was successful).
fromJson s =
  let result = decode (BS.pack s)
  in if isJust result
     then Right (fromJust result)
     else Left ("failed to parse JSON")

-- | Converts an entity into a JSON string.
toJson :: ToJSON a => a -> String
toJson = unescape . BS.unpack . encode
  where unescape = replace "\\u003c" "<" . replace "\\u003e" ">"
        -- ^ For security reasons, Aeson will escape '<' and '>' when dumping
        -- JSON data to string, which is something we want to undo.

-- | Converts 'Text' into a 'String'.
unpack :: T.Text -> String
unpack = T.unpack

-- | Converts a 'String' into 'Text'.
pack :: String -> T.Text
pack = T.pack



------------------------
-- Type class instances
------------------------

instance FromJSON Natural where
  parseJSON (Number sn) = return $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON Natural where
  toJSON i = toJSON (fromNatural i)
