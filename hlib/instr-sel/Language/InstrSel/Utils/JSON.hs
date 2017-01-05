{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.InstrSel.Utils.JSON
  ( module Data.Aeson
  , module Control.Applicative
  , module Control.Monad
  , Scientific
  , fromJson
  , pack
  , toJson
  , unpack
  , hasField
  )
where

import Language.InstrSel.Utils.Base
  ( replace )

import Data.Aeson
import Data.Aeson.Types
  ( Parser )
import qualified Data.HashMap.Strict as H
  ( lookup )
import qualified Data.ByteString.Lazy.Char8 as BS
  ( pack
  , unpack
  )
import Data.Scientific
  ( Scientific )
import qualified Data.Text as T
  ( Text
  , pack
  , unpack
  )

import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Control.Monad
  ( mzero
  , when
  )
import Data.Maybe
  ( fromJust
  , isJust
  )



-------------
-- Functions
-------------

-- | Parses a JSON string into an entity. An empty string will always result in
-- an error.
fromJson
  :: FromJSON a
  => String
  -> Either String a
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field the parsed entity (when parsing was successful).
fromJson [] = Left "failed to parse JSON: empty string"
fromJson s =
  let result = decode (BS.pack s)
  in if isJust result
     then Right (fromJust result)
     else Left "failed to parse JSON: unknown error"

-- | Converts an entity into a JSON string.
toJson :: ToJSON a => a -> String
toJson = unescape . BS.unpack . encode
  where unescape = replace "\\u003c" "<" . replace "\\u003e" ">"
        -- For security reasons, Aeson will escape '<' and '>' when dumping JSON
        -- data to string, which is something we want to undo.

-- | Converts 'Text' into a 'String'.
unpack :: T.Text -> String
unpack = T.unpack

-- | Converts a 'String' into 'Text'.
pack :: String -> T.Text
pack = T.pack

-- | Checks if the given JSON object has a field of certain name.
hasField :: Object -> T.Text -> Parser Bool
hasField obj key = case H.lookup key obj of
                     Nothing -> return False
                     Just _  -> return True
