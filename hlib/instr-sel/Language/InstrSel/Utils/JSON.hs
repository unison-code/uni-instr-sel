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
  , hasField
  , T.pack
  , toJson
  , T.unpack
  )
where

import Language.InstrSel.Utils.Base
  ( fromLeft
  , isRight
  )
import qualified Language.InstrSel.Utils.ByteString as BS

import Data.Aeson
import Data.Aeson.Types
  ( Parser )
import qualified Data.HashMap.Strict as H
  ( lookup )
import Data.Scientific
  ( Scientific )
import qualified Data.Text as T

import Control.Applicative
  ( (<$>)
  , (<*>)
  )
import Control.Monad
  ( mzero
  , when
  )



-------------
-- Functions
-------------

-- | Parses a JSON string into an entity. An empty string will always result in
-- an error.
fromJson
  :: FromJSON a
  => BS.ByteString
  -> Either String a
     -- ^ The left field contains the error message (when parsing failed), and
     -- the right field the parsed entity (when parsing was successful).
fromJson s =
  let result = eitherDecode s
  in if isRight result
     then result
     else Left $ "failed to parse JSON: " ++ fromLeft result

-- | Converts an entity into a JSON string.
toJson :: ToJSON a => a -> BS.ByteString
toJson = unescape . encode
  where unescape = BS.replace (BS.pack "\\u003c") (BS.pack "<") .
                   BS.replace (BS.pack "\\u003e") (BS.pack ">")
                   -- For security reasons, Aeson will escape '<' and '>' when
                   -- dumping JSON data to string, which is something we want to
                   -- undo.

-- | Checks if the given JSON object has a field of certain name.
hasField :: Object -> T.Text -> Parser Bool
hasField obj key = case H.lookup key obj of
                     Nothing -> return False
                     Just _  -> return True
