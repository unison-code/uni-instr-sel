{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.ByteString
  ( BS.ByteString
  , BS.intercalate
  , BS.length
  , BS.pack
  , BS.readFile
  , replace
  , BS.replicate
  , splitOn
  , BS.unpack
  , BS.writeFile
  )
where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Search as Search



-------------
-- Functions
-------------

-- | Replaces a substring with another substring.
replace
  :: BS.ByteString
     -- ^ What to search for.
  -> BS.ByteString
     -- ^ What to replace with.
  -> BS.ByteString
     -- ^ What to search in.
  -> BS.ByteString
replace old new = Search.replace (BS.toStrict old) new

-- | Splits a given list into a list of sublists at points where a given
-- delimiter is found (the delimiters themselves are removed from the resulting
-- list). For example:
--
-- > splitOn ".." "a..b....c" == ["a", "b", "", "c"]
splitOn
  :: BS.ByteString
     -- ^ The delimiter.
  -> BS.ByteString
     -- ^ String to be split.
  -> [BS.ByteString]
splitOn pat = Search.split (BS.toStrict pat)
