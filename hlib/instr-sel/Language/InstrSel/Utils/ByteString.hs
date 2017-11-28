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
  , tokenize
  , BS.unpack
  , BS.writeFile
  )
where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as StrictBS



-------------
-- Functions
-------------

-- | Replaces a substring with another substring. This is an expensive operation
-- as it forces its arguments to be strict.
replace
  :: BS.ByteString
     -- ^ What to search for.
  -> BS.ByteString
     -- ^ What to replace with.
  -> BS.ByteString
     -- ^ What to search in.
  -> BS.ByteString
replace old new = BS.intercalate new . tokenize old

-- | Breaks a string into substrings.
tokenize
  :: BS.ByteString
     -- ^ Delimiter.
  -> BS.ByteString
     -- ^ String to tokenize.
  -> [BS.ByteString]
tokenize x y =
  let x' = BS.toStrict x
      y' = BS.toStrict y
  in map BS.fromStrict $ tokenize' x' y'

-- | Breaks a string into substrings.
tokenize' :: StrictBS.ByteString -> StrictBS.ByteString -> [StrictBS.ByteString]
tokenize' x y =
  ( h:if StrictBS.null t
      then []
      else tokenize' x (StrictBS.drop (StrictBS.length x) t)
  )
  where (h, t) = StrictBS.breakSubstring x y
