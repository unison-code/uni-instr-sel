{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.String
  ( capitalize
  , maybeRead
  , isNumeric
  , replace
  , splitOn
  , splitStartingOn
  , toLower
  , toUpper
  , trim
  )
where

import Data.List
  ( intercalate )
import qualified Data.List.Split as Split
import qualified Data.Char as Char
  ( toLower
  , toUpper
  , isDigit
  )
import qualified Data.Text as Text
  ( pack
  , unpack
  , strip
  )
import Safe
  ( readMay )



-------------
-- Functions
-------------

-- | Trims whitespace from both ends of a given 'String'.
trim :: String -> String
trim = Text.unpack . Text.strip . Text.pack

-- | Splits a given list into a list of sublists at points where a given
-- delimiter is found (the delimiters themselves are removed from the resulting
-- list). For example:
--
-- > splitOn ".." "a..b....c" == ["a", "b", "", "c"]
splitOn
  :: Eq a
  => [a]
     -- ^ The delimiter.
  -> [a]
     -- ^ List to be split.
  -> [[a]]
splitOn = Split.splitOn

-- | Splits a given list into a list of sublists at points where any of the
-- given delimiters are found. For example:
--
-- > splitStartingOn "['A'..'Z'] "AStringToBeSplit" == ["A", "String", "To", "Be", "Split"]
splitStartingOn
  :: Eq a
  => [a]
     -- ^ List of delimiters.
  -> [a]
     -- ^ List to be split.
  -> [[a]]
splitStartingOn = Split.split . Split.startsWithOneOf

-- | Replaces a substring with another substring.
replace
  :: String
     -- ^ What to search for.
  -> String
     -- ^ What to replace with.
  -> String
     -- ^ What to search in.
  -> String
replace old new = intercalate new . splitOn old

-- | Converts a string to lowercase.
toLower :: String -> String
toLower = map Char.toLower

-- | Converts a string to uppercase.
toUpper :: String -> String
toUpper = map Char.toUpper

-- | Converts a string such that the first character is in uppercase and all
-- other characters are in lowercase. For example:
--
-- > capitalize "tEsT STRing" == "Test string"
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (Char.toUpper c:map Char.toLower cs)

-- | Checks whether a given string is numeric. An empty string is considered not
-- numeric.
isNumeric :: String -> Bool
isNumeric [] = False
isNumeric cs = all Char.isDigit cs

-- | Returns the data corresponding to a given parsed 'String'. If parsing
-- fails, 'Nothing' is returned.
maybeRead :: Read a => String -> Maybe a
maybeRead = readMay
