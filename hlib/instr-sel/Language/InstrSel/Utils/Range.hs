{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.Range where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.Base
  ( maybeRead )
import Language.InstrSel.Utils.JSON
import Data.Maybe
  ( fromJust
  , isJust
  )



--------------
-- Data types
--------------

-- | Record for representing a value range.
data Range t
  = Range
      { lowerBound :: t
        -- ^ Smallest possible value (i.e. inclusive).
      , upperBound :: t
        -- ^ Largest possible value (i.e. inclusive).
      }
  deriving (Show, Eq)



------------------------------------------
-- PrettyShow-related type class instances
------------------------------------------

instance (PrettyShow t, Eq t) => PrettyShow (Range t) where
  pShow r
    | lowerBound r == upperBound r = pShow $ lowerBound r
    | otherwise = let tuple = (lowerBound r, upperBound r)
                  in pShow tuple



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON i => FromJSON (Range i) where
  parseJSON v = do (lb, ub) <- parseJSON v
                   return $ Range { lowerBound = lb, upperBound = ub }

instance ToJSON i => ToJSON (Range i) where
  toJSON r = toJSON (lowerBound r, upperBound r)



-------------
-- Functions
-------------

-- | Checks if a range is completely contained within another range.
contains
  :: Ord t
  => Range t
     -- ^ The containing range.
  -> Range t
     -- ^ The range to be contained.
  -> Bool
contains r1 r2 =
  lowerBound r1 <= lowerBound r2 && upperBound r2 <= upperBound r1

-- | Makes a 'Range' from a single value.
rangeFromSingleton :: t -> Range t
rangeFromSingleton i = Range { lowerBound = i, upperBound = i }

-- | Checks if a given range is a single value.
isRangeSingleton :: Eq t => Range t -> Bool
isRangeSingleton r = lowerBound r == upperBound r

-- | Parses a string into a 'Range'.
parseRangeStr :: Read t => String -> Maybe (Range t)
parseRangeStr str =
  let singleton = maybeRead str
      tuple = maybeRead str
  in if isJust singleton
     then Just $ rangeFromSingleton $ fromJust singleton
     else if isJust tuple
          then Just $ Range { lowerBound = fst (fromJust tuple)
                            , upperBound = snd (fromJust tuple)
                            }
          else Nothing
