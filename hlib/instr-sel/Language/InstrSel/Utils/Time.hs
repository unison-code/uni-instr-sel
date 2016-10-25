{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>
  Roberto Castaneda Lozano <rcas@sics.se>

-}

module Language.InstrSel.Utils.Time
  ( Timestamp
  , getTime
  , secondsBetween
  )
where

import qualified System.Clock as Clock
  ( Clock (..)
  , TimeSpec
  , getTime
  , sec
  , nsec
  )



--------------
-- Data types
--------------

-- | The outer-most data type which contains a timestamp.
newtype Timestamp
  = Timestamp Clock.TimeSpec
  deriving (Show)



-------------
-- Functions
-------------

-- | Gets the current timestamp.
getTime :: IO Timestamp
getTime = do time <- Clock.getTime Clock.Realtime
             return $ Timestamp time

-- | Gets the seconds elapsed between two timestamps.
secondsBetween :: Timestamp -> Timestamp -> Double
secondsBetween (Timestamp start) (Timestamp end) =
    let secs = Clock.sec end - Clock.sec start
        nsecs = Clock.nsec end - Clock.nsec start
    in fromIntegral secs + ((fromIntegral nsecs) / 1000000000)
