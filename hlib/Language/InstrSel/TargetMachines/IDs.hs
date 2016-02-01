--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.TargetMachines.IDs
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains the data types for representing various IDs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.TargetMachines.IDs
  ( InstructionID (..)
  , PatternID (..)
  , LocationID (..)
  , LocationName (..)
  , TargetMachineID (..)
  , fromInstructionID
  , fromPatternID
  , fromLocationID
  , fromLocationName
  , fromTargetMachineID
  , toInstructionID
  , toPatternID
  , toLocationID
  , toLocationName
  , toTargetMachineID
  )
where

import Language.InstrSel.PrettyShow
import Language.InstrSel.Utils.Natural
import Language.InstrSel.Utils.Lisp
  hiding
  ( Lisp (..) )
import qualified Language.InstrSel.Utils.Lisp as Lisp
  ( Lisp (..) )
import Language.InstrSel.Utils.JSON
  hiding
  ( Value (..) )
import qualified Language.InstrSel.Utils.JSON as JSON
  ( Value (..) )

import Control.DeepSeq
  ( NFData
  , rnf
  )


--------------
-- Data types
--------------

newtype InstructionID
  = InstructionID Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow InstructionID where
  pShow (InstructionID i) = pShow i

newtype PatternID
  = PatternID Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow PatternID where
  pShow (PatternID i) = pShow i

newtype LocationID
  = LocationID Natural
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance PrettyShow LocationID where
  pShow (LocationID i) = pShow i

newtype LocationName
  = LocationName String
  deriving (Show, Eq)

instance PrettyShow LocationName where
  pShow (LocationName s) = s

newtype TargetMachineID
  = TargetMachineID String
  deriving (Show, Eq)



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON InstructionID where
  parseJSON (JSON.Number sn) = return $ toInstructionID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON InstructionID where
  toJSON iid = toJSON (fromInstructionID iid)

instance FromJSON PatternID where
  parseJSON (JSON.Number sn) = return $ toPatternID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON PatternID where
  toJSON pid = toJSON (fromPatternID pid)

instance FromJSON LocationID where
  parseJSON (JSON.Number sn) = return $ toLocationID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON LocationID where
  toJSON rid = toJSON (fromLocationID rid)

instance FromJSON TargetMachineID where
  parseJSON (JSON.String s) = return $ toTargetMachineID $ unpack s
  parseJSON _ = mzero

instance ToJSON TargetMachineID where
  toJSON tmid = toJSON (fromTargetMachineID tmid)



-------------------------------------
-- Lisp-related type class instances
-------------------------------------

instance FromLisp InstructionID where
  parseLisp (Lisp.Number (I n)) = return $ toInstructionID n
  parseLisp _ = mzero

instance ToLisp InstructionID where
  toLisp (InstructionID nid) = Lisp.Number (I (fromNatural nid))

instance FromLisp PatternID where
  parseLisp (Lisp.Number (I n)) = return $ toPatternID n
  parseLisp _ = mzero

instance ToLisp PatternID where
  toLisp (PatternID nid) = Lisp.Number (I (fromNatural nid))

instance FromLisp LocationID where
  parseLisp (Lisp.Number (I n)) = return $ toLocationID n
  parseLisp _ = mzero

instance ToLisp LocationID where
  toLisp (LocationID nid) = Lisp.Number (I (fromNatural nid))



----------------------------------------
-- DeepSeq-related type class instances
--
-- These are needed to be able to time
-- how long it takes to produce the
-- matchsets
----------------------------------------

instance NFData InstructionID where
  rnf (InstructionID a) = rnf a

instance NFData PatternID where
  rnf (PatternID a) = rnf a

instance NFData TargetMachineID where
  rnf (TargetMachineID a) = rnf a



-------------
-- Functions
-------------

fromPatternID :: PatternID -> Natural
fromPatternID (PatternID i) = i

toPatternID :: (Integral i) => i -> PatternID
toPatternID = PatternID . toNatural

fromLocationID :: LocationID -> Natural
fromLocationID (LocationID i) = i

toLocationID :: (Integral i) => i -> LocationID
toLocationID = LocationID . toNatural

fromTargetMachineID :: TargetMachineID -> String
fromTargetMachineID (TargetMachineID i) = i

toTargetMachineID :: String -> TargetMachineID
toTargetMachineID = TargetMachineID

fromInstructionID :: InstructionID -> Natural
fromInstructionID (InstructionID i) = i

toInstructionID :: (Integral i) => i -> InstructionID
toInstructionID = InstructionID . toNatural

fromLocationName :: LocationName -> String
fromLocationName (LocationName s) = s

toLocationName :: String -> LocationName
toLocationName = LocationName
