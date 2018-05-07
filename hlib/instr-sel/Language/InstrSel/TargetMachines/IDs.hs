{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Language.InstrSel.TargetMachines.IDs
  ( InstructionID (..)
  , LocationID (..)
  , LocationName (..)
  , TargetMachineID (..)
  , fromInstructionID
  , fromLocationID
  , fromLocationName
  , fromTargetMachineID
  , toInstructionID
  , toLocationID
  , toLocationName
  , toTargetMachineID
  , toSafeTargetMachineID
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
import Language.InstrSel.Utils.String
  ( replace )

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

instance PrettyShow TargetMachineID where
  pShow (TargetMachineID i) = i



-------------------------------------
-- JSON-related type class instances
-------------------------------------

instance FromJSON InstructionID where
  parseJSON (JSON.Number sn) = return $ toInstructionID $ sn2nat sn
  parseJSON _ = mzero

instance ToJSON InstructionID where
  toJSON iid = toJSON (fromInstructionID iid)

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

instance NFData TargetMachineID where
  rnf (TargetMachineID a) = rnf a



-------------
-- Functions
-------------

fromLocationID :: LocationID -> Natural
fromLocationID (LocationID i) = i

toLocationID :: (Integral i) => i -> LocationID
toLocationID = LocationID . toNatural

fromTargetMachineID :: TargetMachineID -> String
fromTargetMachineID (TargetMachineID i) = i

toTargetMachineID :: String -> TargetMachineID
toTargetMachineID = TargetMachineID

-- | Converts a 'String' to a 'TargetMachineID' that can safely be used as
-- Haskell module names.
toSafeTargetMachineID :: String -> TargetMachineID
toSafeTargetMachineID =
  toTargetMachineID .
  replace "-" "Minus" .
  replace "_" "Underscore"

fromInstructionID :: InstructionID -> Natural
fromInstructionID (InstructionID i) = i

toInstructionID :: (Integral i) => i -> InstructionID
toInstructionID = InstructionID . toNatural

fromLocationName :: LocationName -> String
fromLocationName (LocationName s) = s

toLocationName :: String -> LocationName
toLocationName = LocationName
