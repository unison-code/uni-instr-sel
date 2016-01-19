--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.DriverTools.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains help functions for when implementing drivers.
--
--------------------------------------------------------------------------------

module Language.InstrSel.DriverTools.Base
  ( Output (..)
  , emitToStdout
  , emitToFile
  , toOutput
  , toOutputWithoutID
  )
where

import Data.Maybe
  ( fromJust
  , isJust
  )

import System.FilePath.Posix
  ( splitExtension )



--------------
-- Data types
--------------

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: Maybe String
        -- ^ A unique string that identifies this output. If the output is to be
        -- written to file, the ID will be suffixed to the file name.
      , oData :: String
        -- ^ The produced output.
      }



-------------
-- Functions
-------------

-- | Creates an output that has no ID. This is useful when there is exactly one
-- output produced.
toOutputWithoutID
  :: String
     -- ^ The output string.
  -> Output
toOutputWithoutID = toOutput' Nothing

-- | Creates an output.
toOutput
  :: String
     -- ^ The ID.
  -> String
     -- ^ The output string.
  -> Output
toOutput oid s = toOutput' (Just oid) s

toOutput' :: Maybe String -> String -> Output
toOutput' oid s = Output { oID = oid, oData = s }

-- | Emits output to 'STDOUT'.
emitToStdout :: Output -> IO ()
emitToStdout = putStrLn . oData

-- | Emits output to a file of a given name and the output ID suffixed.
emitToFile :: FilePath -> Output -> IO ()
emitToFile fp o =
  let (fname, ext) = splitExtension fp
      oid = oID o
      filename =
        fname ++ (if isJust oid then "." ++ fromJust oid else "") ++ ext
  in writeFile filename (oData o)
