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

import System.FilePath
  ( splitExtension
  , takeDirectory
  , takeFileName
  )



--------------
-- Data types
--------------

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: Maybe String
        -- ^ A unique string that identifies this output.
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

-- | Emits output to a file of a given name and the output ID suffixed to the
-- file name before the file extension of the path. If the path has no file
-- extension, then the ID is simply appended to the path.
emitToFile :: FilePath -> Output -> IO ()
emitToFile fp o =
  let dname = takeDirectory fp
      (fname, ext) = splitExtension $ takeFileName fp
      oid = oID o
      filename =
        dname ++ (if length dname > 0 then "/" else "")
              ++ fname
              ++ ( if isJust oid
                   then (if length fname > 0 then "." else "") ++ fromJust oid
                   else "")
              ++ ext
  in writeFile filename (oData o)
