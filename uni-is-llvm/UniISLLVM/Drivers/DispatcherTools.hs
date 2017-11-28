{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniISLLVM.Drivers.DispatcherTools
  ( module UniISLLVM.Drivers.Base
  , module Language.InstrSel.Utils
  , module Language.InstrSel.Utils.IO
  , loadFileContent
  , loadFunctionFileContent
  , loadFromJson
  , loadFunctionFromJson
  )
where

import UniISLLVM.Drivers.Base

import Language.InstrSel.Functions
  ( Function )
import Language.InstrSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import qualified Language.InstrSel.Utils.ByteString as BS
import Language.InstrSel.Utils.IO
import Language.InstrSel.Utils.JSON

import Data.Maybe
  ( fromJust
  , isNothing
  )



-------------
-- Functions
-------------

-- | Loads the content of a file.
loadFileContent
  :: String
     -- ^ Error message when the file path is 'Nothing'.
  -> Maybe FilePath
     -- ^ The file to load.
  -> IO BS.ByteString
     -- ^ The file content.
loadFileContent err file =
  do when (isNothing file) $
       reportErrorAndExit err
     readFileContent $ fromJust file

loadFunctionFileContent :: Options -> IO BS.ByteString
loadFunctionFileContent =
  loadFileContent "No function file provided." . functionFile

-- | Parses a given JSON string and loads its content. Reports error if this
-- fails.
loadFromJson :: (FromJSON a) => BS.ByteString -> IO a
loadFromJson str =
  do let res = fromJson str
     when (isLeft res) $
       reportErrorAndExit $ fromLeft res
     return $ fromRight res

loadFunctionFromJson :: Options -> IO Function
loadFunctionFromJson opts =
  do content <- loadFunctionFileContent opts
     loadFromJson content
