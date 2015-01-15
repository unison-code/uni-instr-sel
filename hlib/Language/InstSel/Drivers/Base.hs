--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstSel.Drivers.Base
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2014
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Contains data types for drivers.
--
--------------------------------------------------------------------------------

module Language.InstSel.Drivers.Base
  ( module Language.InstSel.Functions
  , module Language.InstSel.Utils
  , module Language.InstSel.Utils.IO
  , Output (..)
  , getFunction
  , toOutput
  , toOutputWithoutID
  )
where

import Language.InstSel.Functions
  ( Function )
import Language.InstSel.Utils
  ( fromLeft
  , fromRight
  , isLeft
  )
import Language.InstSel.Utils.IO
import Language.InstSel.Utils.JSON



--------------
-- Data types
--------------

-- | A representation of the output produced by the drivers.
data Output
  = Output
      { oID :: String
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
toOutputWithoutID = toOutput ""

-- | Creates an output.
toOutput
  :: String
     -- ^ The ID.
  -> String
     -- ^ The output string.
  -> Output
toOutput oid s = Output { oID = oid, oData = s }

-- | Gets the function from a JSON string. Reports error if this fails.
getFunction :: String -> IO Function
getFunction str =
  do let res = fromJson str
     when (isLeft res) $
       reportError $ fromLeft res
     return $ fromRight res
