--------------------------------------------------------------------------------
-- |
-- Module      : Language.InstrSel.Utils.IO
-- Copyright   : (c) Gabriel Hjort Blindell 2013-2015
-- License     : BSD-style (see the LICENSE file)
--
-- Maintainer  : ghb@kth.se
-- Stability   : experimental
-- Portability : portable
--
-- Various IO-related functions.
--
--------------------------------------------------------------------------------

module Language.InstrSel.Utils.IO
  ( reportError
  , reportErrorAndExit
  , doesFileExist
  , readFileContent
  , when
  , mapMaybeM
  )
where



import Control.Monad
  ( when )

import System.IO
  ( hPutStrLn
  , stderr
  )

import qualified System.Directory as D
  ( doesFileExist )

import System.Exit
  ( exitFailure )

import Data.Maybe
  ( catMaybes )



-------------
-- Functions
-------------

-- | Writes a message and a newline to @STDERR@.
reportError :: String -> IO ()
reportError msg =
  hPutStrLn stderr msg

-- | Reports an error, and then terminates the program with an appropriate exit
-- code. For how the error messages is written, see 'reportError'.
reportErrorAndExit :: String -> IO a
reportErrorAndExit msg =
  do reportError msg
     exitFailure

-- | Checks if a given file exists.
doesFileExist :: FilePath -> IO Bool
doesFileExist = D.doesFileExist

-- | Reads the content of a given file. If the file does not exist an error is
-- reported and then the program fails.
readFileContent :: FilePath -> IO String
readFileContent file =
  do exists_file <- doesFileExist file
     when (not exists_file) $
       reportErrorAndExit $ "File " ++ show file ++ " does not exist."
     readFile file

-- | Same as 'mapM' but also throws away elements which have been evaluted to
-- 'Nothing'.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as =
  do bs <- mapM f as
     return $ catMaybes bs
