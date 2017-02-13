{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Utils.IO
  ( ExitCode (..)
  , reportError
  , reportErrorAndExit
  , doesFileExist
  , readFileContent
  , foldM
  , foldM_
  , when
  , catMaybeM
  , mapMaybeM
  , errorExitCode
  , successExitCode
  )
where



import Control.Monad
  ( foldM
  , foldM_
  , when
  )

import System.IO
  ( hPutStrLn
  , stderr
  )

import qualified System.Directory as D
  ( doesFileExist )

import System.Exit
  ( ExitCode (..)
  , exitFailure
  )

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

-- | Throws away 'Nothing' elements.
catMaybeM :: Monad m => m [Maybe a] -> m [a]
catMaybeM as =
  do as' <- as
     return $ catMaybes as'

-- | Same as 'mapM' but also throws away elements which have been evaluted to
-- 'Nothing'.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as =
  do bs <- mapM f as
     return $ catMaybes bs

-- | Returns an 'ExitCode' that indicates a success.
successExitCode :: ExitCode
successExitCode = ExitSuccess

-- | Returns an 'ExitCode' that indicates an error.
errorExitCode :: ExitCode
errorExitCode = ExitFailure 1
