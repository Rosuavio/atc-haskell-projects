{-# LANGUAGE TypeApplications #-}
module Util
  ( forkWithCallback
  , getDefaultFile
  , getTasks
  , line
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle, try)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import Data.Text (Text)
import System.Directory.OsPath (XdgDirectory (XdgState), getXdgDirectory)
import System.File.OsPath (withFile)
import System.IO (IOMode (ReadMode))
import System.OsPath (OsPath, unsafeEncodeUtf)
import Task (Task)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Task as TSK

import Reflex
import Reflex.Vty

forkWithCallback :: MonadIO m => IO a -> (a -> IO ()) -> m ()
forkWithCallback f = liftIO . void . forkIO . (f >>=)

getDefaultFile :: IO OsPath
getDefaultFile = getXdgDirectory XdgState $ unsafeEncodeUtf "todo"

getTasks :: OsPath -> IO (Either Text [Task])
getTasks path = handle @SomeException
  (const $ pure $ Left $ "failed to open " <> (T.pack $ show path)
    <> " in read mode")
  $ withFile path ReadMode $ \h ->
    flip fmap (try @SomeException $ T.hGetContents h)
    $ either (const $ Left $ "failed to read " <> (T.pack $ show path))
    $ maybe (Left $ "failed to parse " <> (T.pack $ show path)) Right
    . TSK.parseDoc

-- TODO: Lines that are too long get the word that reaches the
-- end of the line pushed to tne next line, behind (z-axis)
-- the next line
line ::
  ( Reflex t
  , HasTheme t m
  , MonadFix m
  , MonadHold t m
  , HasLayout t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocusReader t m)
  => Behavior t T.Text
  -> m ()
line = grout (fixed 1) . text
