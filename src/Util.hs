module Util
  ( forkWithCallback
  , getDefaultFile
  , getLines
  , isFileReadable
  , line
  ) where

import Control.Concurrent (forkIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (bool)
import Data.Functor (void)
import Data.Text (Text)
import System.Directory.OsPath
  ( Permissions (readable)
  , XdgDirectory (XdgState)
  , doesFileExist
  , getPermissions
  , getXdgDirectory
  )
import System.File.OsPath (withFile)
import System.IO (IOMode (ReadMode))
import System.OsPath (OsPath, unsafeEncodeUtf)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Reflex
import Reflex.Vty

forkWithCallback :: MonadIO m => IO a -> (a -> IO ()) -> m ()
forkWithCallback f = liftIO . void . forkIO . (f >>=)

getDefaultFile :: IO OsPath
getDefaultFile = getXdgDirectory XdgState $ unsafeEncodeUtf "todo"

isFileReadable :: OsPath -> IO Bool
isFileReadable path = doesFileExist path
  >>= bool (pure False) (readable <$> getPermissions path)

getLines :: OsPath -> IO [Text]
getLines path = withFile path ReadMode (fmap T.lines . T.hGetContents)

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
