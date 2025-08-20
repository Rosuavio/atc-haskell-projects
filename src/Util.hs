module Util
  ( forkWithCallback
  , getDefaultFile
  , getLines
  , isFileReadable
  ) where

import Control.Concurrent (forkIO)
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

forkWithCallback :: MonadIO m => IO a -> (a -> IO ()) -> m ()
forkWithCallback f = liftIO . void . forkIO . (f >>=)

getDefaultFile :: IO OsPath
getDefaultFile = getXdgDirectory XdgState $ unsafeEncodeUtf "todo"

isFileReadable :: OsPath -> IO Bool
isFileReadable path = doesFileExist path
  >>= bool (pure False) (readable <$> getPermissions path)

getLines :: OsPath -> IO [Text]
getLines path = withFile path ReadMode (fmap T.lines . T.hGetContents)
