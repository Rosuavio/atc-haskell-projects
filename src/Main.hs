module Main where

import Control.Monad (when)
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Terminal.Size (Window (height), size)
import System.Directory.OsPath
  ( Permissions (readable)
  , doesFileExist
  , getPermissions
  )
import System.File.OsPath (withFile)
import System.IO (IOMode (ReadMode))

import Util

main :: IO ()
main = size >>= \case
  Nothing -> pure ()
  Just w -> do
    todoFilePath <- getDefaultFile
    canRead <- doesFileExist todoFilePath >>= \case
      False -> pure False
      True -> readable <$> getPermissions todoFilePath
    when canRead $
      withFile todoFilePath ReadMode $ \fileHandle -> do
        ls <- hGetNLines fileHandle $ height w
        mapM_ T.putStr $ intersperse (T.singleton '\n') ls
