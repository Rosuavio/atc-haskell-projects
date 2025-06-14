module Main where

import Control.Monad (when)
import qualified Data.Text.IO as T
import System.Directory.OsPath
  ( Permissions (readable)
  , doesFileExist
  , getPermissions
  )
import System.OsPath (decodeFS)

import Util

main :: IO ()
main = do
  todoFilePath <- getDefaultFile
  canRead <- doesFileExist todoFilePath >>= \case
    False -> pure False
    True -> readable <$> getPermissions todoFilePath
  when canRead $
    T.putStr =<< T.readFile =<< decodeFS todoFilePath
