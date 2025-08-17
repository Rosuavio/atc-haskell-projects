module Main where

import Control.Monad (when)
import System.Directory.OsPath
  ( Permissions (readable)
  , doesFileExist
  , getPermissions
  )
import System.OsPath (decodeFS)

import qualified Data.ByteString as BS

import Util

main :: IO ()
main = do
  todoFilePath <- getDefaultFile
  canRead <- doesFileExist todoFilePath >>= \case
    False -> pure False
    True -> readable <$> getPermissions todoFilePath
  when canRead $
    BS.putStr =<< BS.readFile =<< decodeFS todoFilePath
