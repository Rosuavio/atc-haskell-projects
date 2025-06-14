module Main where

import Control.Monad (when)
import System.Directory.OsPath
  ( Permissions (readable)
  , XdgDirectory (XdgState)
  , doesFileExist
  , getPermissions
  , getXdgDirectory
  )
import System.OsPath (decodeFS, unsafeEncodeUtf)

import Data.ByteString as BS

main :: IO ()
main = do
  todoFilePath <- getXdgDirectory XdgState $ unsafeEncodeUtf "todo"
  canRead <- doesFileExist todoFilePath >>= \case
    False -> pure False
    True -> readable <$> getPermissions todoFilePath
  when canRead $
    BS.putStr =<< BS.readFile =<< decodeFS todoFilePath
