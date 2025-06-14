module Main where

import Control.Monad (when)
import qualified Data.Text.IO as T
import System.Directory.OsPath
  ( Permissions (readable)
  , XdgDirectory (XdgState)
  , doesFileExist
  , getPermissions
  , getXdgDirectory
  )
import System.OsPath (decodeFS, unsafeEncodeUtf)

main :: IO ()
main = do
  todoFilePath <- getXdgDirectory XdgState $ unsafeEncodeUtf "todo"
  canRead <- doesFileExist todoFilePath >>= \case
    False -> pure False
    True -> readable <$> getPermissions todoFilePath
  when canRead $
    T.putStr =<< T.readFile =<< decodeFS todoFilePath
