module Util
  ( getDefaultFile
  ) where

import System.Directory.OsPath (XdgDirectory (XdgState), getXdgDirectory)
import System.OsPath (OsPath, unsafeEncodeUtf)

getDefaultFile :: IO OsPath
getDefaultFile = getXdgDirectory XdgState $ unsafeEncodeUtf "todo"
