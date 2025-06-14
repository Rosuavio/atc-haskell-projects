module Util where

import Data.Sequence (Seq ((:|>)))
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory.OsPath (XdgDirectory (XdgState), getXdgDirectory)
import System.IO (Handle, hIsEOF)
import System.OsPath (OsPath, unsafeEncodeUtf)

hGetNLines :: Handle -> Int -> IO (Seq Text)
hGetNLines fileHandle = go mempty
  where
    go :: Seq Text -> Int -> IO (Seq Text)
    go acc 0 = pure acc
    go acc linesToGet = do
      isEOF <- hIsEOF fileHandle
      case isEOF of
        True -> pure acc
        False -> do
          line <- T.hGetLine fileHandle
          go (acc :|> line) (linesToGet - 1)

getDefaultFile :: IO OsPath
getDefaultFile = getXdgDirectory XdgState $ unsafeEncodeUtf "todo"
