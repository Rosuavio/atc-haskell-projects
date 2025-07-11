{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.Functor (void, ($>))
import qualified Data.Text as T
import Graphics.Vty (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Reflex
import Reflex.Network
import Reflex.Vty
import System.Directory.OsPath
  ( Permissions (readable)
  , doesFileExist
  , getPermissions
  )
import System.File.OsPath (withFile)
import System.IO (IOMode (ReadMode))

import Control.Monad.Fix (MonadFix)
import System.OsPath (OsPath)
import Util

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  mainWidgetWithHandle vty $ initManager_ $ do
    quitEv <- input
    pb <- getPostBuild
    defaultFileInfoEv <- performEventAsync $
      pb $> \onComplete -> liftIO $ void $ forkIO $ do
        f <- getDefaultFile
        canRead <- doesFileExist f >>= \case
          False -> pure False
          True -> readable <$> getPermissions f
        onComplete (f, canRead)
    grout flex $ col $ do
      void $ grout flex $ do
        networkHold
          (text "Loading default file...")
          $ ffor defaultFileInfoEv
            $ \(filePath, canRead) -> col $ case canRead of
              False -> text $ constant $ "Can't raad file: "
                <> T.pack (show filePath)
              True -> fileView filePath
      grout (fixed $ constDyn 1) $ text "Press any key to continue..."
    pure $ void quitEv

fileView ::
  ( PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , Adjustable t m
  , HasInput t m
  , HasLayout t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasTheme t m
  , HasFocusReader t m
  , MonadHold t m
  , MonadFix m
  ) => OsPath -> m ()
fileView filePath = do
  pb <- getPostBuild
  height <- displayHeight
  readFileLines <- performEventAsync $ flip fmap (current height `tag` pb)
    $ \initialHeight -> liftIO . void . forkIO
      . (>>=) (withFile filePath ReadMode (`hGetNLines` initialHeight))
  void $ networkHold
    (text $ constant $ "file: " <> T.pack (show filePath))
    $ ffor readFileLines $ \fileLines ->
      fmap snd $ grout flex $ scrollable def $ col $ do
        for_ fileLines $ \line ->
          grout (fixed $ constDyn 1) $ text $ constant line
        -- Event that signals an update to the contents of
        -- scrollable
        pure (never, ())
