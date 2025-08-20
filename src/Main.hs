module Main
  ( main
  ) where

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Graphics.Vty.CrossPlatform (mkVty)
import System.OsPath (decodeUtf)

import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Reflex
import Reflex.Network
import Reflex.Vty

import Util

main :: IO ()
main = do
  vty <- mkVty Vty.defaultConfig
  mainWidgetWithHandle vty $ initManager_ $ do
    gotFilePath <- getPostBuild
      >>= performEventAsync . (forkWithCallback getDefaultFile <$)

    fmap switchDyn $ networkHold (loadingView $ constant placeHolderFileName)
      $ ffor gotFilePath $ \path-> do
      pb <- getPostBuild
      fileName <- performEventAsync
        (forkWithCallback (T.pack <$> decodeUtf path) <$ pb)
        >>= hold placeHolderFileName
      gotFileLines <- performEventAsync
        (forkWithCallback (getLinesIfReadable path) <$ pb)

      fmap switchDyn $ networkHold (loadingView fileName) $ ffor gotFileLines
        $ maybe (quitablePrompt $ "Could not read " <> fileName <> ".")
        $ \f -> grout flex $ col $ do
        grout flex $ col $ traverse_ (grout (fixed 1) . text . constant) f
        grout (fixed 1) $ text "Press any key to quit."
        void <$> input
  where
    getLinesIfReadable path = isFileReadable path
      >>= bool (pure Nothing) (Just <$> getLines path)
    placeHolderFileName = "default TODO file"
    loadingView filename = quitablePrompt $ "Loading " <> filename <> "..."
    quitablePrompt msg = grout flex $ col $ do
      grout (fixed 1) $ text msg
      grout flex blank
      grout (fixed 1) $ text "Press Ctrl+c to quit."
      void <$> keyCombo (Vty.KChar 'c', [Vty.MCtrl])
