module Main
  ( main
  ) where

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
      gotCanReadFile <- performEventAsync
        $ forkWithCallback (isFileReadable path) <$ pb

      fmap switchDyn $ networkHold (loadingView fileName)
        $ ffor gotCanReadFile $ \case
        False -> quitablePrompt $ "Could not read " <> fileName <> "."
        True -> do
          gotFileLines <- getPostBuild
            >>= performEventAsync . ((forkWithCallback $ getLines path) <$)

          fmap switchDyn $ networkHold (loadingView fileName)
            $ ffor gotFileLines $ \f -> grout flex $ col $ do
            grout flex $ col $ traverse_ (grout (fixed 1) . text . constant) f
            grout (fixed 1) $ text "Press any key to quit."
            void <$> input
  where
    placeHolderFileName = "default TODO file"
    loadingView filename = quitablePrompt $ "Loading " <> filename <> "..."
    quitablePrompt msg = grout flex $ col $ do
      grout (fixed 1) $ text msg
      grout flex blank
      grout (fixed 1) $ text "Press Ctrl+c to quit."
      void <$> keyCombo (Vty.KChar 'c', [Vty.MCtrl])
