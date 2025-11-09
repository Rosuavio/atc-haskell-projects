module Main
  ( main
  ) where

import Data.Functor (void)
import Graphics.Vty.CrossPlatform (mkVty)
import System.OsPath (decodeUtf)

import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Reflex
import Reflex.Network
import Reflex.Vty

import TasksView
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
      gotTasks <- performEventAsync
        (forkWithCallback (getTasks path) <$ pb)

      fmap switchDyn $ networkHold (loadingView fileName) $ ffor gotTasks
        $ either (quitablePrompt . constant . T.pack) tasksView
  where
    placeHolderFileName = "default TODO file"
    loadingView filename = quitablePrompt $ "Loading " <> filename <> "..."
    quitablePrompt msg = grout flex $ col $ do
      line msg
      grout flex blank
      line "Press Ctrl+c to quit."
      void <$> keyCombo (Vty.KChar 'c', [Vty.MCtrl])
