module Main
  ( main
  ) where

import Graphics.Vty.CrossPlatform (mkVty)

import qualified Graphics.Vty as Vty

import Reflex
import Reflex.Network
import Reflex.Vty

import TasksView
import Ui
import Util

main :: IO ()
main = do
  vty <- mkVty Vty.defaultConfig
  mainWidgetWithHandle vty $ initManager_ $ fmap switchDyn $ getPostBuild
    >>= performEventAsync . (forkWithCallback (tasksView <$> getDefaultFile) <$)
    >>= networkHold (loadingView "default TODO file")
