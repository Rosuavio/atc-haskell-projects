module Main
  ( main
  ) where

import Graphics.Vty.CrossPlatform (mkVty)
import System.OsPath (encodeUtf)

import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Options.Applicative
import Reflex
import Reflex.Network
import Reflex.Vty

import TasksView
import Ui
import Util



main :: IO ()
main = do
  (placeHolder, getPath) <- execParser $ info
    ((<*>) helper $ optional $ strArgument $ metavar "TARGET"
    <> help "TODO file to use (Defaults to \"$XDG_STATE_HOME/todo\")")
    ( fullDesc
    <> progDesc "A simple interactive program for managing markdown TODO lists"
    <> header "TODO list manager")
    `ffor` \case
    Just fileName -> (fileName, encodeUtf fileName)
    Nothing -> ("default TODO file", getDefaultFile)
  vty <- mkVty Vty.defaultConfig
  mainWidgetWithHandle vty $ initManager_ $ fmap switchDyn $ getPostBuild
    >>= performEventAsync . (forkWithCallback (tasksView <$> getPath) <$)
    >>= networkHold (loadingView $ constant $ T.pack placeHolder)
