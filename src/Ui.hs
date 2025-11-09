module Ui
  ( loadingView
  , quitablePrompt
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Functor (void)
import Data.Text (Text)

import qualified Graphics.Vty as Vty

import Reflex.Vty
import Util

loadingView ::
  ( MonadFix m
  , HasTheme t m
  , HasLayout t m
  , HasFocusReader t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasInput t m
  , MonadHold t m
  )
  => Behavior t Text -> m (Event t ())
loadingView filename = quitablePrompt $ "Loading " <> filename <> "..."

quitablePrompt ::
  ( MonadFix m
  , HasTheme t m
  , HasLayout t m
  , HasFocusReader t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasInput t m
  , MonadHold t m
  )
  => Behavior t Text
  -> m (Event t ())
quitablePrompt msg = grout flex $ col $ do
  line msg
  grout flex blank
  line "Press Ctrl+c to quit."
  void <$> keyCombo (Vty.KChar 'c', [Vty.MCtrl])
