{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module TasksView
  ( tasksView
  ) where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Dependent.Sum ((==>))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (Identity))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup (sconcat))
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence.NonEmpty (NESeq ((:<||), (:||>)))

import qualified Data.Dependent.Map as DMap
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Reflex
import Reflex.Network
import Reflex.Vty

import Util

data Tasks t = Tasks
 { above :: Dynamic t (Seq T.Text)
 , selected :: Dynamic t T.Text
 , below :: Dynamic t (Seq T.Text)
 }

data Change a where
  UpdateTop :: Change (Seq T.Text -> Seq T.Text)
  UpdateSelected :: Change (T.Text -> T.Text)
  UpdateBottom :: Change (Seq T.Text -> Seq T.Text)
  Quit :: Change ()

deriveGEq ''Change
deriveGCompare ''Change

tasksView ::
  ( Monad m
  , Reflex t
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Adjustable t m
  , NotReady t m
  , HasLayout t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocusReader t m
  , HasTheme t m
  )
  => [T.Text] -> m (Event t ())
tasksView fileLines = do
  rec
    tasks <- constDyn <$> case L.uncons fileLines of
      Nothing -> pure Nothing
      Just (t, ts) -> fmap Just $ Tasks
        <$> (foldDyn ($) Seq.Empty $ select changeEv UpdateTop)
        <*> (foldDyn ($) t $ select changeEv UpdateSelected)
        <*> (foldDyn ($) (Seq.fromList ts) $ select changeEv UpdateBottom)
    changeEv <- fmap fan $ ffor input $ push $ \case
      (Vty.EvKey (Vty.KChar 'k') []) -> (>>=) (sample $ current tasks)
        . maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ above ts)
        . NES.withNonEmpty (pure Nothing) $ \(top :||> newSel) -> do
          oldSel <- sample $ current $ selected ts
          pure $ Just $ DMap.fromList
            [ UpdateTop ==> const top
            , UpdateSelected ==> const newSel
            , UpdateBottom ==> (oldSel :<|)
            ]
      (Vty.EvKey (Vty.KChar 'j') []) -> (>>=) (sample $ current tasks)
        . maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ below ts)
        . NES.withNonEmpty (pure Nothing) $ \(newSel :<|| bot) -> do
          oldSel <- sample $ current $ selected ts
          pure $ Just $ DMap.fromList
            [ UpdateTop ==> (:|> oldSel)
            , UpdateSelected ==> const newSel
            , UpdateBottom ==> const bot
            ]
      (Vty.EvKey (Vty.KChar 'q') []) ->
        pure $ Just $ DMap.singleton Quit (Identity ())
      _ -> pure Nothing
  grout flex $ col $ do
    void $ networkView $ ffor tasks $ \case
      Nothing -> do
        grout (fixed 1) $ richText messageConf "File is empty"
        grout flex blank
      Just ts -> grout flex $ col $ do
        void $ networkView $ traverse_ (line . constant) <$> above ts
        grout (fixed 1) $ richText selectedConf $ current $ selected ts
        void $ networkView $ traverse_ (line . constant) <$> below ts
    line $ current $ join $ ffor tasks $ maybe (pure "q - quit") $ \ts ->
      ffor2 (above ts) (below ts) $ \abv blw ->
        sconcat $ NEL.intersperse " | "
          $ ("q - quit" :|)
          $ bool id ("k - move up" :) (not $ Seq.null abv)
          $ bool id ("j - move down" :) (not $ Seq.null blw)
          []
  pure $ select changeEv Quit
  where
    selectedConf = RichTextConfig
      $ constant $ Vty.currentAttr `Vty.withStyle` Vty.underline
    messageConf = RichTextConfig $ constant
      $ Vty.currentAttr `Vty.withStyle` Vty.italic
