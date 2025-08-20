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
import Task (Task (MkTask))

import qualified Data.Dependent.Map as DMap
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NES
import qualified Graphics.Vty as Vty

import Reflex
import Reflex.Network
import Reflex.Vty

import TrackingView
import Util

data Tasks t = Tasks
 { above :: Dynamic t (Seq Task)
 , selected :: Dynamic t Task
 , below :: Dynamic t (Seq Task)
 }

data Change a where
  UpdateTop :: Change (Seq Task -> Seq Task)
  UpdateSelected :: Change (Task -> Task)
  UpdateBottom :: Change (Seq Task -> Seq Task)
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
  => [Task] -> m (Event t ())
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
      Just ts -> do
        rec
          tt <- grout flex $ col $ trackingView tt $ do
            void $ networkView $ traverse_ (line . constant . displayTask) <$> above ts
            trackingTarget <- grout (fixed 1) $ do
              richText selectedConf $ current $ displayTask <$> selected ts
              askRegion
            void $ networkView $ traverse_ (line . constant . displayTask) <$> below ts
            pure trackingTarget
        pure ()
    line $ current $ join $ ffor tasks $ maybe (pure "q - quit") $ \ts ->
      ffor2 (above ts) (below ts) $ \abv blw ->
        sconcat $ NEL.intersperse " | "
          $ ("q - quit" :|)
          $ bool id ("k - move up" :) (not $ Seq.null abv)
          $ bool id ("j - move down" :) (not $ Seq.null blw)
          []
  pure $ select changeEv Quit
  where
    displayTask (MkTask True  d) = "[x] " <> d
    displayTask (MkTask False d) = "[ ] " <> d
    selectedConf = RichTextConfig
      $ constant $ Vty.currentAttr `Vty.withStyle` Vty.underline
    messageConf = RichTextConfig $ constant
      $ Vty.currentAttr `Vty.withStyle` Vty.italic
