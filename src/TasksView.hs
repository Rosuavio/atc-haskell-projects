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
import Data.Functor (void, (<&>))
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
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import qualified Task as Tsk

import Reflex
import Reflex.Network
import Reflex.Vty

import TrackingView
import Util

data Direction
  = Up
  | Down

data Mode
  = Normal
  | Inserting Direction

data Tasks t = Tasks
 { above :: Dynamic t (Seq Task)
 , selected :: Dynamic t Task
 , below :: Dynamic t (Seq Task)
 }

data Change a where
  UpdateTop :: Change (Seq Task -> Seq Task)
  UpdateSelected :: Change (Task -> Task)
  UpdateBottom :: Change (Seq Task -> Seq Task)
  ClearLines :: Change ()
  CreateLines :: Change Task
  ChangeMode :: Change Mode
  UpdateEditTask :: Change (Task -> Task)
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
    initialTasks <- case L.uncons fileLines of
      Nothing -> pure Nothing
      Just (t, ts) -> fmap Just $ Tasks
        <$> (foldDyn ($) Seq.Empty $ select changeEv UpdateTop)
        <*> (foldDyn ($) t $ select changeEv UpdateSelected)
        <*> (foldDyn ($) (Seq.fromList ts) $ select changeEv UpdateBottom)
    tasks <- holdDyn initialTasks $ leftmost
      [ Nothing <$ select changeEv ClearLines
      , flip pushAlways (select changeEv CreateLines) $ \newLine -> fmap Just $
        Tasks
        <$> (foldDyn ($) Seq.Empty $ select changeEv UpdateTop)
        <*> (foldDyn ($) newLine $ select changeEv UpdateSelected)
        <*> (foldDyn ($) Seq.Empty $ select changeEv UpdateBottom)
      ]
    mode <- holdDyn Normal $ select changeEv ChangeMode
    -- TODO: Something feels wired about having this around in all contexts I
    -- tried to put this in the Inserting mode, but it would mean updates to the
    -- editTask would update the mode and trigger bigger
    -- rebuilds, it then felt like it made sense to make it a Dynamic in the
    -- Inserting mode, but then that hkd did not work in ChangeMode
    editTask <- foldDyn ($) (MkTask False "") $ select changeEv UpdateEditTask
    changeEv <- fmap fan $ ffor input $ push $ \k -> do
      m <- sample $ current mode
      case (m, k) of
        (Normal, Vty.EvKey (Vty.KChar 'k') []) -> (>>=) (sample $ current tasks)
          . maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ above ts)
          . NES.withNonEmpty (pure Nothing) $ \(top :||> newSel) -> do
            oldSel <- sample $ current $ selected ts
            pure $ Just $ DMap.fromList
              [ UpdateTop ==> const top
              , UpdateSelected ==> const newSel
              , UpdateBottom ==> (oldSel :<|)
              ]
        (Normal, Vty.EvKey (Vty.KChar 'j') []) -> (>>=) (sample $ current tasks)
          . maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ below ts)
          . NES.withNonEmpty (pure Nothing) $ \(newSel :<|| bot) -> do
            oldSel <- sample $ current $ selected ts
            pure $ Just $ DMap.fromList
              [ UpdateTop ==> (:|> oldSel)
              , UpdateSelected ==> const newSel
              , UpdateBottom ==> const bot
              ]
        (Normal, Vty.EvKey (Vty.KChar 'I') []) ->
          pure $ Just $ DMap.fromList
            [ ChangeMode ==> Inserting Up
            , UpdateEditTask ==> const (MkTask False "")
            ]
        (Normal, Vty.EvKey (Vty.KChar 'i') []) ->
          pure $ Just $ DMap.fromList
            [ ChangeMode ==> Inserting Down
            , UpdateEditTask ==> const (MkTask False "")
            ]
        (Normal, Vty.EvKey (Vty.KChar 'd') []) -> (>>=) (sample $ current tasks)
          $ maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ below ts)
          $ \case
            (newSel :<| bot) -> pure $ Just $ DMap.fromList
              [ UpdateSelected ==> const newSel
              , UpdateBottom ==> const bot
              ]
            Seq.Empty -> sample $ current $ above ts <&> Just . \case
              (top :|> newSel) -> DMap.fromList
                [ UpdateSelected ==> const newSel
                , UpdateTop ==> const top
                ]
              Seq.Empty -> DMap.singleton ClearLines $ Identity ()
        (Normal, Vty.EvKey (Vty.KChar 'D') []) -> (>>=) (sample $ current tasks)
          $ maybe (pure Nothing) $ \ts -> (>>=) (sample $ current $ above ts)
          $ \case
            (top :|> newSel) -> pure $ Just $ DMap.fromList
              [ UpdateSelected ==> const newSel
              , UpdateTop ==> const top
              ]
            Seq.Empty -> sample $ current $ below ts <&> Just . \case
              (newSel :<| bot) -> DMap.fromList
                [ UpdateSelected ==> const newSel
                , UpdateBottom ==> const bot
                ]
              Seq.Empty -> DMap.singleton ClearLines $ Identity ()
        (Normal, Vty.EvKey (Vty.KChar 'q') []) ->
          pure $ Just $ DMap.singleton Quit (Identity ())
        (Inserting _, Vty.EvKey (Vty.KEsc) []) ->
          pure $ Just $ DMap.fromList
            [ ChangeMode ==> Normal
            , UpdateEditTask ==> const (MkTask False "")
            ]
        (Inserting d, Vty.EvKey (Vty.KEnter) []) -> do
          (sample $ current tasks) >>= \case
            Nothing -> do
              newSel <- sample $ current editTask
              pure $ Just $ DMap.fromList
                [ ChangeMode ==> Normal
                , UpdateEditTask ==> const (MkTask False "")
                , CreateLines ==> newSel
                ]
            Just ts -> do
              oldSel <- sample $ current $ selected ts
              newSel <- sample $ current editTask
              pure $ Just $ DMap.fromList
                [ ChangeMode ==> Normal
                , UpdateEditTask ==> const (MkTask False "")
                , UpdateSelected ==> const newSel
                , case d of
                    Up -> UpdateBottom ==> (oldSel :<|)
                    Down -> UpdateTop ==> (:|> oldSel)
                ]
        (Inserting _, Vty.EvKey kk []) -> do
          pure $ case kk of
            Vty.KChar c -> Just $ DMap.singleton UpdateEditTask $ Identity
              (\t@(MkTask _ d) -> t{ Tsk.description = T.snoc d c })
            Vty.KBS -> Just $ DMap.singleton UpdateEditTask $ Identity
              (\t@(MkTask _ d) ->
                t{ Tsk.description = maybe "" fst $ T.unsnoc d })
            _ -> Nothing
        _ -> pure Nothing
  grout flex $ col $ do
    void $ networkView $ ffor tasks $ \case
      Nothing -> do
        void $ networkView $ ffor mode $ \case
          Normal -> grout (fixed 1) $ richText messageConf "File is empty"
          Inserting _ -> grout (fixed 1)
            $ richText selectedConf $ current $ displayTask <$> editTask
        grout flex blank
      Just ts -> do
        rec
          tackingTarget <- grout flex $ col $ trackingView tackingTarget $ do
            r <- askRegion
            void $ networkView $ traverse_ (line . constant . displayTask) <$> above ts
            trackingTarget <- fmap join $ (=<<) (holdDyn r) $ networkView
              $ ffor mode $ \case
              Normal -> grout (fixed 1) $ do
                richText selectedConf $ current $ displayTask <$> selected ts
                askRegion
              Inserting Up -> do
                newTaskRegion <- grout (fixed 1) $ do
                  richText selectedConf $ current $ displayTask <$> editTask
                  askRegion
                line $ current $ displayTask <$> selected ts
                pure newTaskRegion
              Inserting Down -> do
                line $ current $ displayTask <$> selected ts
                newTaskRegion <- grout (fixed 1) $ do
                  richText selectedConf $ current $ displayTask <$> editTask
                  askRegion
                pure newTaskRegion
            void $ networkView $ traverse_ (line . constant . displayTask) <$> below ts
            pure trackingTarget
        pure ()
    line $ current $ join $ ffor mode $ \case
      Normal -> fmap (sconcat . NEL.intersperse " | ")
        $ (<*>) (pure ("Mode: Normal | q - quit | i/I - insert below/above | d/D - delete & move down/up" :|))
        $ join $ ffor tasks $ maybe (pure []) $ \ts ->
          ffor2 (above ts) (below ts) $ \abv blw ->
            bool id ("j - move down" :) (not $ Seq.null blw)
            $ bool id ("k - move up" :) (not $ Seq.null abv)
            []
      Inserting _ -> pure $ "Mode: Inserting | Esc - cancel | Enter - submit"
  pure $ select changeEv Quit
  where
    displayTask (MkTask True  d) = "[x] " <> d
    displayTask (MkTask False d) = "[ ] " <> d
    selectedConf = RichTextConfig
      $ constant $ Vty.currentAttr `Vty.withStyle` Vty.underline
    messageConf = RichTextConfig $ constant
      $ Vty.currentAttr `Vty.withStyle` Vty.italic
