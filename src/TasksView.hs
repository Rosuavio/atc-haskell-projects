{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module TasksView
  ( tasksView
  ) where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
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
import System.OsPath (OsPath, decodeUtf)
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
import Ui
import Util

data Direction
  = Up
  | Down

data Mode
  = Normal
  | Inserting Direction
  | Editing

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
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  )
  => OsPath -> m (Event t ())
tasksView path = do
  pb <- getPostBuild
  fileName <- performEventAsync
    (forkWithCallback (T.pack <$> decodeUtf path) <$ pb)
    >>= hold ""
  getTasksRezEv <- performEventAsync (forkWithCallback (getTasks path) <$ pb)
  fmap switchDyn $ networkHold (loadingView fileName) $ ffor getTasksRezEv
    $ \getTasksRez -> do
    rec
      tasks <- holdMaybeTasks (fromRight [] getTasksRez)
        (select changeEv UpdateTop)
        (select changeEv UpdateSelected)
        (select changeEv UpdateBottom)
        (select changeEv CreateLines)
        (select changeEv ClearLines)
      mode <- holdDyn Normal $ select changeEv ChangeMode
      -- TODO: Something feels wired about having this around in all contexts I
      -- tried to put this in the Inserting mode, but it would mean updates to the
      -- editTask would update the mode and trigger bigger
      -- rebuilds, it then felt like it made sense to make it a Dynamic in the
      -- Inserting mode, but then that hkd did not work in ChangeMode
      editTask <- foldDyn ($) Tsk.def $ select changeEv UpdateEditTask
      changeEv <- fmap fan $ ffor input $ push $ \i ->
        (sample $ current mode) >>= \case
          Normal -> handleNormal i tasks
          Inserting d -> handleInserting i d tasks editTask
          Editing -> handleEditing i editTask
    grout flex $ col $ do
      void $ networkView $ ffor tasks
        $ maybe (emptyView mode editTask) $ nonEmptyView mode editTask
      line $ current $ menuView mode tasks
    pure $ select changeEv Quit

holdMaybeTasks ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  )
  => [Task]
  -> Event t (Seq Task -> Seq Task)
  -> Event t (Task -> Task)
  -> Event t (Seq Task -> Seq Task)
  -> Event t Task
  -> Event t b
  -> m (Dynamic t (Maybe (Tasks t)))
holdMaybeTasks
  initialTasks
  updateTop
  updateSelected
  updateBottom
  createLines
  clearLines
  = do
  i <- case L.uncons initialTasks of
    Nothing -> pure Nothing
    Just (t, ts) -> fmap Just $
      holdTasks t (Seq.fromList ts) updateTop updateSelected updateBottom
  holdDyn i $ leftmost
    [ Nothing <$ clearLines
    , flip pushAlways createLines $ \newLine -> fmap Just $
      holdTasks newLine Seq.Empty updateTop updateSelected updateBottom
    ]

holdTasks ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  )
  => Task
  -> Seq Task
  -> Event t (Seq Task -> Seq Task)
  -> Event t (Task -> Task)
  -> Event t (Seq Task -> Seq Task)
  -> m (Tasks t)
holdTasks x xs updateTop updateSelected updateBottom = Tasks
  <$> (foldDyn ($) Seq.Empty updateTop)
  <*> (foldDyn ($) x updateSelected)
  <*> (foldDyn ($) xs updateBottom)

nonEmptyView ::
  ( HasFocusReader t m
  , HasDisplayRegion t m
  , HasImageWriter t m
  , HasInput t m
  , HasLayout t m
  , HasTheme t m
  , PostBuild t m
  , Adjustable t m
  , NotReady t m
  , MonadHold t m
  , MonadFix m
  )
  => Dynamic t Mode
  -> Dynamic t Task
  -> Tasks t
  -> m ()
nonEmptyView mode editTask ts = mdo
  tackingTarget <- grout flex $ col $ trackingView tackingTarget $ do
    displayTasks $ above ts
    gotTrackingTarget <- networkView $ ffor mode $ \case
      Normal -> grout (fixed 1) $ do
        selectedTaskView $ selected ts
        askRegion
      Inserting Up -> do
        newTaskRegion <- grout (fixed 1) $ do
          selectedTaskView editTask
          askRegion
        line $ current $ displayTask <$> selected ts
        pure newTaskRegion
      Inserting Down -> do
        line $ current $ displayTask <$> selected ts
        newTaskRegion <- grout (fixed 1) $ do
          selectedTaskView editTask
          askRegion
        pure newTaskRegion
      Editing -> grout (fixed 1) $ do
        selectedTaskView editTask
        askRegion
    displayTasks $ below ts
    fmap join $ askRegion
      >>= flip holdDyn gotTrackingTarget
  pure ()
  where
    displayTasks = void . networkView
      . fmap (traverse_ $ line . constant . displayTask)

emptyView ::
  ( PostBuild t m
  , Adjustable t m
  , NotReady t m
  , HasLayout t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocusReader t m
  , HasTheme t m
  , MonadHold t m
  , MonadFix m
  )
  => Dynamic t Mode
  -> Dynamic t Task
  -> m ()
emptyView mode editTask = do
  void $ grout (fixed 1) $ networkView $ ffor mode $ \case
    Normal -> richText messageConf "No tasks"
    Inserting _ -> selectedTaskView $ editTask
    -- Should not be possible
    Editing -> selectedTaskView $ editTask
  grout flex blank
  where
    messageConf = RichTextConfig $ constant
      $ Vty.currentAttr `Vty.withStyle` Vty.italic

selectedTaskView ::
  ( HasDisplayRegion t m
  , HasImageWriter t m
  , HasTheme t m
  , MonadFix m
  , MonadHold t m
  , HasLayout t m
  , HasInput t m
  , HasFocusReader t m
  )
  => Dynamic t Task -> m ()
selectedTaskView = grout (fixed 1)
  . richText conf . current . fmap displayTask
  where
    conf = RichTextConfig $ constant
      $ Vty.currentAttr `Vty.withStyle` Vty.underline

displayTask :: Task -> T.Text
displayTask (MkTask True  d) = "[x] " <> d
displayTask (MkTask False d) = "[ ] " <> d

menuView :: Reflex t
  => Dynamic t Mode
  -> Dynamic t (Maybe (Tasks t))
  -> Dynamic t T.Text
menuView mode tasks = join $ ffor mode $ \case
  Normal -> fmap (sconcat . NEL.intersperse " | ")
    $ (<*>) (pure ("Mode: Normal | q - quit | i/I - insert below/above | d/D - delete & move down/up" :|))
    $ join $ ffor tasks $ maybe (pure []) $ \ts ->
      ffor3 (selected ts) (above ts) (below ts) $ \sel abv blw ->
        ("space - mark " <> case Tsk.completed sel of
          True -> "incomplete"
          False -> "complete"
        :)
        $ bool id ("j - move down" :) (not $ Seq.null blw)
        $ bool id ("k - move up" :) (not $ Seq.null abv)
        []
  Inserting _ -> pure $ "Mode: Inserting | Esc - cancel | Enter - submit"
  Editing -> pure $ "Mode: Editing | Esc - cancel | Enter - submit"

handleNormal ::
  ( Reflex t
  , MonadSample t m
  )
  => Vty.Event
  -> Dynamic t (Maybe (Tasks t))
  -> m (Maybe (DMap.DMap Change Identity))
handleNormal (Vty.EvKey (Vty.KChar 'k') []) dynTs =
  (>>=) (sample $ current dynTs) $ maybe (pure Nothing)
  $ \ts -> (>>=) (sample $ current $ above ts) . NES.withNonEmpty (pure Nothing)
  $ \(top :||> newSel) -> do
    oldSel <- sample $ current $ selected ts
    pure $ Just $ DMap.fromList
      [ UpdateTop ==> const top
      , UpdateSelected ==> const newSel
      , UpdateBottom ==> (oldSel :<|)
      ]
handleNormal (Vty.EvKey (Vty.KChar 'j') []) dynTs =
  (>>=) (sample $ current dynTs) $ maybe (pure Nothing)
  $ \ts -> (>>=) (sample $ current $ below ts) . NES.withNonEmpty (pure Nothing)
  $ \(newSel :<|| bot) -> do
    oldSel <- sample $ current $ selected ts
    pure $ Just $ DMap.fromList
      [ UpdateTop ==> (:|> oldSel)
      , UpdateSelected ==> const newSel
      , UpdateBottom ==> const bot
      ]
handleNormal (Vty.EvKey (Vty.KChar 'I') []) _ = pure $ Just
  $ DMap.fromList
  [ ChangeMode ==> Inserting Up
  , UpdateEditTask ==> const Tsk.def
  ]
handleNormal (Vty.EvKey (Vty.KChar 'i') []) _ = pure $ Just
  $ DMap.fromList
  [ ChangeMode ==> Inserting Down
  , UpdateEditTask ==> const Tsk.def
  ]
handleNormal (Vty.EvKey (Vty.KChar 'd') []) dynTs =
  (>>=) (sample $ current dynTs) $ maybe (pure Nothing)
  $ \ts -> sample (current $ below ts) >>= \case
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
handleNormal (Vty.EvKey (Vty.KChar ' ') []) _  = pure $ Just
  $ DMap.singleton UpdateSelected $ Identity $ Tsk.toggleComplete
handleNormal (Vty.EvKey (Vty.KChar 'D') []) dynTs =
  (>>=) (sample $ current dynTs) $ maybe (pure Nothing)
  $ \ts -> sample (current $ above ts) >>= \case
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
handleNormal (Vty.EvKey (Vty.KChar 'e') []) dynTs =
  (>>=) (sample $ current dynTs) $ maybe (pure Nothing)
  $ \ts -> do
    sel <- (sample $ current $ selected ts)
    pure $ Just $ DMap.fromList
      [ ChangeMode ==> Editing
      , UpdateEditTask ==> const sel
      ]
handleNormal (Vty.EvKey (Vty.KChar 'q') []) _ = pure $ Just
  $ DMap.singleton Quit (Identity ())
handleNormal _ _ = pure Nothing

handleInserting ::
  ( Reflex t
  , MonadSample t m
  )
  => Vty.Event
  -> Direction
  -> Dynamic t (Maybe (Tasks t))
  -> Dynamic t Task
  -> m (Maybe (DMap.DMap Change Identity))
-- BUG: Shift+Enter Seems to cancel (like Esc)
handleInserting (Vty.EvKey (Vty.KEnter) []) d tasks editTask = do
  newSel <- sample $ current editTask
  (sample $ current tasks) >>= \case
    Nothing -> pure $ Just $ DMap.insert CreateLines (Identity newSel)
      resetChangSet
    Just ts -> do
      oldSel <- sample $ current $ selected ts
      pure $ Just $ DMap.union resetChangSet $ DMap.fromList
        [ UpdateSelected ==> const newSel
        , case d of
            Up -> UpdateBottom ==> (oldSel :<|)
            Down -> UpdateTop ==> (:|> oldSel)
        ]
handleInserting (Vty.EvKey (Vty.KEsc) []) _ _ _ = pure $ Just $ resetChangSet
handleInserting (Vty.EvKey kk []) _ _ _ = pure $ updateTaskForKey kk
handleInserting _ _ _ _ = pure Nothing

handleEditing ::
  ( Reflex t
  , MonadSample t m
  )
  => Vty.Event
  -> Dynamic t Task
  -> m (Maybe (DMap.DMap Change Identity))
-- BUG: Shift+Enter Seems to cancel (like Esc)
handleEditing (Vty.EvKey (Vty.KEnter) []) editTask = do
  newSel <- sample $ current editTask
  pure $ Just $ DMap.insert UpdateSelected (Identity $ const newSel)
    resetChangSet
handleEditing (Vty.EvKey (Vty.KEsc) []) _ = pure $ Just $ resetChangSet
handleEditing (Vty.EvKey kk []) _ = pure $ updateTaskForKey kk
handleEditing _ _ = pure Nothing

resetChangSet :: DMap.DMap Change Identity
resetChangSet = DMap.fromList
  [ ChangeMode ==> Normal
  , UpdateEditTask ==> const Tsk.def
  ]

updateTaskForKey :: Vty.Key -> Maybe (DMap.DMap Change Identity)
updateTaskForKey kk = case kk of
  Vty.KChar c -> Just $ DMap.singleton UpdateEditTask $ Identity $ Tsk.append c
  Vty.KBS -> Just $ DMap.singleton UpdateEditTask $ Identity $ Tsk.delete
  _ -> Nothing
