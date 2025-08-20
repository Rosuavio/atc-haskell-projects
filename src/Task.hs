module Task
  ( Task (MkTask, completed, description)
  , parseDoc
  ) where

import Data.Text (Text)

import CMarkGFM

data Task = MkTask
  { completed :: Bool
  , description :: Text
  }

parseDoc :: Text -> Maybe [Task]
parseDoc t = case commonmarkToNode [] [extTaskList] t of
  (Node _ DOCUMENT []) -> Just []
  (Node _ DOCUMENT [Node _ (LIST _) c]) -> traverse fromTaskListItem c
  _ -> Nothing

fromTaskListItem :: Node -> Maybe Task
fromTaskListItem (Node _ (TASKLIST p) c) = Just $ MkTask
  { completed = p
  , description = nodeToCommonmark [] Nothing $ Node Nothing DOCUMENT c
  }
fromTaskListItem _ = Nothing
