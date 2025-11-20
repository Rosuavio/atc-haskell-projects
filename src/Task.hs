module Task
  ( Task (MkTask, completed, description)
  , append
  , def
  , delete
  , parseDoc
  , tasksToCommonmark
  , toggleComplete
  ) where

import Data.Foldable (Foldable (toList))
import Data.Text (Text)

import qualified Data.List as L
import qualified Data.Text as T

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

tasksToCommonmark :: Traversable f => f Task -> Maybe Text
tasksToCommonmark t =
  traverse (\(MkTask s tt) -> Node Nothing (TASKLIST s) <$> toDocNodes tt) t
  >>= Just . nodeToCommonmark [] Nothing . Node Nothing DOCUMENT . L.singleton
    . Node Nothing (LIST $ ListAttributes BULLET_LIST True 0 PERIOD_DELIM) . toList

toDocNodes :: Text -> Maybe [Node]
toDocNodes t = case commonmarkToNode [] [extTaskList] t of
  (Node _ DOCUMENT c) -> Just c
  _ -> Nothing

fromTaskListItem :: Node -> Maybe Task
fromTaskListItem (Node _ (TASKLIST p) c) = Just $ MkTask
  { completed = p
  , description = T.dropEnd 1 $ nodeToCommonmark [] Nothing $ Node Nothing DOCUMENT c
  }
fromTaskListItem _ = Nothing

append :: Char -> Task -> Task
append c t@(MkTask _ d) = t{ description = T.snoc d c }

delete :: Task -> Task
delete t@(MkTask _ d) = t{ description = maybe "" fst $ T.unsnoc d }

toggleComplete :: Task -> Task
toggleComplete t@(MkTask c _) = t{ completed = not c }

def :: Task
def = (MkTask False "")
