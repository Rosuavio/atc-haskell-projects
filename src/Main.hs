module Main where

import System.IO (hFlush, stdout)
import Data.List (stripPrefix)
import Control.Monad.Trans.Class (lift)
import GHC.List (List)
import Control.Monad.Trans.State (StateT, evalStateT, modify, get)
import Data.Foldable (traverse_)

data Input
  = Exit
  | Add String
  | View
  | Invaid String

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  _ <- evalStateT loop []
  pure ()

loop :: StateT (List String) IO ()
loop = do
  input <- parseInput <$> lift getInput
  case input of
    Exit -> do
      lift $ putStrLn "Goodbye!"
      pure ()
    _ -> case input of
      Add task -> do
        lift $ putStrLn $ "Task \"" ++ task ++ "\" added to the to-do list."
        modify (task :)
        loop
      View -> do
        s <- get
        lift $ if null s
          then putStrLn "No tasks in to-do list"
          else do
            putStrLn "Tasks..."
            traverse_
              (\(i, t) -> putStrLn $ show i ++ ": " ++ t)
              $ zip ([1..] :: List Integer) s
        loop
      Invaid invalidInput -> do
        lift $ putStrLn $ "Invaid invalidInput: " ++ invalidInput
        lift $ putStrLn "Please provide valid input."
        loop

parseInput :: String -> Input
parseInput "exit" = Exit
parseInput "list" = View
parseInput input = case stripPrefix "add " input of
  Just v -> Add v
  Nothing -> Invaid input

getInput :: IO String
getInput = do
  putStr "Enter command: "
  hFlush stdout
  getLine
