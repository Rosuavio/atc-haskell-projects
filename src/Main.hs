module Main where

import System.IO (hFlush, stdout)
import Data.List (stripPrefix)
import Control.Monad.Trans.Class (lift)
import GHC.List (List)
import Control.Monad.Trans.State (StateT, evalStateT, modify)

data Input
  = Exit
  | Add String
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
    Add task -> do
      lift $ putStrLn $ "Task \"" ++ task ++ "\" added to the to-do list."
      modify (task :)
      loop
    Invaid invalidInput -> do
      lift $ putStrLn $ "Invaid invalidInput: " ++ invalidInput
      lift $ putStrLn "Please provide valid input."
      loop

parseInput :: String -> Input
parseInput "exit" = Exit
parseInput input = case stripPrefix "add " input of
  Just v -> Add v
  Nothing -> Invaid input

getInput :: IO String
getInput = do
  putStr "Enter command: "
  hFlush stdout
  getLine
