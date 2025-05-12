module Main where

import System.IO (hFlush, stdout)
import GHC.List (List)
import Data.List (stripPrefix)

data Input
  = Exit
  | Add String
  | Invaid String

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  _ <- loop []
  pure ()

loop :: List String ->  IO (List String)
loop s = do
  input <- parseInput <$> getInput
  case input of
    Exit -> do
      putStrLn "Goodbye!"
      pure s
    Add task -> do
      putStrLn $ "Task \"" ++ task ++ "\" added to the to-do list."
      loop $ task : s
    Invaid invalidInput -> do
      putStrLn $ "Invaid invalidInput: " ++ invalidInput
      putStrLn "Please provide valid input."
      loop s

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
