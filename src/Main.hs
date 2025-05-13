module Main where

import System.IO (hFlush, stdout)

data Input
  = Exit
  | Invaid String

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  input <- parseInput <$> getInput
  case input of
    Exit -> putStrLn "Goodbye!"
    Invaid invalidInput -> do
      putStrLn $ "Invaid invalidInput: " ++ invalidInput
      putStrLn "Please provide valid input."
      loop

parseInput :: String -> Input
parseInput "exit" = Exit
parseInput input = Invaid input

getInput :: IO String
getInput = do
  putStr "Enter command: "
  hFlush stdout
  getLine
