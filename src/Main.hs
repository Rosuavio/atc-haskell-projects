module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)

data Input
  = Exit
  | Invaid String
  deriving Eq

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  input <- parseInput <$> getInput
  handlInput input
  when (input /= Exit) loop

handlInput :: Input -> IO ()
handlInput Exit = putStrLn "Goodbye!"
handlInput (Invaid input) = do
  putStrLn $ "Invaid input: " ++ input
  putStrLn "Please provide valid input."

parseInput :: String -> Input
parseInput "exit" = Exit
parseInput input = Invaid input

getInput :: IO String
getInput = do
  putStr "Enter command: "
  hFlush stdout
  getLine
