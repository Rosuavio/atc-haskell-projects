module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)

data Input
  = Exit
  | Other String
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
handlInput (Other input) = putStrLn $ "You entered: " ++ input

parseInput :: String -> Input
parseInput "exit" = Exit
parseInput input = Other input

getInput :: IO String
getInput = do
  putStr "Enter command: "
  hFlush stdout
  getLine
