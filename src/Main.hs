module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  when isLooping loop

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True
