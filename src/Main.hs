module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)

data Input
  = Exit
  | Other
  deriving Eq

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  handledInput <- handleInput input
  when (handledInput /= Exit) loop

handleInput :: String -> IO Input
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure Exit
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure Other
