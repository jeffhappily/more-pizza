module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO

import MorePizza

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- readFile filePath

  case solve content of
    Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 1)
    Right a -> print a
  
