{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment (getArgs)
import System.IO
import Text.Parser.Combinators
import Text.Trifecta

data Input =
  Input
  { maxSlices :: Integer
  , pizzaCount :: Integer
  , pizzas :: [Integer]
  } deriving Show

inputParser :: Parser Input
inputParser = do
  maxSlices <- decimal
  char ' '
  pizzaCount <- decimal
  char '\n'
  pizzas <- decimal `sepBy` char ' '

  return $ Input{..}

data Output =
  Output
  { pizzaAmount :: Integer
  , oPizzas :: [Integer]
  } deriving Show

solve :: Input -> Output
solve = undefined

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- readFile filePath

  case parseString inputParser mempty content of
    Failure err -> print err
    Success a -> print $ solve a
