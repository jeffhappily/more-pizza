{-# LANGUAGE RecordWildCards #-}
module MorePizza 
  ( solve
  ) where

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
  , score :: Integer
  } deriving (Eq, Show)

instance Ord Output where
  compare a b = compare (score a) (score b)

solve :: String -> Either String Output
solve content = case parseString inputParser mempty content of
  Failure err -> Left $ show err
  Success a -> Right $ calculate a

calculate :: Input -> Output
calculate Input{..} = go pizzas maxSlices 0 (Output 0 [] 0)
  where
    go [] _ _ out = out
    go (x:xs) max' index out@(Output{..}) = 
      if (score + x) > maxSlices 
        then out 
        else max with without
      where
        with = 
          go xs (max' - x) (index + 1) 
            out
            { score=x + score
            , pizzaAmount=pizzaAmount+1
            , oPizzas=oPizzas ++ [index]
            }
        without = go xs max' (index + 1) out