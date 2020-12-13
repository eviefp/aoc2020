module Day3 where

import Prelude

data Square = Open | Tree

charToSquare :: Char -> Square
charToSquare = \case
    '.' -> Open
    '#' -> Tree
    c   -> error $ "Bad input found: " <> show c

parse :: IO [[Square]]
parse = do
    contents <- readFile "day3-1.txt"
    let m = lines contents
    pure $ map (map charToSquare) m

countTreesWhileMoving :: Int -> Int -> [[Square]] -> Int
countTreesWhileMoving right down = \case
     xs@((s:_):_) -> countSquare s + countTreesWhileMoving right down (move xs)
     _ -> 0
  where
    move :: [[Square]] -> [[Square]]
    move xs = map (drop right) $ drop down xs

    countSquare :: Square -> Int
    countSquare = \case
        Open -> 0
        Tree -> 1

solution1 :: IO Int
solution1 = countTreesWhileMoving 3 1 . map cycle <$> parse

solution2 :: IO Int
solution2 = do
    terrain <- map cycle <$> parse
    let
        attempts = [ countTreesWhileMoving 1 1 terrain
                   , countTreesWhileMoving 3 1 terrain
                   , countTreesWhileMoving 5 1 terrain
                   , countTreesWhileMoving 7 1 terrain
                   , countTreesWhileMoving 1 2 terrain
                   ]
    pure $ product attempts
