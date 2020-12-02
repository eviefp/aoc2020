module Day1 where

import Data.List
    (tails)
import Prelude

type Input = [Int]

parse :: IO Input
parse = map read . lines <$> readFile "day1-1.txt"
--      --------   ----      ---------------------
--    [S] -> [I]  S -> [S]         IO String

run :: IO ()
run = parse >>= printSolution

printSolution :: Input -> IO ()
printSolution = print . findSolution2

-- Reason why we're not using [ x * y | x <- xs, y <- xs, x + y == 2020 ]
-- is that it would find '1010' to be a corret answer even if it's not
-- actually duplicated in the input list
-- So we make sure we use distinct items from the list using 'tails'.
findSolution :: Input -> Int
findSolution xs =
    case [ x * y
      | (x:rest) <- tails xs
      , y <- rest
      , x + y == 2020
    ] of
       (x:_) -> x
       []    -> error "No solution found."

findSolution2 :: Input -> Int
findSolution2 xs =
    case [ x * y * z
         | (x:ys) <- tails xs
         , (y:zs) <- tails ys
         , z      <- zs
         , x + y + z == 2020
         ] of
       (x:_) -> x
       []    -> error "No solution found."

-- An example using explicit do syntax for lists.
findSolutionDoSyntax :: Input -> Int
findSolutionDoSyntax xs = case go xs of
       (x:_) -> x
       []    -> error "No solution found."
  where
    go :: [Int] -> [Int]
    go xs' = do
        (x:rest) <- tails xs'
        y <- rest
        if x + y == 2020
            then [x * y]
            else []
