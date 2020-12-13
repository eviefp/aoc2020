{-# LANGUAGE TypeApplications #-}
module Day5 where

import           Prelude
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

data BoardingPass = BoardingPass
    { row :: Int
    , column :: Int
    } deriving stock (Show)

-- BFFFBBF RRR
-- ^^ 7    ^ 3
-- F = 0, B = 1
-- L = 0, R = 1

parse :: IO [BoardingPass]
parse = map go . lines <$> readFile "day5-1.txt"
  where
    go :: String -> BoardingPass
    go input = makeBoardingPass $ span (`elem` ['F', 'B']) input

    replaceRow :: Char -> Char
    replaceRow = \case
      'F' -> '0'
      'B' -> '1'
      c   -> error $ "Unexpected row " <> show c

    replaceColumn :: Char -> Char
    replaceColumn = \case
      'L' -> '0'
      'R' -> '1'
      c   -> error $ "Unexpected row " <> show c

    makeBoardingPass :: ([Char], [Char]) -> BoardingPass
    makeBoardingPass (srow, scolumn) =
      let
          bcolumn = map replaceColumn scolumn
          brow    = map replaceRow    srow
      in case
            (,)
                <$> M.parseMaybe @Void L.binary bcolumn
                <*> M.parseMaybe @Void L.binary brow of
            Just (column, row) -> BoardingPass {..}
            Nothing -> error "Invalid boarding pass"

run :: IO ()
run = parse >>= print . solution2

solution1 :: [BoardingPass] -> Int
solution1 bp = maximum $ seatId <$> bp

seatId :: BoardingPass -> Int
seatId BoardingPass {..} = row * 8 + column

solution2 :: [BoardingPass] -> Int
solution2 bp =
  let seats = seatId <$> bp
   in head $ [ x + 1
             | x <- seats
             , x + 1 `notElem` seats
             , x + 2 `elem` seats
             ]
