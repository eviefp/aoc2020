module Day6 where

import Prelude
import Data.Set (Set)
import qualified Data.Set as S

doubleLines :: String -> [[String]]
doubleLines i = go (lines i)
  where
    go :: [String] -> [[String]]
    go input =
      case span (/= "") input of
        (gn, []:rest) -> gn : go rest
        (gn, _)       -> [gn]

parse :: IO [[Set Char]]
parse = (map . map) S.fromList . doubleLines <$> readFile "day6-1.txt"

solution1 :: [[Set Char]] -> Int
solution1 = sum . map (S.size . S.unions)

solution2 :: [[Set Char]] -> Int
solution2 = sum . map (S.size . foldl1 S.intersection)
