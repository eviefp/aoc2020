{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day7 where

import Prelude
import qualified Data.Map.Strict as M
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import qualified Data.Tuple as T

type BagName = String

data Bag = Bag
    { name :: BagName
    , contents :: M.Map BagName Int
    } deriving stock Show

canItCarryAGoldBag :: M.Map BagName Bag -> Bag -> Bool
canItCarryAGoldBag bags Bag {..} =
    name == "shiny gold"
        || any
            (canItCarryAGoldBag bags)
            (catMaybes $ M.lookup <$> M.keys contents <*> pure bags)

bagsWhichCanContainAGoldBag :: M.Map BagName Bag -> Int
bagsWhichCanContainAGoldBag bags =
    M.size (M.filter (canItCarryAGoldBag bags) bags) - 1

countBags :: M.Map BagName Bag -> Bag -> Int
countBags bags Bag {..} =
    1 + sum (calculate <$> go contents)

  where
    go :: M.Map BagName Int -> [(Int, Bag)]
    go c = catMaybes $ traverse (`M.lookup` bags) . T.swap <$> M.toList c

    calculate :: (Int, Bag) -> Int
    calculate (num, b) = num * countBags bags b

solution2 :: M.Map BagName Bag -> Int
solution2 bags =
    go $ M.lookup "shiny gold" bags
  where
    go :: Maybe Bag -> Int
    go = \case
            Just bag -> countBags bags bag - 1
            Nothing -> 0

parse :: IO (M.Map BagName Bag)
parse = M.fromList . map (toPair . go) . lines <$> readFile "day7-1.txt"
  where
    toPair :: Bag -> (BagName, Bag)
    toPair bag@Bag {..} = (name, bag)

    go :: String -> Bag
    go input =
      let
           w = words input
           (name', rest') = span (/= "bags") w
           name = unwords name'
           rest = drop 2 rest'
      in if head rest == "no"
            then Bag { name, contents = M.empty }
            else Bag { name, contents = M.fromList $ findContents rest }

    findContents :: [String] -> [(BagName, Int)]
    findContents rest =
      let
          (num':bag', _:rest') = break (isPrefixOf "bag") rest
          num = read @Int num'
          bagName = unwords bag'
      in if null rest'
            then [(bagName, num)]
            else (bagName, num) : findContents rest'


