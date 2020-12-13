module Day4 where

import           Data.Void
    (Void)
import           Prelude
import qualified Text.Megaparsec            as MP
import qualified Data.Map                   as M
import qualified Text.Megaparsec.Char       as C

type Parser = MP.Parsec Void String
type Passport = M.Map String String

parseKeyValue :: Parser (String, String)
parseKeyValue = do
    left <- MP.some (MP.noneOf [':', ' ', '\n'])
    _ <- C.char ':'
    right <- MP.some (MP.noneOf [' ', '\n' ])
    _ <- MP.optional C.spaceChar
    pure (left, right)

parsePassport :: Parser Passport
parsePassport = do
    kv <- MP.some parseKeyValue
    _ <- C.space
    pure $ M.fromList kv

parser :: Parser [Passport]
parser = MP.many parsePassport

run :: IO ()
run = do
    rawInput <- readFile "day4-1.txt"
    case MP.parseMaybe parser rawInput of
        Nothing -> error "Parser error."
        Just pl -> print $ length $ filter isValid pl 

isValid :: Passport -> Bool
isValid m =
    all (`M.member` m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
