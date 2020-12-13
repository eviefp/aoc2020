{-# LANGUAGE TypeApplications #-}
module Day42 where

import           Data.Void
    (Void)
import           Prelude
import qualified Text.Megaparsec            as MP
import qualified Data.Map                   as M
import qualified Text.Megaparsec.Char       as C
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Data.Char (isHexDigit, isDigit)

type Parser = MP.Parsec Void String

newtype BirthYear = BirthYear Int
newtype IssueYear = IssueYear Int
newtype ExpirationYear = ExpirationYear Int
data Height = HeightCm Int | HeightIn Int
newtype HairColor = HairColor String
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
newtype PassportId = PassportId String
newtype CountryId = CountryId String

data Passport = Passport
    { byr :: BirthYear
    , iyr :: IssueYear
    , eyr :: ExpirationYear
    , hgt :: Height
    , hcl :: HairColor
    , ecl :: EyeColor
    , pid :: PassportId
    , cid :: Maybe CountryId
    }
  

parseKeyValue :: Parser (String, String)
parseKeyValue = do
    left <- MP.some (MP.noneOf [':', ' ', '\n'])
    _ <- C.char ':'
    right <- MP.some (MP.noneOf [' ', '\n' ])
    _ <- MP.optional C.spaceChar
    pure (left, right)

parsePassportData :: Parser (M.Map String String)
parsePassportData = do
    kv <- MP.some parseKeyValue
    _ <- C.space
    pure $ M.fromList kv

birthYear :: String -> Maybe BirthYear
birthYear input = do
  year <- readMaybe input
  guard $ year >= 1920 && year <= 2002
  pure $ BirthYear year

issueYear :: String -> Maybe IssueYear
issueYear input = do
  year <- readMaybe input
  guard $ year >= 2010 && year <= 2020
  pure $ IssueYear year
  
expirationYear :: String -> Maybe ExpirationYear
expirationYear input = do
  year <- readMaybe input
  guard $ year >= 2020 && year <= 2030
  pure $ ExpirationYear year

height :: String -> Maybe Height
height input = do
  let (snum, unit) = span isDigit input
  num <- readMaybe snum
  case unit of
    "cm" -> do
      guard $ num >= 150 && num <= 193
      pure $ HeightCm num
    "in" -> do
      guard $ num >= 59 && num <= 76
      pure $ HeightIn num
    _ -> Nothing

hairColor :: String -> Maybe HairColor
hairColor = \case
  input@('#':xs) -> do
    guard $ all isHexDigit xs
    guard $ length xs == 6
    pure $ HairColor input
  _ -> Nothing
  
eyeColor :: String -> Maybe EyeColor
eyeColor = \case
  "amb" -> pure Amb
  "blu" -> pure Blu
  "brn" -> pure Brn
  "gry" -> pure Gry
  "grn" -> pure Grn
  "hzl" -> pure Hzl
  "oth" -> pure Oth
  _     -> Nothing

passportId :: String -> Maybe PassportId
passportId input = do
  guard $ length input == 9
  guard $ all isDigit input
  pure $ PassportId input

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.
parsePassport :: Parser (Maybe Passport)
parsePassport = do
  pdata <- parsePassportData
  pure $ do
    byr <- M.lookup "byr" pdata >>= birthYear
    iyr <- M.lookup "iyr" pdata >>= issueYear
    eyr <- M.lookup "eyr" pdata >>= expirationYear
    hgt <- M.lookup "hgt" pdata >>= height
    hcl <- M.lookup "hcl" pdata >>= hairColor
    ecl <- M.lookup "ecl" pdata >>= eyeColor
    pid <- M.lookup "pid" pdata >>= passportId
    let cid = CountryId <$> M.lookup "cid" pdata
    pure Passport {..}
    
 
parser :: Parser [Passport]
parser = catMaybes <$> MP.many parsePassport

run :: IO ()
run = do
    rawInput <- readFile "day4-1.txt"
    case MP.parseMaybe parser rawInput of
        Nothing -> error "Parser error."
        Just pl -> print $ length pl
