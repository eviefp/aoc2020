module Day2 where

import           Data.Void
    (Void)
import           Prelude
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = M.Parsec Void String

data PasswordEntry
  = PasswordEntry
      { first    :: Int
      , second   :: Int
      , char     :: Char
      , password :: String
      }

entryParser :: Parser PasswordEntry
entryParser = do
    first <- L.decimal
    _ <- C.char '-'
    second <- L.decimal
    _ <- C.spaceChar
    char <- C.alphaNumChar
    _ <- C.string ": "
    password <- M.many C.alphaNumChar
    _ <- M.many C.newline
    pure $ PasswordEntry {..}

parser :: Parser [PasswordEntry]
parser = M.many entryParser

run :: IO ()
run = do
    rawInput <- readFile "day2-1.txt"
    case M.parseMaybe parser rawInput of
        Nothing -> error "Parser error."
        Just xs -> print $ length . filter isValidEncore $ xs

isValid :: PasswordEntry -> Bool
isValid PasswordEntry {..} =
    let count = length . filter (== char) $ password
     in count >= first && count <= second

isValidEncore :: PasswordEntry -> Bool
isValidEncore PasswordEntry {..} = go first /= go second
  where
    go :: Int -> Bool
    go i = (Just char ==) $ safeIndex password (i-1)

safeIndex :: [a] -> Int -> Maybe a
safeIndex (x:_) 0  = Just x
safeIndex [] _     = Nothing
safeIndex (_:xs) n = safeIndex xs $ abs (n-1)
