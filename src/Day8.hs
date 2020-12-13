{-# LANGUAGE TypeApplications #-}
module Day8 where

import Prelude
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as S
import Control.Monad (guard)
import Data.Void (Void)

data Instruction
    = Nop
    | Acc Int
    | Jmp Int
    deriving stock (Show)

parser :: IO [Instruction]
parser = map go . lines <$> readFile "day8-1.txt"
  where
    go :: String -> Instruction
    go input =
      case words input of
        [i, '+':n] -> parseInstruction i n
        [i, n]     -> parseInstruction i n
        p          -> error $ "unexpected format: " <> unwords p

parseInstruction :: String -> String -> Instruction
parseInstruction instr num =
  case instr of
    "nop" -> Nop
    "acc" -> Acc (read @Int num)
    "jmp" -> Jmp (read @Int num)
    _     -> error "Bad input format"

evaluate :: Instruction -> (Int, Int) -> (Int, Int)
evaluate instr (acc, ip) = case instr of
  Nop -> (acc, ip + 1)
  Acc n -> (acc + n, ip + 1)
  Jmp n -> (acc, ip + n)

type ExecutionContext = ExceptT () (State (Int, Int, [Int]))

accWhenLoop :: [Instruction] -> Int
accWhenLoop instructions@(i1:_) =
      case (`S.execState` (0, 0, [])) . E.runExceptT $ go i1 of
        (acc, _, _) -> acc
  where
    go :: Instruction -> ExecutionContext Void
    go instr = do
      (acc, i, is) <- S.get
      let (acc', i') = evaluate instr (acc, i)
      guard $ i `notElem` is
      S.put (acc', i', i:is)
      go $ instructions !! i'
      
      
    

