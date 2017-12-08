{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import Data.Text (Text, pack)  
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Parsec (eof)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (char, string, lower, digit, newline, spaces)
import Text.Parsec.Combinator (many1, sepBy1, option)
import TextShow (FromStringShow(..))
import Lib(Parsecable(..), parsecer, multisolve)

newtype Register = Register Text
  deriving (Show)
data Op = Inc Int | Dec Int
  deriving (Show)
data Comparison = Ne | Eq | Gt | Gte | Lt | Lte
  deriving (Show)
data Condition = Condition {getRegister :: Register, getComparison :: Comparison, getValue :: Int} 
  deriving (Show)
data Instruction = Instruction {getTarget :: Register, getOp :: Op, getCondition :: Condition}
  deriving (Show)

trim :: Parser a -> Parser a
trim = (<* option () spaces)

line :: Parser a -> Parser a
line = (<* newline)

mkCondition :: Register -> Comparison -> Int -> Condition
mkCondition r c i = Condition {getRegister=r, getComparison=c, getValue=i}

mkInstruction :: Register -> Op -> Condition -> Instruction
mkInstruction r o c = Instruction {getTarget=r, getOp=o, getCondition=c}

register :: Parser Register
register = (Register . pack) <$> many1 lower

num :: Parser Int
num = (char '-' *> ((negate . read) <$> many1 digit))
  <|> (read <$> many1 digit)

op :: Parser Op
op = (Inc <$> (trim (string "inc") *> num))
  <|> (Dec <$> (trim (string "dec") *> num))
  
comparison :: Parser Comparison
comparison = (string "==" $> Eq)
  <|> (string "!=" $> Ne)
  <|> try (string ">=" $> Gte)
  <|> (string ">" $> Gt)
  <|> try (string "<=" $> Lte)
  <|> (string "<" $> Lte)
  
condition :: Parser Condition
condition = trim (string "if") *> (mkCondition <$> trim register <*> trim comparison <*> num)

instruction :: Parser Instruction
instruction = mkInstruction <$> trim register <*> trim op <*> condition

instructions :: Parser [Instruction]
instructions = sepBy1 instruction newline <* eof

instance Parsecable [Instruction] where
  parsec = instructions

solve1 :: [Instruction] -> FromStringShow [Instruction]
solve1 = FromStringShow

run :: IO ()
run = multisolve parsecer [solve1]
