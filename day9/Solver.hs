{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import TextShow (FromStringShow(..))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text, pack)
import Text.Parsec (eof, try, many, anyChar, char, string, lower, digit, noneOf, newline, spaces)
import Text.Parsec.Text (Parser)
import Text.Parsec.Combinator (sepBy, option)
import Lib(Parsecable(..), parsecer, multisolve)

data Group = Garbage | Group [Group]

escaped :: Parser Char
escaped = char '!' *> anyChar

garbage :: Parser Group
garbage = char '<' *> many (escaped <|> noneOf ">") *> char '>' $> Garbage

contents :: Parser [Group]
contents = sepBy (group <|> garbage) (char ',')

group :: Parser Group
group = Group <$> (char '{' *> contents <* char '}')

instance Parsecable Group where
  parsec = group <* eof

score :: Int -> Group -> Int
score base Garbage = 0
score base (Group children) = 
  let current = 1 + base in 
    current + sum (fmap (score current) children)

solve1 :: Group -> Int
solve1 = score 0

run :: IO ()
run = multisolve parsecer [solve1]
