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

data Group = Garbage Int | Group [Group]

escaped :: Parser Char
escaped = char '!' *> anyChar

garbage :: Parser Group
-- garbage = char '<' *> many (escaped <|> noneOf ">") *> char '>' $> Garbage
garbage = char '<' *> ((Garbage . sum) <$> many ((escaped $> 0) <|> (noneOf ">" $> 1))) <* char '>'

contents :: Parser [Group]
contents = sepBy (group <|> garbage) (char ',')

group :: Parser Group
group = Group <$> (char '{' *> contents <* char '}')

instance Parsecable Group where
  parsec = group <* eof

scoreGroups :: Int -> Group -> Int
scoreGroups base (Garbage _) = 0
scoreGroups base (Group children) = 
  let current = 1 + base in 
    current + sum (fmap (scoreGroups current) children)
    
countGarbage :: Group -> Int
countGarbage (Group children) = sum (countGarbage <$> children)
countGarbage (Garbage int) = int

solve1 :: Group -> Int
solve1 = scoreGroups 0

solve2 :: Group -> Int
solve2 = countGarbage

run :: IO ()
run = multisolve parsecer [solve1, solve2]
