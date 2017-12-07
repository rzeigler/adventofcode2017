{-# LANGUAGE FlexibleInstances #-}
module Solver (run)
where

import Debug.Trace (trace)
import Data.Text (Text, pack)
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Parsec (Parsec, ParseError, parse, eof)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (string, letter, digit, char, spaces, newline)
import Text.Parsec.Combinator (many1, sepBy1)
import Lib (Parseable(parse), multisolve)

data Prog = Prog { getName :: Text, getWeight :: Int, getChildren :: [Text]}
  deriving (Show)
  
newProg :: Text -> Int -> [Text] -> Prog
newProg name weight children = Prog { getName = name, getWeight = weight, getChildren = children }

name :: Parser Text
name = pack <$> many1 letter

weight :: Parser Int
weight = char '(' *> (read <$> many1 digit) <* char ')'

right :: Parser ()
right = string "->" $> ()

comma :: Parser ()
comma = char ',' $> ()

prog :: Parsec Text () Prog
prog = try (newProg <$> (name <* spaces) <*> (weight <* spaces) <*> (right *> spaces *>  sepBy1 name (comma *> spaces)))
  <|> (newProg <$> (name <* spaces) <*> weight <*> pure [])
  
progs :: Parser [Prog]
progs = sepBy1 prog newline <* eof

instance Parseable (Either ParseError [Prog]) where
  parse = Text.Parsec.parse progs ""

run :: IO ()
run = multisolve ([show] :: [Either ParseError [Prog] -> String])