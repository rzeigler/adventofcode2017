{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import Debug.Trace (trace)
import Prelude hiding (lookup)
import Data.Text (Text, pack)  
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map, alter, empty, lookup, foldr)
import Text.Parsec (eof)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (char, string, lower, digit, newline, spaces)
import Text.Parsec.Combinator (many1, sepBy1, option)
import TextShow (FromStringShow(..))
import Lib(Parsecable(..), parsecer, multisolve)

newtype Register = Register Text
  deriving (Show, Eq, Ord)

type Adjust = (Int -> Int)
type Comparison = Int -> Int -> Bool
data Condition = Condition {getRegister :: Register, getComparison :: Comparison, getValue :: Int} 
data Instruction = Instruction {getTarget :: Register, getAdjust :: Adjust, getCondition :: Condition}

trim :: Parser a -> Parser a
trim = (<* option () spaces)

line :: Parser a -> Parser a
line = (<* newline)

mkCondition :: Register -> Comparison -> Int -> Condition
mkCondition r c i = Condition {getRegister=r, getComparison=c, getValue=i}

mkInstruction :: Register -> Adjust -> Condition -> Instruction
mkInstruction r o c = Instruction {getTarget=r, getAdjust=o, getCondition=c}

register :: Parser Register
register = (Register . pack) <$> many1 lower

num :: Parser Int
num = (char '-' *> ((negate . read) <$> many1 digit))
  <|> (read <$> many1 digit)

-- op :: Parser Op
op :: Parser Adjust
op = ((+) <$> (trim (string "inc") *> num))
  <|> (subtract <$> (trim (string "dec") *> num))

-- op = (((+) . read) <$> (trim (string "inc") *> num))
--   <|> ((subtract . read) <$> (trim (string "dec") *> num))
  
comparison :: Parser Comparison
comparison = (string "==" $> (==))
  <|> (string "!=" $> (/=))
  <|> try (string ">=" $> (>=))
  <|> (string ">" $> (>))
  <|> try (string "<=" $> (<=))
  <|> (string "<" $> (<))
  
condition :: Parser Condition
condition = trim (string "if") *> (mkCondition <$> trim register <*> trim comparison <*> num)

instruction :: Parser Instruction
instruction = mkInstruction <$> trim register <*> trim op <*> condition

instructions :: Parser [Instruction]
instructions = sepBy1 instruction newline <* eof

instance Parsecable [Instruction] where
  parsec = instructions
  
getRegisterValue :: Register -> Map Register Int -> Int
getRegisterValue r = fromMaybe 0 . lookup r

satisfied :: Condition -> Map Register Int -> Bool
satisfied c m = getComparison c (getRegisterValue (getRegister c) m) (getValue c)
  
modify :: Register -> Adjust -> Map Register Int -> Map Register Int
modify r o = alter (Just . o . fromMaybe 0) r
  
pipe = flip (.)
  
execute :: Instruction -> Map Register Int -> Map Register Int
execute i mem = if not $ satisfied (getCondition i) mem 
    then mem
    else modify (getTarget i) (getAdjust i) mem

solve1 :: [Instruction] -> Int
solve1 input = Data.Map.Strict.foldr max minBound (exec empty)
  where exec = foldl1 pipe (execute <$> input)

run :: IO ()
run = multisolve parsecer [solve1]
