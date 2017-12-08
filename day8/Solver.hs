{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import Debug.Trace (trace)
import Prelude hiding (lookup)
import Data.Text (Text, pack)  
import Data.Traversable (traverse)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad.State (State, get, put, evalState, execState, modify)
import Data.Map.Strict (Map, alter, empty, lookup, foldr, insert)
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
  
getRegisterValue :: Register -> State (Map Register Int) Int
getRegisterValue r = do 
  mem <- get
  return (fromMaybe 0 (lookup r mem))
  
isConditionSatisfied :: Condition -> State (Map Register Int) Bool
isConditionSatisfied c = do
  val <- getRegisterValue (getRegister c)
  return (getComparison c val (getValue c))

execute :: Instruction -> State (Map Register Int) Int
execute i = let target = getTarget i in
  do
    val <- getRegisterValue (getTarget i)
    let val' = getAdjust i val
    modify (insert target val')
    return val'
  
step :: Instruction -> State (Map Register Int) Int
step i = do
  mem <- get
  isSatisfied <- isConditionSatisfied (getCondition i)
  if isSatisfied then execute i else return minBound
  

compile :: [Instruction] -> State (Map Register Int) [Int]
compile = traverse step

solve1 :: [Instruction] -> Int
solve1 input = Data.Map.Strict.foldr max minBound $ execState program empty
  where program = compile input
  
solve2 :: [Instruction] -> Int
solve2 input = Prelude.foldr max minBound $ evalState program empty
  where program = compile input

run :: IO ()
run = multisolve parsecer [solve1, solve2]
