{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import Prelude hiding (lookup)
import TextShow (FromStringShow(..))
import Control.Applicative ((<|>))
import Control.Monad.State (State, gets, put, modify, evalState)
import Control.Lens (Lens', view, over, set)
import Control.Lens.Lens (lens)
import Data.Vector (Vector, fromList, (!))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, splitOn, pack)
import Data.Text.Read (decimal)
import Data.Map (Map, lookup, insert, empty)
import Text.Parsec (eof, try, many, many1, letter, char, string, lower, digit, noneOf, newline, spaces, sepBy1, option)
import Text.Parsec.Text (Parser)
import Lib(Parsecable(..), parsecer, multisolve)

data Deref = Deref Char | Literal Int
newtype Register = Register Char 

data Op = Snd Deref
        | Set Register Deref
        | Add Register Deref
        | Mul Register Deref
        | Mod Register Deref
        | Rcv Deref
        | Jgz Deref Deref


trim :: Parser a -> Parser a
trim = (<* spaces)

num :: Parser Int
num = (char '-' *> ((negate . read) <$> many1 digit))
  <|> (read <$> many1 digit)

deref :: Parser Deref 
deref = (Deref <$> letter) <|> (Literal <$> num)

register :: Parser Register
register = Register <$> letter

op :: Parser Op
op = try (Snd <$> (string "snd " *> deref))
  <|> try (Set <$> (string "set " *> trim register) <*> deref)
  <|> try (Add <$> (string "add " *> trim register) <*> deref)
  <|> try (Mul <$> (string "mul " *> trim register) <*> deref)
  <|> try (Mod <$> (string "mod " *> trim register) <*> deref)
  <|> try (Rcv <$> (string "rcv " *> deref))
  <|> (Jgz <$> (string "jgz " *> trim deref) <*> deref)

ops :: Parser (Vector Op)
ops = fromList <$> sepBy1 op newline

instance Parsecable (Vector Op) where
  parsec = ops <* eof

data Machine = Machine {
  getOps :: Vector Op,
  getRegisters :: Map Char Int,
  getPc :: Int,
  getLastSound :: Int
}

type MachineState = State Machine

-- registersLens = Machine 
-- registerLens :: Char -> _
registerLens c = lens (fromMaybe 0 . lookup c . getRegisters) (\m v -> m {getRegisters = insert c v (getRegisters m)}) 

pcLens = lens getPc (\m v -> m {getPc = v})

dereference :: Deref -> MachineState Int
dereference (Literal i) = return i
dereference (Deref c) = gets $ view (registerLens c)

advancePc = over pcLens (+1)

compile :: Op -> MachineState (Maybe Int)
compile (Snd d) = do
  sound <- dereference d
  modify (\s -> s {getLastSound = sound})
  modify advancePc
  return Nothing
compile (Set (Register r) d) = do
  v <- dereference d
  modify (set (registerLens r) v)
  modify advancePc
  return Nothing
compile (Add (Register r) d) = do
  v <- dereference d
  modify (over (registerLens r) (+v))
  modify advancePc
  return Nothing
compile (Mul (Register r) d) = do
  v <- dereference d
  modify (over (registerLens r) (*v))
  modify advancePc
  return Nothing
compile (Mod (Register r) d) = do
  v <- dereference d
  modify (over (registerLens r) (`mod` v))
  modify advancePc
  return Nothing
compile (Rcv d) = do
  v <- dereference d
  modify advancePc
  if v /= 0 
    then gets (Just . getLastSound)
    else return Nothing
compile (Jgz r d) = do
  rv <- dereference r -- but not always a register
  v <- dereference d
  modify (over pcLens (if rv > 0 then (+v) else (+1)))    
  return Nothing

step :: MachineState (Maybe Int)
step = do
  pc <- gets getPc
  ops <- gets getOps
  compile (ops ! pc)
  
go :: MachineState [Maybe Int]
go = sequence (repeat step)

solve1 :: Vector Op -> Int
solve1 ops = head (e >>= maybeToList)
  where e = evalState go Machine {getOps = ops, getRegisters = empty, getPc = 0, getLastSound = 0}

run :: IO ()
run = multisolve parsecer [solve1]
