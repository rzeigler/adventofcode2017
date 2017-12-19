{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where
    

import TextShow (FromStringShow(..))
import Debug.Trace
import Data.Char (isLetter)
import Data.Bifunctor (bimap)
import Data.Monoid (Sum)
import Control.Monad (filterM)
import Control.Monad.Reader (Reader, asks, reader, runReader)
import Control.Monad.Writer (WriterT, tell, lift, runWriterT)
import Data.Vector (Vector, fromList, elemIndex, (!))
import qualified Data.Vector as V
import Data.Text (Text, splitOn, pack, unpack)
import Data.Text.Read (decimal)
import Data.Map (Map)
import Lib(multisolve')

-- X axis is inverted

data Cell = Pipe (Maybe Char) | Turn | Empty
  deriving (Show, Eq)
  
type Maze = Vector (Vector Cell)

readCell :: Char -> Cell
readCell '|' = Pipe Nothing
readCell '-' = Pipe Nothing
readCell '+' = Turn
readCell ch | isLetter ch = Pipe (Just ch)
readCell _ = Empty

newtype Coord = Coord (Int, Int)
  deriving (Show)
newtype Bearing = Bearing (Int, Int)
  deriving (Show)

index :: Coord -> Reader Maze Cell
index (Coord (x, y)) = asks ((! x) . (! y))

up = Bearing (0, -1)
down = Bearing (0, 1)
left = Bearing (-1, 0)
right = Bearing (1, 0)

data Position = Position {getCoord :: Coord, getBearing :: Bearing}
  deriving (Show)

vectorize :: [String] -> Maze
vectorize = fromList . fmap (fromList . fmap readCell)

parse :: Text -> Either () Maze
parse = Right . vectorize . lines . unpack

initial :: Maze -> (Int, Int)
initial m = case mx of
    Just x -> (x, 0)
    Nothing -> error "Couldn't locate an entry"
  where mx = elemIndex (Pipe Nothing) (V.head m)
  
nextPipePosition :: Position -> Reader Maze (Maybe Position)
nextPipePosition Position {getCoord = Coord (x, y), getBearing = b@(Bearing (dx, dy))} = 
  let next = Coord (x + dx, y + dy) in
  do
    cell <- index next
    case cell of 
      Empty -> return Nothing
      _ -> return (Just Position {getCoord = next, getBearing = b})
      
invert = (* negate 1)

rotated :: Bearing -> [Bearing]
rotated (Bearing (dx, dy)) = [Bearing (dy, dx), Bearing (negate dy, negate dx)]

coord :: Coord -> Bearing -> Position
coord (Coord (x, y)) b@(Bearing (dx, dy)) = Position {getCoord = Coord (x + dx, y + dy), getBearing = b}

isValid :: Position -> Reader Maze Bool
isValid Position {getCoord = coord} = do
  cell <- index coord
  case cell of
    Pipe _ -> return True
    _ -> return False

neighbors :: Position -> [Position]
neighbors Position {getCoord = c, getBearing = b} = coord c <$> rotated b
      
nextTurnPosition :: Position -> Reader Maze (Maybe Position)
nextTurnPosition p = (Just . head) <$> filterM isValid (neighbors p)
  
nextPosition :: Position -> Reader Maze (Maybe Position)
nextPosition p@Position {getCoord = coord, getBearing = bearing} = do
  cell <- index coord
  case cell of 
    Pipe x -> nextPipePosition p
    Turn -> nextTurnPosition p

record :: Position -> WriterT (String, Sum Int) (Reader Maze) ()
record Position {getCoord=coord} = do
  cell <- lift (index coord)
  case cell of
    Pipe (Just ch) -> tell ([ch], 1)
    _ -> tell ([], 1)

advance :: Position -> WriterT (String, Sum Int) (Reader Maze) Position
advance p = do
  record p
  next <- lift (nextPosition p)
  case next of 
    Just np -> advance np
    Nothing -> return p
  
solve1 :: Maze -> (String, Sum Int)
solve1 maze = snd (runReader wm maze)
  where start = initial maze
        wm = runWriterT (advance Position {getCoord = Coord start, getBearing = down})

run :: IO ()
run = multisolve' parse [solve1]
