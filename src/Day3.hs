module Day3 (run)
where

import Debug.Trace (trace)
import Control.Arrow ((***))
import Control.Applicative (pure, (<*>))
import Data.Monoid
import Data.Text (unpack)
import Data.List (find)
import Lib (Parseable(parse), multisolve)

  
instance Parseable Int where
  parse = read . unpack
  
data Bearing = PosX | NegX | PosY | NegY
  deriving (Show)

data Segment = Segment { getPoint :: (Int, Int), getBearing :: Bearing, getLength :: Int, getCellOffset :: Int }
  deriving (Show)

nextBearing :: Bearing -> Bearing
nextBearing PosX = PosY
nextBearing PosY = NegX
nextBearing NegX = NegY
nextBearing NegY = PosX

bearingToVector :: Bearing -> Int -> (Int, Int)
bearingToVector PosX l = (l, 0)
bearingToVector NegX l = (negate l, 0)
bearingToVector PosY l = (0, l)
bearingToVector NegY l = (0, negate l)

vadd :: Num a => Num b => (a, b) -> (a, b) -> (a, b)
vadd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scale :: Int -> (Int, Int) -> (Int, Int)
scale x = (x*) *** (x*)

extend :: Segment -> Bool -> [Segment]
extend seg further = next : extend next (not further)
  where point = vadd (getPoint seg) (bearingToVector (getBearing seg) (getLength seg))
        bearing = nextBearing (getBearing seg)
        len = if further then getLength seg + 1 else getLength seg
        next = Segment { getPoint = point, getBearing = bearing, getLength = len, getCellOffset = getCellOffset seg + getLength seg }

initial = Segment { getPoint = (0, 0), getBearing = PosX, getLength = 1, getCellOffset = 0 }
segments = initial : extend initial False
  
search :: [Segment] -> Int -> Segment
search (seg:rest) cell = if getCellOffset seg <= cell && getCellOffset seg + getLength seg > cell 
  then seg
  else search rest cell
  
runSearch = search segments
  
manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
  
solve1 :: Int -> Int
solve1 cell = manhattan (0, 0) (vadd (getPoint seg) (bearingToVector (getBearing seg) (cell - getCellOffset seg))) - 1
  where seg = runSearch cell
  
nearVecs :: [(Int, Int)]
nearVecs = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent = (vadd <$> nearVecs <*>) . return
  
data Cell = Cell { getAddr :: (Int, Int), getValue :: Int }
  deriving (Show)
type Memory = [Cell]

isCellAt :: (Int, Int) -> Cell -> Bool
isCellAt loc cell = getAddr cell == loc

findCell :: (Int, Int) -> Memory -> Maybe Cell
findCell = find . isCellAt

cellValues :: [Maybe Cell] -> [Int]
cellValues = (>>= maybe [] (return . getValue))

cellValue :: Memory -> (Int, Int) -> Int
cellValue mem cell = sum (cellValues nearbyCells)
  where nearbyCells = fmap findCell (adjacent cell) <*> pure mem

advance :: [Segment] -> (Int, Int) -> ([Segment], (Int, Int))
advance (cur:rest) point | point == vadd (getPoint cur) (bearingToVector (getBearing cur) (getLength cur)) = (rest, getPoint (head rest))
advance (cur:rest) point = (cur:rest, vadd point (bearingToVector (getBearing cur) 1))

impl2 :: Int -> Memory -> [Segment] -> (Int, Int) -> Int
-- impl2 search mem (cur:rest) addr | trace (show addr) False = undefined
impl2 search mem (cur:rest) addr = 
  let val = cellValue mem addr in
    if val > search 
      then val 
      else let nextMem = Cell{getAddr = addr, getValue = val}:mem in
           let (nextSeg, nextPoint) = advance (cur:rest) addr in
            impl2 search nextMem nextSeg nextPoint

solve2 :: Int -> Int
solve2 v = impl2 v [Cell{getAddr = (0, 0), getValue = 1}] segments (1, 0)
  
run :: IO ()
run = multisolve [solve1, solve2]
