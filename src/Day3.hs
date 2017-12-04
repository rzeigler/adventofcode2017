module Day3 (run)
where

import Data.Text (unpack)
import Lib (Parseable(parse), multisolve)

  
instance Parseable Int where
  parse = read . unpack
  
data Bearing = PosX | NegX | PosY | NegY

data Segment = Segment { getPoint :: (Int, Int), getBearing :: Bearing, getLength :: Int }

nextBearing :: Bearing -> Bearing
nextBearing PosX = PosY
nextBearing PosY = NegX
nextBearing NegX = NegY
nextBearing NegY = PosX

bearingToVector :: Bearing -> Int -> (Int, Int)
bearingToVector PosX l = (l, 0)
bearingToVector NegX l = (-l, 0)
bearingToVector PosY l = (0, l)
bearingToVector NegY l = (0, -l)

extend :: Bearing -> Int -> [Bearing]
extend b c = undefined
  
run :: IO ()
run = multisolve [id :: Int -> Int]
