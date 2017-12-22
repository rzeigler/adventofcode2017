{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import Prelude hiding (reverse)
import Control.Monad.ST (ST, runST)
import TextShow (FromStringShow(..))
import Data.Monoid ((<>))
import Data.Vector (MVector, Vector, fromList, thaw, freeze, reverse)
import Data.Vector.Generic.Mutable (slice)
import Data.Text (Text, splitOn, pack)
import Data.Text.Read (decimal)
import Lib(Parsecable(..), parsecer, multisolve)

comma = pack ","

parse :: Text -> Either String [Int]
parse = traverse (fmap fst . decimal) . splitOn comma

list :: Vector Int
list = fromList [0..255]

data Track = Track { lengths :: [Int], offset :: Int }

computeSliceRanges :: Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
computeSliceRanges m (lower, upper) accum = 
  if upper <= m 
    then accum <> [(lower, upper)]
    else let adjusted = (lower, m) in
         computeSliceRanges m (0, upper - m) (accum <> [adjusted])
         
slices = computeSliceRanges 255
  
  
slicer :: Int -> Int -> MVector s Int -> MVector s Int
slicer = slice

freezer :: MVector s Int -> ST s (Vector Int)
freezer = freeze

collect :: [(Int, Int)] -> MVector s Int -> ST s (Vector Int)
collect ranges v = foldl1 (<>) <$> packed
  where extractors = (freezer .) <$> (uncurry slicer <$> ranges)
        packed = sequence (extractors <*> pure v)

rewrite :: Int -> [(Int, Int)] -> Vector Int -> MVector s Int -> ST s ()
rewrite _ [] _ v = return ()
rewrite o r:rest source v = undefined

run1 :: Track -> MVector s Int -> ST s (Vector Int)
run1 Track {lengths = []} v = freeze v
run1 Track {lengths = l:rest, offset = o} v = do
  let ranges = slices (o, l) []
  collected <- collect ranges v
  rewrite ranges (reverse collected) v
  run1 Track {lengths = rest, offset = l + o} v

solve1 :: [Int] -> Vector Int
solve1 input = runST $ thaw list >>= run1 Track{ lengths = input, offset = 0}

run :: IO ()
run = multisolve parse [FromStringShow . solve1]
