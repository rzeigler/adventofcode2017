{-# LANGUAGE FlexibleInstances #-}
module Day6 (run)
where

import Prelude hiding (maximum)
import Debug.Trace (trace)
import Data.List (elemIndex)
import Data.Vector (Vector, fromList, maxIndex, maximum, (!), (//))
import Data.Text (Text, lines, splitOn, unpack, pack)
import Lib (Parseable(parse), multisolve)

type Mem = Vector Int

instance Parseable Mem where
  parse = fromList . fmap (read . unpack) . splitOn (pack "\t")

move :: [Int] -> Mem -> Mem
move idxs mem = mem // fmap (\i -> (i, (mem ! i) + 1)) idxs
  
realloc :: Int -> Int -> Mem -> Mem
realloc r start mem = if r == 0 
  then mem 
  else realloc (r - 1) (target + 1) (mem // [(target, (mem ! target) + 1)])
    where target = if start < length mem then start else 0
        
  
compute :: ([Mem], Int) -> Mem -> ([Mem], Int)
compute (history, ct) cur = if cur `elem` history 
  then (cur:history, ct)
  else compute (cur:history, ct + 1) (realloc blks (idx + 1) (cur // [(idx, 0)]))
    where blks = maximum cur
          idx = maxIndex cur
        
          
solve1 :: Mem -> Maybe Int
solve1 = Just . snd . compute ([], 0)

solve2 :: Mem -> Maybe Int
solve2 mem = 
  let (cur:history, ct) = compute ([], 0) mem
  in (ct - 1 -) <$> elemIndex cur history 
  
run :: IO ()
run = multisolve [solve1, solve2]
