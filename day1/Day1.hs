{-# LANGUAGE FlexibleInstances #-}
module Day1
  ( run ) where

import Debug.Trace (trace)
import Data.Char (digitToInt)
import Data.Text (Text, unpack)
import Lib (multisolve, aperture)

type NumberSeq = [Int]

parse :: Text -> Either () NumberSeq
parse = Right . fmap digitToInt . unpack

same :: Eq a => [a] -> Bool
same (a:b:xs) = a == b
same _ = undefined

part1 :: NumberSeq -> Int
-- part1 input | trace ("input was " ++ show (getSeq input)) False = undefined
part1 ns = solve pairs
  where pairs = [last ns, head ns] : aperture 2 ns
        solve = sum . fmap head . filter same

useable :: Eq a => Int -> Int -> [a] -> (Int, a) -> Bool
useable len offset ref (i, a) = ref !! idx == a
  where idx = (i + offset) `mod` len

part2 :: NumberSeq -> Int
part2 ns = solve indexed
  where len = length ns
        indexed = zip [0..len-1] ns
        solve = sum . fmap snd . filter (useable len (quot len 2) ns)
        
run :: IO ()
run = multisolve parse [part1, part2]