module Day1
  ( main1 ) where

import Debug.Trace (trace)
import Data.Char (digitToInt)
import Data.Text (unpack)
import Lib (Parseable(parse), solve, aperture)

newtype NumberSeq = NumberSeq { getSeq :: [Int] } 
instance Parseable NumberSeq where
  parse = NumberSeq . fmap digitToInt . unpack

same :: Eq a => [a] -> Bool
same (a:b:xs) = a == b
same _ = undefined

part1 :: NumberSeq -> Int
-- part1 input | trace ("input was " ++ show (getSeq input)) False = undefined
part1 input = solve pairs
  where ns = getSeq input
        pairs = [last ns, head ns] : aperture 2 ns
        solve = sum . fmap head . filter same
main1 :: IO ()
main1 = solve part1
