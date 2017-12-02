{-# LANGUAGE FlexibleInstances #-}
module Day2 (run) where

import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Data.List (find, tails)
import Data.Text (Text, splitOn, unpack, pack)
import Lib (Parseable(parse), multisolve, allPass)

splitRows :: Text -> [Text]
splitRows = splitOn (pack "\n")

splitCells :: Text -> [Text]
splitCells = splitOn (pack "\t")

parseCell :: Text -> Int
parseCell = read . unpack

instance Parseable [[Int]] where
  parse = fmap (fmap parseCell . splitCells) . splitRows
     
diff :: [Int] -> Int
diff cells = maximum cells - minimum cells
     
solve1 :: [[Int]] -> Int
solve1 = sum . fmap diff

wholeDivide :: Int -> Int -> Maybe Int
wholeDivide a b = if a > b && a `mod` b == 0
  then Just (quot a b)
  else Nothing

-- Can they ever be the same?
findRowValue :: [Int] -> Maybe Int
-- findRowValue input | trace (show input) False = undefined
findRowValue input = foldl (<|>) Nothing $ fmap (uncurry wholeDivide) xprod
  where xprod = [(x, y) | x <- input, y <- input]

solve2 :: [[Int]] -> Int
solve2 rows = maybe (negate 1) sum (traverse findRowValue rows)

run :: IO ()
run = multisolve [solve1, solve2]