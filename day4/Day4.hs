{-# LANGUAGE FlexibleInstances #-}
module Day4 (run)
where

import Prelude hiding (lines, words)
import Data.List (sort)
import Data.Text (Text, pack, unpack, splitOn, lines, words)
import Lib (multisolve)

space = pack " "
newline = pack "\n"

parse :: Text -> Either () [[Text]]
parse = Right . fmap words . lines

cart :: [a] -> [b] -> [(a, b)]
cart as bs = [(a, b) | a <- as, b <- bs]

isValid :: [Text] -> Bool
isValid [] = True
isValid (t:ts) = notElem t ts && isValid ts

solve1 :: [[Text]] -> Int
solve1 = sum . fmap (const 1) . filter isValid

sortLetters :: Text -> Text
sortLetters = pack . sort . unpack

solve2 :: [[Text]] -> Int
solve2 = solve1 . fmap (fmap sortLetters)

run :: IO ()
run = multisolve parse [solve1, solve2]
