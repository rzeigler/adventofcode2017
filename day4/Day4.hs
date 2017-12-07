{-# LANGUAGE FlexibleInstances #-}
module Day4 (run)
where

import Data.List (sort)
import Data.Text (Text, pack, unpack, splitOn)
import Lib (Parseable(parse), multisolve)

space = pack " "
newline = pack "\n"

instance Parseable [[Text]] where
  parse = fmap (splitOn space) . splitOn newline

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
run = multisolve [solve1, solve2]
