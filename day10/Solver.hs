{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where

import TextShow (FromStringShow(..))
import Data.Vector (Vector, fromList)
import Data.Text (Text, splitOn, pack)
import Data.Text.Read (decimal)
import Lib(Parsecable(..), parsecer, multisolve)

comma = pack ","

parse :: Text -> Either String [Int]
parse = traverse (fmap fst . decimal) . splitOn comma

list :: Vector Int
list = fromList [0..255]

data Location = Location (Vector Int) Int Int

getContent :: Location -> Vector Int
getContent (Location v _ _) = v

getOffset :: Location -> Int
getOffset (Location _ o _) = o

getSkip :: Location -> Int
getSkip (Location _ _ s) = s

solve1 :: [Int] -> [Int]
solve1 = id

run :: IO ()
run = multisolve parse [solve1]
