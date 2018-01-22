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

solve1 :: [Int] -> Vector Int
solve1 input = undefined

run :: IO ()
run = multisolve parse [FromStringShow . solve1]
