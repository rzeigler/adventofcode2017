{-# LANGUAGE FlexibleInstances #-}
module Solver (run)
where

import Debug.Trace (trace)
import Data.Text (Text)
import Lib (Parseable(parse), multisolve)

data Input = Input { getName :: Text, getWeight :: Int, getChildren :: [Text]}
  deriving (Show)

instance Parseable Input where
  parse = undefined

run :: IO ()
run = print "Hi\n"