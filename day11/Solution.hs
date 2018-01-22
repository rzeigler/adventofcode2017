{-# LANGUAGE OverloadedStrings #-}

module Solution
  ( run
  ) where
    
import Data.Monoid ((<>))
import Data.Bifunctor (bimap)
import Data.Text (Text, splitOn)
import TextShow (FromStringShow(FromStringShow))  
import Lib (multisolve)

-- N/S is the Z, NE/SW is the X, and NW/SE is the Y

data Step = N | S | NE | SW | NW | SE
  deriving (Show, Eq)
newtype Error = Error Text
  deriving (Show, Eq)

fromChars :: Text -> Either Error Step
fromChars "n" = Right N
fromChars "s" = Right S
fromChars "ne" = Right NE
fromChars "se" = Right SE
fromChars "sw" = Right SW
fromChars "nw" = Right NW
fromChars t = Left (Error ("Saw unexpected token: " <> t))

comma = ","

parse :: Text -> Either Error [Step]
parse = traverse fromChars . splitOn comma
    
run :: IO ()
run = multisolve parse [FromStringShow]