{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( Parsecable(..)
    , multisolve
    , parsecer
    , aperture
    , allPass 
    ) where

import Debug.Trace (trace)
import Control.Arrow ((&&&))
import Prelude hiding (interact, unlines)
import Data.Bifunctor (first, bimap)
import Text.Parsec (ParseError, parse)
import Text.Parsec.Text (Parser)
import Data.List (find)
import Data.Text.IO (interact)
import Data.Text (Text, strip, pack, unlines, append)
import TextShow (TextShow(showt), FromStringShow(..))

class Parsecable a where
  parsec :: Parser a
-- Cannot typecheck :-(  
-- multisolve :: (TextShow e, TextShow b, Parseable e a) => [a -> b] -> IO ()
-- multisolve solns = interact $ 
--   \text -> 
--     let result = ((solns <*>) . return) <$> (parse $ strip text) in
--     either showt (unlines . fmap showt) result
-- 

renderLine :: (Int, Text) -> Text
renderLine (part, t) = foldl append (pack "Part ") [showt part, pack " = ", t]

render :: TextShow a => [a] -> Text
render as = unlines $ renderLine <$> zip [0..] (showt <$> as)

solve :: [a -> b] -> a -> [b]
solve solns = (solns <*>) . return

multisolve :: TextShow e => TextShow b => (Text -> Either e a) -> [a -> b] -> IO ()
multisolve load solns = interact $
  either showt render .
  fmap ((solns <*>) . return) .
  load . strip
  
parsecer :: Parsecable a => Text -> Either (FromStringShow ParseError) a
parsecer text = first FromStringShow $ parse parsec "" text

aperture :: Int -> [a] -> [[a]]
aperture len [] = []
aperture len as = if length win == len 
    then win : aperture len (drop 1 as)
    else []
  where win = take len as
  
-- is there a library function that does this
allPass :: [a -> Bool] -> a -> Bool
allPass = (and .) . sequence

anyPass :: [a -> Bool] -> a -> Bool
anyPass = (or .) . sequence