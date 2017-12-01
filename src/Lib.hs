module Lib
    ( Compute,
      Parseable(parse),
      solve,
      multisolve,
      aperture
    ) 
    where

import Debug.Trace (trace)
import Control.Arrow ((&&&))
import Prelude hiding (interact)
import Data.Text.IO (interact)
import Data.Text (Text, strip)
import TextShow (TextShow(showb), toText)

type Compute a b = a -> b

class Parseable a where
  parse :: Text -> a
  
solve :: Parseable a => TextShow b => Compute a b -> IO ()
solve c = interact (toText . showb . c . parse . strip)

multisolve :: Parseable a => TextShow b => [Compute a b] -> IO ()
multisolve runs = interact (toText . showb . (runs <*>) . return . parse . strip)

aperture :: Int -> [a] -> [[a]]
aperture len [] = []
aperture len as = if length win == len 
    then win : aperture len (drop 1 as)
    else []
  where win = take len as