module Day5 (run)
where

import Prelude hiding (lines)
import Debug.Trace (trace)
import Control.Applicative ((<$>))
import Control.Monad.State (State, state, evalState)
import Data.Vector (Vector, fromList, (!?), (//))
import Data.Vector.Mutable (write)
import Data.Text (Text, lines, unpack)
import Lib (Parseable(parse), multisolve, newline)

data VM = VM { getMem :: Vector Int, getOffset :: Int, getJumps :: Int }
  deriving (Show)
  
step :: (Int -> Int) -> VM -> ((Int, Bool), VM)
step moveUpdate vm = 
  case instruction of
    Just move -> ((getJumps vm, False), VM { getMem = getMem vm // [(offset, moveUpdate move)], getJumps = getJumps vm + 1, getOffset = offset + move })
    Nothing -> ((getJumps vm, True), vm)
  where offset = getOffset vm
        instruction = getMem vm !? offset
        
update :: Int -> Int
update move | move >= 3 = move - 1
update move = move + 1

instance Parseable VM where
  parse = (\js -> VM { getMem = js, getOffset = 0, getJumps = 0}) . fromList . fmap (read . unpack) . lines
  
execute :: State VM (Int, Bool)
execute = head . dropWhile (not . snd) <$> sequence (repeat (state (step (+1))))

execute' :: State VM (Int, Bool)
execute' = head . dropWhile (not . snd) <$> sequence (repeat (state (step update)))

solve1 :: VM -> Int
solve1 = fst . evalState execute

solve2 :: VM -> Int
solve2 = fst . evalState execute'

run :: IO ()
run = multisolve [solve1, solve2]
