{-# LANGUAGE FlexibleInstances #-}
module Solver 
  ( run
  ) where
    

import TextShow (FromStringShow(..))
import GHC.Exts (sortWith)
import Data.Functor
import Text.Parsec (char, string, spaces, option, many1, digit, sepBy1, newline)
import Text.Parsec.Text (Parser)
import Debug.Trace
import Lib(multisolve, Parsecable(..), parsecer)

num :: Parser Int
num = option id (char '-' $> negate) <*> (read <$> many1 digit)

comma = char ','

newtype Vec = Vec (Int, Int, Int)
  deriving (Eq, Show)

manhattan :: Vec -> Int
manhattan (Vec (x, y, z)) = abs x + abs y + abs z

instance Ord Vec where
  compare v1 v2 = compare (manhattan v1) (manhattan v2)

vec :: Parser Vec
vec = char '<' *> (Vec <$>  ((,,) <$> (num <* comma) <*> (num <* comma) <*> num)) <* char '>'

data Particle = Particle {getPosition :: Vec, getVelocity :: Vec, getAccel :: Vec}
  deriving (Show)

mkParticle :: Vec -> Vec -> Vec -> Particle
mkParticle p v a = Particle {getPosition = p, getVelocity = v, getAccel = a}

commaSpace = string ", "

particle :: Parser Particle
particle = (mkParticle <$> (string "p=" *> vec <* commaSpace) <*> (string "v=" *> vec <* commaSpace) <*> (string "a=" *> vec)) <* spaces

particles = sepBy1 particle newline

instance Parsecable [Particle] where
  parsec = particles

solve1 :: [Particle] -> FromStringShow (Int, Particle)
solve1 particles = FromStringShow $ head $ sortWith (getAccel . snd) (traceShowId named)
  where named = zip [0..] particles :: [(Int, Particle)]

run :: IO ()
run = multisolve parsecer [solve1]
