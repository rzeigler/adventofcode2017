{-# LANGUAGE FlexibleInstances #-}
module Solver (run)
where

import Debug.Trace (trace)
import TextShow (FromStringShow(..), showt)
import Control.Monad (join)
import Data.Text (Text, pack)
import Data.Set (fromList, difference)
import Control.Monad.Reader (Reader, asks)
import Data.Map.Strict (Map, insert, empty, (!))
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Text.Parsec (Parsec, ParseError, parse, eof)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (string, letter, digit, char, spaces, newline)
import Text.Parsec.Combinator (many1, sepBy1)
import Lib (Parseable(parse), multisolve)

data InputLine = InputLine { getName :: Text, getWeight :: Int, getChildren :: [Text]}
  deriving (Show)
  
newProg :: Text -> Int -> [Text] -> InputLine
newProg name weight children = InputLine { getName = name, getWeight = weight, getChildren = children }

name :: Parser Text
name = pack <$> many1 letter

weight :: Parser Int
weight = char '(' *> (read <$> many1 digit) <* char ')'

right :: Parser ()
right = string "->" $> ()

comma :: Parser ()
comma = char ',' $> ()

prog :: Parsec Text () InputLine
prog = try (newProg <$> (name <* spaces) <*> (weight <* spaces) <*> (right *> spaces *>  sepBy1 name (comma *> spaces)))
  <|> (newProg <$> (name <* spaces) <*> weight <*> pure [])
  
progs :: Parser [InputLine]
progs = sepBy1 prog newline <* eof

instance Parseable (Either ParseError [InputLine]) where
  parse = Text.Parsec.parse progs ""
  
data Edge = Edge Text Text
  deriving (Show)
data Data = Data { getNodes :: [Text], getWeights :: Map Text Int, getEdges :: [Edge]}
  deriving (Show)
  
getEdgeStart :: Edge -> Text
getEdgeStart (Edge s _) = s

getEdgeEnd :: Edge -> Text
getEdgeEnd (Edge _ e) = e

mkWeightInsert :: InputLine -> Map Text Int -> Map Text Int
mkWeightInsert input = insert (getName input) (getWeight input)

mkEdges :: InputLine -> [Edge]
mkEdges input = fmap (Edge (getName input)) (getChildren input) 

load :: [InputLine] -> Data
load inputs = Data { getNodes = nodes, getWeights = weights, getEdges = edges }
  where nodes = getName <$> inputs
        weights = (foldl1 (.) $ fmap mkWeightInsert inputs) empty
        edges = inputs >>= mkEdges
        
impl1 :: [InputLine] -> Text
impl1 inputs = showt $ FromStringShow $ difference (fromList $ getNodes d) (fromList $ getEdgeEnd <$> getEdges d)
  where d = load inputs
  
solve1 :: Either ParseError [InputLine] -> FromStringShow (Either ParseError Text)
solve1 = FromStringShow . fmap impl1

getNodeChildren :: Text -> Reader Data [Text]
getNodeChildren parent = asks $ fmap getEdgeEnd . filter startsAtParent . getEdges
  where startsAtParent = (parent ==) . getEdgeStart

getLeaves :: Reader Data [Text]
getLeaves = do
  parents <- asks $ fmap getEdgeStart . getEdges
  asks $ filter (not . (`elem` parents)) . getNodes
  
getNodeWeight :: Text -> Reader Data Int
getNodeWeight node = asks ((! node) . getWeights)
  
getDescendants :: Text -> Reader Data [Text]
getDescendants root = do
  children <- getNodeChildren root
  descendants <- join <$> traverse getNodeChildren children
  return (children ++ descendants)
  
getDescendantsWeight :: Text -> Reader Data Int
getDescendantsWeight root = do
  descendants <- getDescendants root
  sum <$> traverse getNodeWeight descendants
  
data ChildAnalysis = ChildAnalysis Text Int Int
  
analyzeNode :: Text -> Reader Data ()
analyzeNode node = do
  children <- getNodeChildren node
  weights <- traverse getNodeWeight children
  subWeights <- traverse getDescendantsWeight children
  let analysis = zipWith3 ChildAnalysis children weights subWeights
  return ()
  
impl2 :: [InputLine] -> Text
impl2 inputs = undefined
  where d = load inputs

run :: IO ()
run = multisolve [solve1]
