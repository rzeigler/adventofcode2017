{-# LANGUAGE FlexibleInstances #-}
module Solver (run)
where

import Prelude hiding (unwords)
import Debug.Trace (trace)
import TextShow (FromStringShow(..), showt)
import Control.Monad (join, filterM)
import Data.Text (Text, pack, unwords)
import Data.Set (fromList, difference)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Map.Strict (Map, insert, empty, (!))
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe)
import Text.Parsec (Parsec, ParseError, parse, eof)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (string, letter, digit, char, spaces, newline)
import Text.Parsec.Combinator (many1, sepBy1)
import Lib (Parseable(parse), multisolve)

data InputLine = InputLine { getName :: Text, getWeight :: Int, getStackedOn :: [Text]}
  deriving (Show)
  
newProg :: Text -> Int -> [Text] -> InputLine
newProg name weight children = InputLine { getName = name, getWeight = weight, getStackedOn = children }

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
mkEdges input = fmap (Edge (getName input)) (getStackedOn input) 

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

getNodeWeight :: Text -> Reader Data Int
getNodeWeight node = asks $ (! node) . getWeights

getNodeChildren :: Text -> Reader Data [Text]
getNodeChildren parent = asks $ fmap getEdgeEnd . filter startsAtParent . getEdges
  where startsAtParent = (parent ==) . getEdgeStart
  
getNodeDescendants :: Text -> Reader Data [Text]
getNodeDescendants root = do
  children <- getNodeChildren root
  (children ++) . join <$> traverse getNodeDescendants children
  
getNodeDescendantsWeight :: Text -> Reader Data Int
getNodeDescendantsWeight root = do
  descendants <- getNodeDescendants root
  sum <$> traverse getNodeWeight descendants
  
getNodeParent :: Text -> Reader Data (Maybe Text)
getNodeParent child = asks $ listToMaybe . fmap getEdgeStart . filter endsAtChild . getEdges
  where endsAtChild = (child ==) . getEdgeEnd
  
getNodeSiblings :: Text -> Reader Data [Text]
getNodeSiblings sibling = do
   parent <- getNodeParent sibling
   maybe (return []) (fmap (filter (/= sibling)) . getNodeChildren) parent

hasChildren :: Text -> Reader Data Bool
hasChildren = fmap (not . null) . getNodeChildren

correction :: Text -> Reader Data (Text, Int)
correction node = do
  sibs <- getNodeSiblings node
  weight <- getNodeWeight node
  descendantWeight <- getNodeDescendantsWeight node
  sibWeights <- traverse getNodeWeight sibs
  sibDescendantWeights <- traverse getNodeDescendantsWeight sibs
  let sibStats = zip sibWeights sibDescendantWeights
  let sibTots = uncurry (+) <$> sibStats
  let mismatched = filter (/= weight + descendantWeight) sibTots
  case mismatched of
    f:_:_ -> return (node, f - descendantWeight)
    _ -> return (node, 0)

impl2 :: Reader Data Text
impl2 = do
  nodes <- asks getNodes
  internal <- filterM hasChildren nodes
  corrections <- traverse correction internal
  let candidates = filter ((0<) . snd) corrections
  return (unwords $ fmap showt candidates)

solve2 :: Either ParseError [InputLine] -> FromStringShow (Either ParseError Text)
solve2 = FromStringShow . fmap (runReader impl2 . load)

run :: IO ()
run = multisolve [solve1, solve2]
