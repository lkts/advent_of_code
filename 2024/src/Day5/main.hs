{-# LANGUAGE RecordWildCards #-}

module Day5.Main where

import Control.Monad (void)
import Data.List (filter, isPrefixOf, tails, transpose)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Arr (indices)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Rule = Rule {before :: Int, after :: Int}
  deriving (Show)

data Input = Input {rules :: [Rule], updates :: [[Int]]}
  deriving (Show)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

ruleParser :: Parser Rule
ruleParser = do
  l <- lexeme L.decimal
  void (char '|')
  r <- lexeme L.decimal
  return Rule {before = l, after = r}

updateParser :: Parser [Int]
updateParser = some $ lexeme L.decimal <* optional (char ',')

parser :: Parser Input
parser = do
  rules <- some (ruleParser <* optional eol)
  void <- newline
  updates <- some (updateParser <* optional eol)
  return Input {..}

type ComesAfter = Map.Map Int [Int]

buildComesAfter :: [Rule] -> ComesAfter
buildComesAfter = Map.fromListWith (++) . map (\r -> (after r, [before r]))

isValidSequence :: ComesAfter -> [Int] -> Bool
isValidSequence ca xs = all (\(i, n) -> everythingBeforeIsCorrect (take i xs) (Map.findWithDefault [] n ca)) $ zip [0 ..] xs
  where
    everythingBeforeIsCorrect :: [Int] -> [Int] -> Bool
    everythingBeforeIsCorrect xs bs = all (`elem` bs) xs

part1 :: Input -> Int
part1 i = sum . map (\l -> l !! (length l `div` 2)) $ filter (isValidSequence comesAfter) (updates i)
  where
    comesAfter = buildComesAfter $ rules i

part2 _ = 0

main :: IO ()
main = do
  content <- readFile "./src/Day5/input"
  let parsed = parse parser "input" content
  case parsed of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right input -> print $ part1 input