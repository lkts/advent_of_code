module Day6.Main where

import Data.List (group, sort)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, choice, errorBundlePretty, optional, parse, some)
import Text.Megaparsec.Char

data Orientation = U | D | L | R
  deriving (Show)

newtype Guard = Guard {orientation :: Orientation}
  deriving (Show)

data Cell = G Guard | Obstacle | Free
  deriving (Show)

type Input = [[Cell]]

type Parser = Parsec Void String

cellParser :: Parser Cell
cellParser =
  choice
    [ G (Guard U) <$ char '^',
      Obstacle <$ char '#',
      Free <$ char '.'
    ]

parser :: Parser Input
parser = some line
  where
    line = some cellParser <* optional eol

findGuard :: [[Cell]] -> (Int, Int)
findGuard input =
  head
    [ (i, j)
      | (i, row) <- enumerate input,
        (j, cell) <- enumerate row,
        case cell of
          G _ -> True
          _ -> False
    ]
  where
    enumerate = zip [0 ..]

turn :: Guard -> Guard
turn g = case orientation g of
  U -> Guard R
  D -> Guard L
  L -> Guard U
  R -> Guard D

step :: Input -> Guard -> (Int, Int) -> Maybe (Guard, Int, Int)
step grid g (i, j) = do
  (fi, fj) <- Just $ case orientation g of
    U -> (i - 1, j)
    D -> (i + 1, j)
    L -> (i, j - 1)
    R -> (i, j + 1)
  row <- if fi >= 0 && fi < length grid then Just (grid !! fi) else Nothing
  in_front <- if fj >= 0 && fj < length row then Just (row !! fj) else Nothing
  return
    ( case in_front of
        Obstacle -> (turn g, i, j)
        _ -> (g, fi, fj)
    )

walk :: Input -> [(Guard, Int, Int)] -> [(Guard, Int, Int)]
walk input [] = case step input (Guard U) (i, j) of
  Just t -> walk input [t]
  Nothing -> []
  where
    (i, j) = findGuard input
walk input history = case step input g (i, j) of
  Just t -> walk input (t : history)
  Nothing -> history
  where
    (g, i, j) = head history

part1 :: Input -> Int
part1 i = 1 + length (group . sort . map (\(g, i, j) -> (i, j)) $ walk i [])

part2 :: Input -> Int
part2 i = 0

main :: IO ()
main = do
  content <- readFile "./src/Day6/input"
  let parsed = parse parser "input" content
  case parsed of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right input -> print $ part1 input