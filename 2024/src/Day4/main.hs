module Day4.Main where

import Data.List (isPrefixOf, tails, transpose)
import Data.Void (Void)
import GHC.Arr (indices)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), Parsec, anySingle, errorBundlePretty, parse, some)

type Input = [[Char]]

countXmas :: [Char] -> Int
countXmas = length . filter (\s -> isXmas s || isXmasReversed s) . tails
  where
    isXmas = ("XMAS" `isPrefixOf`)
    isXmasReversed = ("SAMX" `isPrefixOf`)

horizontal :: Input -> Int
horizontal i = sum $ map countXmas i

vertical = horizontal . transpose

diagonal :: Input -> Int -> Int -> [Char]
diagonal [] _ _ = []
diagonal xs i j | i >= length xs = []
diagonal xs i j | j >= length a = []
  where
    a = xs !! i
diagonal xs i j = a !! j : diagonal xs (i + 1) (j + 1)
  where
    a = xs !! i

diagonals :: Input -> [[Char]]
diagonals input = filter (\s -> length s > 3) $ map (uncurry (diagonal input)) (firstRow ++ firstColumn)
  where
    firstRow = [(0, j) | j <- [0 .. length (head input)]]
    firstColumn = [(i, 0) | i <- [1 .. length input]]

onDiagonal :: Input -> Int
onDiagonal = horizontal . diagonals

onLeftDiagonal :: Input -> Int
onLeftDiagonal = horizontal . diagonals . map reverse

part1 :: Input -> Int
part1 i = horizontal i + vertical i + onDiagonal i + onLeftDiagonal i

submatrices :: [[a]] -> Int -> [[[a]]]
submatrices m s | s > length m = []
submatrices m s | s > length (head m) = []
submatrices m s = concatMap (`doRow` 0) rows
  where
    rows = [0 .. length m - 1]
    doRow i j | length m - i < s = []
    doRow i j | length (m !! i) - j < s = []
    doRow i j = map (take s . drop j) (take s . drop i $ m) : doRow i (j + 1)

part2 :: Input -> Int
part2 i = sum $ map match (submatrices i 3)
  where
    match :: [[Char]] -> Int
    match
      [ [tl, _, tr],
        [_, 'A', _],
        [bl, _, br]
        ]
        | (tl, br, tr, bl)
            `elem` [ ('M', 'S', 'M', 'S'),
                     ('S', 'M', 'S', 'M'),
                     ('S', 'M', 'M', 'S'),
                     ('M', 'S', 'S', 'M')
                   ] =
            1
    match _ = 0

main :: IO ()
main = do
  parsed <- lines <$> readFile "./src/Day4/input"
  print $ part1 parsed
  print $ part2 parsed