module Main where

import Data.List (foldl, sort, zip)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

parseInts :: String -> Maybe (Int, Int)
parseInts s =
  case words s of
    [num1, num2] -> Just (read num1, read num2)
    _ -> Nothing

parse :: String -> [(Int, Int)]
parse = mapMaybe parseInts . lines

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

scores (a, b) = map (\i -> i * count i b) a

solve :: [(Int, Int)] -> Int
solve = sum . scores . unzip

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      let parsed = parse content
      let solution = solve parsed
      print solution
    _ -> putStrLn "Wrong number of arguments"
