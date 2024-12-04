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

sortLists :: ([Int], [Int]) -> [(Int, Int)]
sortLists (a, b) = zip (sort a) (sort b)

differences :: [(Int, Int)] -> [Int]
differences = map (\(a, b) -> abs (a - b)) . sortLists . unzip

solve :: [(Int, Int)] -> Int
solve = sum . differences

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
