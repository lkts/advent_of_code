import Data.List (foldl)
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Input = [[Int]]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

line :: Parser [Int]
line = some (lexeme L.decimal) <* optional eol

parser :: Parser Input
parser = some line <* eof

window :: [Int] -> [(Int, Int)]
window [] = []
window xs = zip xs (tail xs)

type SafeChanges = (Int, Int)

addChange :: (Int, Int) -> SafeChanges -> SafeChanges
addChange (l, r) (inc, dec) | r - l >= 1 && r - l <= 3 = (inc + 1, dec)
addChange (l, r) (inc, dec) | l - r >= 1 && l - r <= 3 = (inc, dec + 1)
addChange (l, r) (inc, dec) = (inc, dec)

changes :: [Int] -> SafeChanges
changes = foldl (\(inc, dec) (l, r) -> addChange (l, r) (inc, dec)) (0, 0) . window

checkReport :: [Int] -> Bool
checkReport xs = case changes xs of
  (inc, dec) -> if inc == length xs - 1 || dec == length xs - 1 then True else False

solvePart1 :: Input -> Int
solvePart1 = sum . map (\r -> if checkReport r then 1 else 0)

removeI :: Int -> [a] -> [a]
removeI _ [] = []
removeI 0 (x : xs) = xs
removeI n (x : xs) = x : removeI (n - 1) (xs)

checkReport2 :: [Int] -> Bool
checkReport2 xs = foldl (\acc i -> if checkReport $ removeI i xs then True else acc) False [0 .. length xs]

solvePart2 :: Input -> Int
solvePart2 = sum . map (\r -> if checkReport r || checkReport2 r then 1 else 0)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      let parsed = parse parser "input" content
      case parsed of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right input -> print $ solvePart2 input
    _ -> putStrLn "Wrong number of arguments"