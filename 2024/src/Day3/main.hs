{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Day3.Main where

import Control.Arrow (ArrowChoice (right))
import Control.Concurrent (yield)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Void
import GHC.Arr (accum)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Instruction
  = Mul Int Int
  | Do
  | Dont
  deriving stock (Show, Eq)

type Input = [Instruction]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

mul :: Parser Instruction
mul = do
  _ <- string "mul"
  void (char '(')
  l <- lexeme L.decimal
  void (char ',')
  r <- lexeme L.decimal
  void (char ')')
  return (Mul l r)

item :: Parser (Maybe Instruction)
item =
  choice
    [ Just <$> try mul,
      Just Do <$ try (string "do()"),
      Just Dont <$ try (string "don't()"),
      anySingle $> Nothing
    ]

parser :: Parser Input
parser = catMaybes <$> some item <* eof

value (Mul x y) = x * y
value _ = 0

part1 :: Input -> Int
part1 = foldl' (\acc i -> acc + value i) 0

part2' [] acc _ = acc
part2' ((Mul x y) : tail) acc enabled = if enabled then part2' tail (acc + x * y) enabled else part2' tail acc enabled
part2' (Do : tail) acc _ = part2' tail acc True
part2' (Dont : tail) acc _ = part2' tail acc False

part2 :: Input -> Int
part2 xs = part2' xs 0 True

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      let parsed = parse parser "input" content
      case parsed of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right input -> print $ part2 input
    _ -> putStrLn "Wrong number of arguments"