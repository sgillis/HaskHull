import Text.ParserCombinators.Parsec
import Data.List

import Paths_HaskHull

csvFile :: Parser [[String]]
csvFile = endBy line eol

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser [Char]
cell = spaces >> many (noneOf ",\n")

eol :: Parser Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "" input

readInputFile :: String -> IO String
readInputFile name =
    getDataFileName name >>= readFile

main :: IO ()
main =
    readInputFile "points.txt" >>= \input ->
    either (\error -> putStrLn "something went wrong")
           (\input -> putStrLn $ intercalate "|" $ concat input)
           (parseCSV input)
