import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many)

import Paths_HaskHull

data Point = Point
    { x :: Double
    , y :: Double
    , z :: Double
    } deriving Show

csvFile :: Parser [Point]
csvFile = endBy point eol

point :: Parser Point
point = Point <$> double <*> (char ',' *> double) <*> (char ',' *> double)

line :: Parser [Double]
line = sepBy double (char ',')

eol :: Parser Char
eol = char '\n'

(<++>) :: Applicative f => f [a] -> f [a] -> f[a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

number :: Parser String
number = many1 digit

integer :: Parser String
integer = plus <|> minus <|> number

double :: Parser Double
double = read <$> (spaces *> integer <++> decimal)
    where decimal = option "" $ char '.' <:> number

parseCSV :: String -> Either ParseError [Point]
parseCSV = parse csvFile ""

readInputFile :: String -> IO String
readInputFile name =
    getDataFileName name >>= readFile

main :: IO ()
main = do
    input <- readInputFile "points.txt"
    let parseResult = parseCSV input
    either (\error -> putStrLn "something went wrong")
           print parseResult
