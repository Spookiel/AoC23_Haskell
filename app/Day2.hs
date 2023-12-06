{-# LANGUAGE TypeApplications #-}
module Day2 where
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Data.Maybe
import Utils

data Colour = R | G | B deriving (Show, Eq)
data Pick = P Int Colour deriving Show
type Turn = [Pick]
data Game = Game Int [Turn] deriving Show

-- res <- string "red"
-- >
parseColour = choice
    [ R <$ string "red",
      G <$ string "green",
      B <$ string "blue"
    ]

parsePick :: Parsec Pick
parsePick = do
    q <- sdecimal
    c <- parseColour
    optional (char ',')
    whitespaces
    return $ P q c

parseTurn :: Parsec Turn
parseTurn = do
    picks <- manyN 1 parsePick
    optional $ char ';' 
    spaces
    return picks
    

parseGame :: Parsec Game
parseGame = do
    string "Game"
    whitespaces
    id <- sdecimal
    char ':'
    whitespaces
    turns <- many parseTurn
    whitespaces
    return (Game id turns)

parseFile :: Parsec [Game]
parseFile = many parseGame

turnValid :: Turn -> Bool
turnValid = all pickValid where
    pickValid (P x c)
        | c == R = x <= 12
        | c == G = x <= 13
        | otherwise = x <= 14

solveGame :: Game -> Bool
solveGame (Game id ts)= all turnValid ts

minEach :: [Turn] -> [Int]
minEach ts = res where
    res = map minForColour [R,G,B]
    minForColour :: Colour -> Int
    minForColour wc= maximum [x | P x hc <- concat ts, wc == hc]


part1 :: [Game] -> Int
part1 res = sum [id | Game id _ <- filter solveGame res]

part2 :: [Game] -> Int
part2 gs = sum $ map power gs

power :: Game -> Int
power (Game id ts) = product (minEach ts)

solve = do
    inp <- readFile "input/Day2.in"
    -- --let res = parse @String parseGame (head (splitLines inp))
    -- let res = map (parse @String parseGame) (splitLines inp)
    let (Success res) = parse @String parseFile inp
    let p1ans = part1 res
    let p2ans = part2 res

    printDay 2 p1ans p2ans