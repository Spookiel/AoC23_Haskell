{-# LANGUAGE TypeApplications #-}
module Day4 where
import Utils
import qualified Data.Map as M
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Debug.Trace
type Card = ([Int], [Int])

parseCard :: Parsec Card
parseCard = do
    string "Card" *> whitespaces *> sdecimal *> char ':' *> whitespaces
    ws <- manyN 1 sdecimal
    char '|'
    whitespaces
    hs <- manyN 1 sdecimal
    option (char '\n')
    return (ws, hs)

solveGame1 ws hs
    | exp == -1 = 0
    | otherwise = 2^exp
    where
        exp = countWins (ws, hs) - 1

countWins :: Card -> Int
countWins (ws, hs) = countF (`elem` ws) hs

processLine :: String -> Int 
processLine s = solveGame1 ws hs where
    Success (ws,hs) = parse @String parseCard s

solve2 :: [Card] -> Int
solve2 cards = sum $ M.elems amap
    where
        amap = foldl processCard M.empty (zip [1..] cards)    
        processCard :: M.Map Int Int -> (Int, Card) -> M.Map Int Int
        processCard cmap (i,c) = M.unionWith (+) cmap addMap
            where
                freq = M.findWithDefault 0 i cmap + 1
                morecard = countWins c
                addMap = M.fromList ((i,1):[(i+ind, freq) | ind <- [1..morecard]])

part1 = do
    let ans = 0
    lines <- getLines
    let ans = sum $ map processLine lines
    return ans

getLines :: IO [String]
getLines = splitLines <$> readFile "input/Day4.in"

part2 = do
    lis <- getLines
    let cards = map (\(Success x) -> x) $ map (parse @String parseCard) lis
    let ans = solve2 cards
    return ans

solve = do
    p1ans <- part1
    p2ans <- part2

    printDay 4 p1ans p2ans