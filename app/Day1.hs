module Day1 where
import Utils
import Data.Char

words1 = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
wordsToDig = zip words1 (map show [1..])

p1line :: String -> Int
p1line xs = read r where
    r = head digs:[last digs]
    digs = filter isDigit xs

p2line :: String -> Int
p2line x = read r where
    r = firstDig:[lastDig]
    firstDig = head (filter isDigit fs)
    lastDig = last (filter isDigit bs)
    fs = replace x wordsToDig
    bs = replaceBackwards x wordsToDig

part1 = do
    inp <- splitLines <$> readFile "input/Day1.in"
    return $ sum $ map p1line inp

part2 = do
    inp <- splitLines <$> readFile "input/Day1.in"
    return $ sum $ map p2line inp
solve = do
    p1ans <- part1
    p2ans <- part2
    printDay 1 p1ans p2ans
