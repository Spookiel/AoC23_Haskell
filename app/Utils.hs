module Utils where
import Data.List
import Data.Void
import Data.Char
import qualified Data.Map as M
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Data.Maybe
import qualified Data.Array as A

split :: String -> Char -> [String]
split s delim = go s delim [] where
    go [] _ acc = [acc]
    go (c:cs) delim acc
        | c == delim && not (null acc) = if null acc then rest else acc:rest
        | otherwise = go cs delim (acc ++ [c])
        where
            rest = go cs delim []
splitLines :: String -> [String]
splitLines s = split s '\n'

replaceInd :: [a] -> Int -> a -> [a]
replaceInd [] ind rep = [rep]
replaceInd (x:xs) ind rep
    | ind == 0 = rep:xs
    | otherwise = x: replaceInd xs (ind-1) rep

replace :: String -> [(String, String)] -> String
replace "" _ = ""
replace s@(c:cs) kvs= head cans where
    cans = [v++replace (drop (length k) s) kvs| (k,v)  <- kvs, take (length k) s == k] ++ [c:replace cs kvs]

replaceBackwards :: String -> [(String, String)] -> String
replaceBackwards s kvs = reverse ans where
    ans = replace (reverse s) [(reverse k, reverse v) | (k,v) <- kvs]

sign :: Int -> Int
sign x
    | x == 0 = 0
    | x < 0 = -1
    | otherwise = 1


printDay :: (Show a, Show b) => Int -> a -> b -> IO ()
printDay dnum p1ans p2ans = do
    putStrLn ("---------------- DAY " ++ show dnum ++ "-------------")
    putStr "Part 1: " >> print p1ans
    putStr "Part 2: " >> print p2ans

count :: Eq a => a -> [a] -> Int
count k xs = length ( filter (k ==) xs)

countF :: (a -> Bool) -> [a] -> Int
countF f xs = length $ filter (==True) (map f xs)

chunk :: Int -> [a] -> [[a]]
chunk size inp
    | length inp <= size = [inp]
    | otherwise = before:chunk size after
    where
        (before, after) = splitAt size inp

join :: String -> [String] -> String
join = intercalate

-- Gets the first n digits (LSD) from a number
digits :: Int -> Int -> [Int]
digits _ 0 = []
digits 0 x = []
digits n x = let (left, dig) = divMod x 10 in digits (n-1) left ++ [dig]

digitsToNum :: [Int] -> Int
digitsToNum ds = sum $ zipWith (*) (iterate (10*) 1) (reverse ds)

mdist :: (Int, Int) -> (Int, Int) -> Int
mdist (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)


find :: Eq a => [a] -> a -> Maybe Int
find [] _= Nothing
find xs t = go xs 0 where
    go [] _ = Nothing
    go (y:ys) ind
        | y == t = Just ind
        | otherwise = go ys (ind+1)

sdecimal :: Parsec Int
sdecimal = do
    s <- option $ char '-'
    cs <- manyN 1 digit
    spaces
    return $ read ((fromMaybe ' ' s):cs)


tabulate :: A.Ix i => (i,i) -> (i -> a) -> A.Array i a
tabulate r f = A.array r (map (\x -> (x, f x)) (A.range r))