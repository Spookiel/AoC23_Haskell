module Day12 where
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import qualified Data.Array as A
import Data.List
import Debug.Trace
-- U -> ?
-- S -? .
-- D -> #
data Ground = U | S | D deriving (Ord, Show, Eq)

pground = choice [
    U <$ char '?',
    S <$ char '.',
    D <$ char '#'
    ] 

pline = do
    record <- some pground
    whitespaces
    groups <- sepBy sdecimal (char ',')
    whitespaces
    return (record, groups)

dp :: [Ground] -> [Int] -> Int
dp gs ss = table A.! (n, m) 
    where
        n = length gs 
        m = length ss
        table :: A.Array (Int, Int) Int
        table = tabulate ((0,0), (n,m)) (uncurry memo)
        memo :: Int -> Int -> Int
        memo i 0
            | D `notElem` drop (n-i) gs = 1
            | otherwise = 0

        memo 0 j = 0
        memo i j
            | s > i = 0
            | l == S = cont
            | l== U = cont + putD
            | l == D = putD
            | otherwise = 0
            where
                before = take s (drop (n-i) gs)
                match = S `notElem` before && (i-s == 0 || gsA A.! (n-i+s) /= D)
                putD = if match then table A.! (max (i-s-1) 0, j-1) else 0
                cont = table A.! (i-1, j)
                s = ssA A.! (m-j)
                l = gsA A.! (n-i)
        gsA = A.listArray (0, n-1) gs
        ssA = A.listArray (0, m-1) ss

dup :: Int -> a -> [a] -> [a]
dup x o ls = intercalate [o] (replicate x ls)

solve = do
    Success res <- parseFromFile @String (some pline) "input/Day12.in"
    --print res
    --let ans = sum  $ map (uncurry solveNaive) res
    let ans1 = sum $ map (uncurry dp) res

    let ans2 = sum $ map (\(x, y) -> dp (dup 5 U x) (concat $ replicate 5 y)) res
    --print diff
    printDay 12 ans1 ans2
