module Day6 where
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
type Time = Int
type Dist = Int
type HoldTime = Int
type Race = (Int, Int)

ptimes = string "Time:" *> whitespaces *> many sdecimal
pdists = string "Distance:" *> whitespaces *> many sdecimal

pall :: Parsec ([Time], [Dist])
pall = do
    times <- ptimes <* endOfLine
    dists <- pdists
    optional whitespaces
    return (times,dists)

calc :: HoldTime -> Time -> Dist
calc ht t = max 0 (ht * (t-ht))

brute :: Race -> [Int]
brute (t, d)= filter (\x -> calc x t > d) [1..t]


part1 :: [Race] -> Int
part1 rs = product (map (length . brute) rs)

part2 :: [Race] -> Int
part2 rs = length $ brute (tt, td) 
    where
        (times, dists) = unzip rs
        tt = read $ concatMap show times
        td = read $ concatMap show dists

solve :: IO ()
solve = do
    (Success (times, dists)) <- parseFromFile @String pall "input/Day6.in"
    let races = zip times dists
    let p1ans = part1 races
    let p2ans = part2 races

    printDay 6 p1ans p2ans