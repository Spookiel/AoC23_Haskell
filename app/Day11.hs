module Day11 where
import Utils
import Data.List
import Debug.Trace

type Coord = (Int, Int)

pairDists :: Int -> [Int] -> [Int] -> [Coord] -> Int
pairDists mul brows bcols gs = result 
    where
        sgs = sort gs
        result = compute sgs

        compute :: [Coord] -> Int
        compute [] = 0
        compute (c:cs) =  sum (map (computePair c) cs) + compute cs

        computePair :: Coord -> Coord -> Int
        computePair c1@(x1,y1) c2@(x2,y2) =  (x2-x1) + abs (y2-y1) + blankx*(mul-1) + blanky*(mul-1)
            where
                blankx = length $ filter (\p -> x1 <= p && p <= x2)  bcols
                blanky = length $ filter (\p -> min y1 y2 <= p && p <= max y1 y2) brows
            
part1 = pairDists 2
part2 = pairDists 1000000

solve = do
    rows <- splitLines <$> readFile "input/Day11.in"
    let cols = transpose rows
    let galaxies = [(x, y) | x <- [0..length cols-1], y <- [0..length rows-1], rows !! y !! x == '#']
    let blankRows = [y | y <- [0..length rows - 1], all (=='.') (rows !! y)]
    let blankCols = [x | x <- [0..length cols - 1], all (=='.') (cols !! x)]
    let p1ans = part1 blankRows blankCols galaxies
    let p2ans = part2 blankRows blankCols galaxies

    printDay 11 p1ans p2ans