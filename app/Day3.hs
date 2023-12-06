module Day3 where
import Utils
import Data.Char

-- Store coordinates of symbols
-- Store numbers as list of coordinates
-- see if any adjacent

type Coord = (Int, Int)
data PNum = P Int [Coord] deriving Show
data Symbol = S Char Coord deriving Show

val :: PNum -> Int
val (P val _) = val
isAdj :: Coord -> Coord -> Bool
isAdj (x1,y1) (x2, y2) = abs (x1-x2) <= 1 && abs (y1-y2) <= 1

isValid :: PNum -> [Symbol] -> Bool
isValid (P _ cs) ss = or [isAdj c1 c2| c1 <- cs, S _ c2 <- ss]

gearRatios :: [Symbol] -> [PNum] -> [Int]
gearRatios ss ps = [product near| S c c1 <- ss,
                    c == '*',
                    let near = [val| P val c2 <- ps, any (isAdj c1) c2],
                    length near == 2]


parseLine :: String -> Int -> Int -> ([Symbol], [PNum])
parseLine [] _ _ = ([], [])
parseLine s x y 
    | null pnum && ns /= '.' = (S ns (x,y):ssIfS, psIfS)
    | null pnum = (ssIfS, psIfS)
    | otherwise = (ss, P (read pnum) [(x+i, y) | i <- [0..length pnum-1]] :nps)
    where     
    (ss, nps) = parseLine left (x+length pnum) y
    (ssIfS, psIfS) = parseLine leftS (x+1) y
    pnum = takeWhile isDigit s
    ([ns], leftS) = splitAt 1 s
    left = drop (length pnum) s 


getSymsAndPNums :: String -> ([Symbol], [PNum])
getSymsAndPNums inp = (asyms, apnums)
    where
        (syms, pnums) = unzip [parseLine s 0 i | (s, i) <- zip (splitLines inp) [0..]]
        asyms = concat syms
        apnums = concat pnums

part1 :: [Symbol] -> [PNum] -> Int
part1 ss ps = sum $ map val result 
    where
        result = filter (`isValid` ss) ps

part2 :: [Symbol] -> [PNum] -> Int
part2 ss ps = sum $ gearRatios ss ps

solve :: IO ()
solve = do
    inp <- readFile "input/Day3.in"
    let (ss, ps) = getSymsAndPNums inp
    let p1ans = part1 ss ps
    
    let p2ans = part2 ss ps

    printDay 3 p1ans p2ans

