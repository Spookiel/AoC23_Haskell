module Day10 where
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Map as M

data Pipe = V | H | NEB | NWB | SWB | SEB | Ground | Start deriving (Show, Eq)

ppipe = choice [
    V <$ char '|',
    H <$ char '-',
    NEB <$ char 'L',
    NWB <$ char 'J',
    SWB <$ char '7',
    SEB <$ char 'F',
    Ground <$ char '.',
    Start <$ char 'S'
    ]
data Dir = U | L | R | D deriving (Show, Enum)
type Coord = (Int, Int)
move :: Coord -> Dir -> Pipe -> (Bool, Coord, Dir)

move _ _ Start = (True, (-1,-1), U)
move (x,y) U p
    | p == V = (True, (x, y-1), U)
    | p == SEB = (True,(x, y-1), R)
    | p == SWB = (True, (x, y-1), L)
    | otherwise = (False, (x, y), U)

move (x,y) D p
    | p == V = (True, (x, y+1), D)
    | p == NEB = (True,(x, y+1), R)
    | p == NWB = (True, (x, y+1), L)
    | otherwise = (False, (x, y), U)
move (x,y) L p
    | p == H = (True, (x-1, y), L)
    | p == NEB = (True, (x-1, y), U)
    | p == SEB = (True, (x-1, y), D)
    | otherwise = (False, (x, y), U)

move (x,y) R p
    | p == H = (True, (x+1, y), R)
    | p == NWB = (True, (x+1, y), U)
    | p == SWB = (True, (x+1, y), D)
    | otherwise = (False, (x, y), U)

next :: Coord -> Dir -> Coord
next (x,y) U = (x, y-1)
next (x,y) D = (x, y+1)
next (x,y) L = (x-1, y)
next (x,y) R = (x+1, y)


step :: [[Pipe]] -> Coord -> Dir -> [Coord]
step grid cur@(cx, cy) cd
    | px < 0 || py < 0 = []
    | ptype == Start = [cur]
    | canmove = cur:step grid (nx, ny) nd
    | otherwise = []
    where
        (canmove, (nx, ny), nd) = move cur cd ptype
        (px,py) = next cur cd
        ptype = (grid !! py) !! px



findStart :: [[Pipe]] -> Coord
findStart g = head [(x,y) |(row, y) <- zip g [0..], (cell, x) <- zip row [0..], cell == Start]

hcomp :: Pipe -> Pipe -> Bool
hcomp p1 p2
    | p1 == SEB = p2 `elem` valid
    | p1 == NEB = p2 `elem` valid
    | p1 == H = p2 `elem` valid
    | otherwise = False
    where
        valid = [SWB, NWB, H]

vcomp :: Pipe -> Pipe -> Bool
vcomp p1 p2 
    | p1 == V = p2 `elem` valid
    | p1 == SEB = p2 `elem` valid
    | p1 == SWB =p2 `elem` valid
    | otherwise = False
    where
        valid = [V, NEB, NWB] 


scaleRow mgrid y
    | M.null onRow = M.empty
    | otherwise = M.union res between
    where
        onRow = M.filterWithKey (\(_, ny) _ -> ny==y) mgrid
        res = M.mapKeys (\(x,y) -> (2*x, y)) onRow
        lstv = M.toList res
        between = M.fromList $ zipWith f (tail lstv) lstv
        f :: (Coord, Pipe) -> (Coord, Pipe) -> (Coord, Pipe)
        f ((x, y), p1) (_, p2)
            | hcomp p2 p1 = ((x-1, y), H)
            | otherwise = ((x-1, y), Ground)

scaleCol mgrid x
    | M.null onCol = M.empty
    | otherwise = M.union res between
    where
        onCol = M.filterWithKey (\(nx, _) _ -> nx==x) mgrid
        res = M.mapKeys (\(x,y) -> (x, 2*y)) onCol
        lstv = M.toList res
        between = M.fromList $ zipWith f (tail lstv) lstv
        f :: (Coord, Pipe) -> (Coord, Pipe) -> (Coord, Pipe)
        f ((x, y), p1) (_, p2)
            | vcomp p2 p1 = ((x, y-1), V)
            | otherwise = ((x, y-1), Ground)



toMap :: [[Pipe]] -> M.Map Coord Pipe
toMap g = M.fromList wcs
    where
        wcs = [((x,y), cell)| (row, y) <- zip g [0..length g - 1],
                 (cell, x) <- zip row [0.. length row - 1]]
    
filterPath :: M.Map Coord Pipe -> S.Set Coord -> M.Map Coord Pipe
filterPath mgrid path = M.mapWithKey f  mgrid
    where
        f :: Coord -> Pipe -> Pipe
        f k v
            | k `S.member` path = v
            | otherwise = Ground

--part2 :: [[Pipe]] -> Int 
part2 g = total
    where
        numRows = length g
        numCols = length (head g)
        start = findStart g
        path = S.fromList $ head $ allPaths g start
        gridMap = toMap g
        gridMap' = M.adjust (\x -> V) start gridMap
        filteredGridMap = filterPath gridMap' path
        scaledXGM = scaleX filteredGridMap (2*numRows)
        scaledGM =  scaleY scaledXGM (2*numCols)
        top = [(x, 0) | x <- [0.. 2*numCols-1]]
        bot = [(x, 2*numCols-1) | x <- [0.. 2*numCols-1]]
        
        left = [(0, y)| y <- [0..2*numRows-1]]
        right = [(2*numCols-1, y)| y <- [0..2*numRows-1]]
        doing = left++right++top++bot
        outsideBig = floodFill scaledGM S.empty doing
        outside = filter (\(x,y) -> even x && even y) (S.toList outsideBig)
        total = numCols*numRows -  S.size path - length outside

allPaths :: [[Pipe]] -> Coord -> [[Coord]]
allPaths grid s = [res| d <- [U, L, R, D], 
            let res = step grid s d,
            not $ null res]
    
adj :: Coord -> [Coord]
adj (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

floodFill :: M.Map Coord Pipe -> S.Set Coord -> [Coord] -> S.Set Coord
floodFill _ seen [] = seen
floodFill mapGrid seen (c@(x, y):cs)
    | c `M.member` mapGrid && mapGrid M.! c /= Ground = skip
    | c `S.member` seen = skip
    | otherwise = floodFill mapGrid (S.insert c seen) (adjs ++ cs)
    where
        adjs = filter (\x -> x `M.member` mapGrid && x `S.notMember` seen) (adj c)
        skip = floodFill mapGrid seen cs
    
scaleX :: M.Map Coord Pipe -> Int -> M.Map Coord Pipe
scaleX grid rows = M.unions [scaleRow grid y| y <- [0.. rows-1]]

scaleY :: M.Map Coord Pipe -> Int -> M.Map Coord Pipe
scaleY grid cols = M.unions [scaleCol grid x| x <- [0.. cols-1]]


toString :: Pipe -> Char
toString H = '-'
toString V = '|'
toString NEB = 'L'
toString NWB = 'J'
toString SEB = 'F'
toString SWB = '7'
toString Ground = '.'
toString Start = 'S'

outputGrid :: M.Map Coord Pipe ->String 
outputGrid grid = concat [[toString (grid M.! (x,y)) | x <- [0..cols]]++"\n" | y <- [0..rows]] 
    where
        cols = maximum (map fst $ M.keys grid) 
        rows = maximum (map snd $ M.keys grid) 

pline = some ppipe <* whitespaces
solve = do
    Success grid <- parseFromFile @String (many pline) "input/Day10.in"
    let s = findStart grid
    let p1ans = head $ map (\x -> (length x + 1) `div` 2 ) (allPaths grid s)
    let p2ans = part2 grid
    printDay 10 p1ans p2ans
-- 381 