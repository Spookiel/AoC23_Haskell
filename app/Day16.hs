module Day16 where
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import qualified Data.Array as A
import qualified Data.Set as S
import Data.List
import Utils
import Debug.Trace

data Cell = E | F | B | V | H deriving (Show, Eq)
data Dir = U | D | L | R deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Grid = A.Array Coord Cell
type Seen = S.Set (Coord, Dir)


pcell = choice [
    E <$ char '.',
    F <$ char '/',
    B <$ char '\\',
    V <$ char '|',
    H <$ char '-']

inGrid :: Grid -> Coord -> Bool
inGrid g c = A.inRange (A.bounds g) c  

next :: Coord -> Dir -> Coord
next (x,y) U = (x, y-1)
next (x,y) D = (x, y+1)
next (x,y) L = (x-1, y)
next (x,y) R = (x+1, y)

handleF :: Dir -> [Dir]
handleF L = [D]
handleF R = [U]
handleF U = [R]
handleF D = [L]

handleB :: Dir -> [Dir]
handleB L = [U]
handleB R = [D]
handleB U = [L]
handleB D = [R]

nextDir :: Cell -> Dir ->[Dir]
nextDir curCell cd
    | curCell == E = [cd]
    | curCell == H = if cd == L || cd == R then [cd] else [L, R]
    | curCell == V = if cd == U || cd == D then [cd] else [U,D]
    | curCell == F = handleF cd
    | otherwise = handleB cd

stepLight :: Grid -> Seen -> [(Coord, Dir)] -> [Coord]
stepLight _ seen [] = S.toList (S.map fst seen)
stepLight grid seen (cd@(curPos, curDir):rest) 
    | inGrid grid curPos && cd `S.notMember` seen = stepLight grid newSeen (todo ++ rest)
    | otherwise =  stepLight grid seen rest
    where
        newSeen = S.insert (curPos, curDir) seen
        curCell = grid A.! curPos
        todo = [(next curPos d, d) |d <- nextDir curCell curDir]

pline = some pcell <* whitespaces

part1 :: Grid -> Int
part1 grid = length $ stepLight grid S.empty [((0,0),R)]

part2 :: Grid -> Int
part2 grid = maximum (map length (top ++ bot ++ left ++ right) )
    where
        top = [stepLight grid S.empty [((i, 0), D)] | i <- [0..width]]
        bot = [stepLight grid S.empty [((i, height), U)] | i <- [0..width]]
        right = [stepLight grid S.empty [((width, i), R)] | i <- [0..height]]
        left = [stepLight grid S.empty [((0, i), L)] | i <- [0..height]]
        (_, (width, height)) = A.bounds grid

solve = do
    Success res <- parseFromFile @String (some pline) "input/Day16.in"
    let height = length res
    let width = length (head res)

    -- Coord in (x,y) form
    let grid = A.listArray ((0,0), (width-1, height-1)) (concat (transpose res))
    let p1ans = part1 grid
    let p2ans = part2 grid
    printDay 16 p1ans p2ans
    --print (ans)
    --print grid