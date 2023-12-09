module Day8 where
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import qualified Data.Map as M
import Debug.Trace
import Utils 
type Directions = String
type Label = String
data Node = N {
    left :: Label,
    right :: Label
} deriving Show
type Edge = (Label, Node)
plabel = many (upper <|> digit) <* whitespaces
pline = do
    l1 <- plabel <* char '=' <* whitespaces <* char '('
    left <- plabel <* char ',' <* whitespaces
    right <- plabel <* whitespaces <* char ')' <* whitespaces
    return (l1,N left right)
pall = do
    s <- plabel
    lines <- manyN 1 pline
    return (s, lines)


inc :: (Int, a) -> (Int, a)
inc (a,b) = (a+1,b)

step :: M.Map Label Node -> Directions -> Label -> (Label -> Bool) -> (Int , Label)
step m (d:ds) cur f
    | f cur = (0, cur)
    | d == 'L' = inc $ step m ds left f 
    | otherwise = inc $ step m ds right f
    where
        N left right = m M.! cur

part1 :: M.Map Label Node -> Directions -> Int
part1 g ds = fst $ step g ds "AAA" (== "ZZZ")

part2 g ds = foldr lcm 1 dists
    where
        starts = filter (\x -> 'A' == last x) (M.keys g)
        dists = map fst (map (\x -> step g ds x (\y -> 'Z' == last y)) starts)


solve = do
    (Success (dirs, g)) <- parseFromFile @String pall "input/Day8.in"
    let graph = M.fromList g
    let p1ans = part1 graph (cycle dirs)
    let p2ans = part2 graph (cycle dirs)
    
    
    printDay 8 p1ans p2ans
