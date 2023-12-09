module Day9 where
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator

pline = some sdecimal <* whitespace

diff xs = zipWith (-) (tail xs) xs

histories xs = takeWhile (any (/= 0)) (iterate diff xs)

forward ys x = x + last ys
back ys x = head ys - x 

predict :: ([Int] -> Int -> Int) -> [Int] -> Int
predict f xs = foldr f 0 (histories xs)

part1 hs = sum $ map (predict forward) hs
part2 hs = sum $ map (predict back) hs

solve = do
    Success lines <- parseFromFile @String (many pline) "input/Day9.in"
    let p1ans = part1 lines
    let p2ans = part2 lines
    printDay 9 p1ans p2ans