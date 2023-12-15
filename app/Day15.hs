module Day15 where

import Utils
import Data.Char
import Debug.Trace
import Text.Gigaparsec
import Text.Gigaparsec.Char
import qualified Data.Map as M
import Text.Gigaparsec.Combinator


type Label = String
data Operation = Rem Label | Ins Label Int deriving (Show, Eq)
type BoxLens = (Label, Int)

prem :: Parsec (Label -> Operation)
prem = char '-' $> Rem

pins :: Parsec (Label -> Operation)
pins = (char '=' *> sdecimal) <**> pure (flip Ins)

pop :: Parsec Operation
pop = some letter <**> (prem <|> pins)

repr :: Operation -> Label
repr (Rem la) = la++"-"
repr (Ins la d) = la++"="++show d

--Store box in correct order, so last thing is at the back

applyOperation :: M.Map Int [BoxLens] -> Operation -> M.Map Int [BoxLens]
applyOperation state (Rem label) = M.adjust (filter (\y -> label /= fst y)) (hash label) state

applyOperation state (Ins label focus) = M.insert key newBox state
    where
        key = hash label
        relBox = M.findWithDefault [] key state
        newBox = update (label, focus) relBox
        
focusPower :: M.Map Int [BoxLens] -> Int
focusPower state = sum [(k+1) * sum (zipWith (*) [1..] (map snd v))| (k,v) <- M.toList state]

hash :: Label -> Int
hash s = go s 0
    where
        go :: Label -> Int -> Int
        go "" acc = acc
        go (c:cs) acc = go cs (((acc+ord c) * 17) `mod` 256)

part2 :: [Operation] -> Int
part2 = focusPower . foldl applyOperation M.empty

solve = do
    Success ops <- parseFromFile @String (sepBy1 pop (char ',')) "input/Day15.in"
    let p1ans = sum $ map (hash . repr) ops
    let p2ans = part2 ops
    
    printDay 15 p1ans p2ans