module Day5 where
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Char
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Debug.Trace
data MType = Seed | Soil | Fert | Water | Light | Temp | Humid | Loc deriving (Show, Eq)
data Range = R Int Int Int deriving Show
data Interval = I Int Int deriving (Show, Eq)
data NeedMap = NM 
    { stype :: MType,
      dtype :: MType,
      ranges :: [Range]
    } deriving Show

parseAll :: Parsec ([Int], [NeedMap])
parseAll = do
    seeds <- string "seeds:" *> whitespace *> manyN 1 sdecimal
    whitespaces
    nms <- manyN 1 (parseMap <* whitespaces)
    return (seeds, nms)

parseMType :: Parsec MType
parseMType = choice
    [atomic $ Seed <$ string "seed",
        atomic $ Soil <$ string "soil",
        Fert <$ string "fertilizer",
        Water <$ string "water",
        atomic $ Light <$ string "light",
        Temp <$ string "temperature",
        Humid <$ string "humidity",
        atomic $ Loc <$ string "location"
    ]
parseRange :: Parsec Range
parseRange = do
    [p1, p2, p3] <- exactly 3 sdecimal
    return $ R p1 p2 p3

    -- R <$> sdecimal <*>
--missing monad fail instance
instance MonadFail Parsec where
    fail _ = empty

parseMap :: Parsec NeedMap
parseMap = do
    t1 <- parseMType
    string "-to-"
    t2 <- parseMType
    whitespaces *> string "map:" *> whitespaces
    ranges <- manyN 1 (parseRange <* endOfLine)
    return $ NM t1 t2 ranges

-- [81, 95] -> 18 25 70 -> [74, 88]
breakInterval :: Interval -> Range -> ([Interval], [Interval])
breakInterval oi@(I s e) (R dest src size)
    | e <= src || s >= src+size = ([oi], [])
    | sin && ein = ([], [mid] )
    | sin && (not ein) = ([ I (src+size) e], [I (s-diff) (src+size-diff)])
    | not sin && ein = ([I s src], [I (src-diff) (e-diff)])
    | otherwise = ([I s src,  I (src+size) e],  [I (src-diff) (src+size-diff)])
    where
        diff = src-dest
        ein = src < e && e <= (src+size)
        sin = src <= s && s < (src+size)
        
        mid = I (s-diff) (e-diff)
sortRangesDesc :: [Range] -> [Range]
sortRangesDesc = sortBy (\(R _ st1 sz1) (R _ st2 sz2) -> compare (st2+sz2) (st1+sz1))

applyRanges :: Interval -> [Range] -> [Interval]
applyRanges int [] = [int]
applyRanges int (r:rs) =  done++rest
    where
        (todo, done) = breakInterval int r
        rest = concatMap (\x -> applyRanges x rs) todo


--sepEndBy1
inRange :: Int -> Range -> Bool
inRange x (R dest start size)
    | x-start < 0 || x-start >= size = False
    | otherwise = True

findRanges :: Int -> [Range] -> Maybe Range
findRanges x [] = Nothing
findRanges x (r:rs)
    | inRange x r = Just r
    | otherwise = findRanges x rs 

applyRange :: Int -> Maybe Range -> Int
applyRange x Nothing = x
applyRange x (Just (R outRange inRange _)) = outRange + (x-inRange)

stepRange :: MType -> [Interval] -> [NeedMap] -> (MType, [Interval])
stepRange Loc ints _ = (Loc, ints)
stepRange ctype ints nms = stepRange dtype newInts nms
    where
        (NM stype dtype ranges) = head $ filter (\(NM x _ _) -> x == ctype) nms
        sranges = sortRangesDesc ranges
        newInts = concatMap (`applyRanges` sranges) ints
    
step :: MType -> [Int] -> [NeedMap] -> (MType, [Int])
step Loc ss nms = (Loc, ss)
step ctype ss nms = step dtype nvs nms 
    where
        (NM stype dtype ranges) = head $ filter (\(NM x _ _) -> x == ctype) nms

        nvs = map (\x -> applyRange x (findRanges x ranges)) ss

part1 seeds nms= minimum nvs
    where
        (Loc, nvs) = step Seed seeds nms

part2 seeds nms = minimum [x | (I x _) <- ints]
    where
        pairs = chunk 2 seeds
        seeds' = [I a (a+x) | [a,x] <- pairs]
        (Loc, ints) = stepRange Seed seeds' nms

solve = do
    inp <- readFile "input/Day5.in"
    let (Success (seeds, nms)) = parse @String parseAll inp
    let p1ans = part1 seeds nms
    let p2ans = part2 seeds nms
    printDay 5 p1ans p2ans
    
