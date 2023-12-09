module Day7 where
import Utils
import Data.List
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator hiding (count)
import Debug.Trace
data Card = Joker | One | Two | Three | Four | 
            Five | Six | Seven | Eight | Nine |
            T | J | Q | K | A deriving (Show, Eq, Ord, Enum)
data Type = High | OP | TP | ToK | FH | FourKind | FiveKind deriving (Show, Eq, Ord, Enum)
type Hand = [Card]


-- Idea is that the jokers can be added to the most frequent
-- card to make the strongest possible hand
countCards :: Hand -> [Int]
countCards cs 
    | null freqs = [jokers]
    | otherwise = (f+jokers):sfs
    where
        (f:sfs) = reverse (sort freqs) 
        freqs = map (`count` cs) (nub (filter (/= Joker) cs))
        jokers = Joker `count` cs
    
pcard :: Parsec Card
pcard = choice [
    One <$ char '1',
    Two <$  char '2',
    Three <$ char '3',
    Four <$ char '4',
    Five <$ char '5',
    Six <$ char '6',
    Seven <$ char '7',
    Eight <$ char '8',
    Nine <$ char '9',
    T <$ char 'T',
    Q <$ char 'Q',
    K <$ char 'K',
    A <$ char 'A']

p1cards = pcard <|> (J <$ char 'J')
p2cards = pcard <|> (Joker <$ char 'J')

phand :: Parsec Card -> Parsec Hand
phand pcard = many pcard <* whitespaces

pline :: Parsec Hand ->  Parsec (Hand, Int)
pline phand= do
    hand <- phand <* whitespaces
    bid <- sdecimal <* whitespaces
    return (hand, bid)

fiveOfKind :: Hand -> Bool
fiveOfKind cs = head (countCards cs) == 5

fourOfKind :: Hand -> Bool
fourOfKind cs = head (countCards cs) == 4

fullHouse :: Hand -> Bool
fullHouse cs = countCards cs == [3,2]

threeOfKind :: Hand -> Bool
threeOfKind cs = head (countCards cs) == 3

twoPair :: Hand -> Bool
twoPair cs = take 2 (countCards cs) == [2,2]

onePair :: Hand -> Bool
onePair cs = head (countCards cs) == 2

findType :: Hand -> Type
findType cs
    | fiveOfKind cs = FiveKind
    | fourOfKind cs = FourKind
    | fullHouse cs = FH
    | threeOfKind cs = ToK
    | twoPair cs = TP
    | onePair cs = OP
    | otherwise = High

byValue :: Hand -> Hand -> Ordering
byValue [] [] = EQ
byValue (c1:cs1) (c2:cs2)
    | c /=  EQ = c
    | otherwise = byValue cs1 cs2
    where
        c = compare c1 c2 
compareHand :: Hand -> Hand -> Ordering
compareHand h1 h2
    | c /= EQ = c
    | otherwise = byValue h1 h2
    where
        t1 = findType h1
        t2 = findType h2
        c = compare t1 t2

part1 :: [(Hand, Int)] -> Int
part1 his = sum $ zipWith (*) [1..] (map snd sortedHands)
    where
        sortedHands = sortBy (\(h1, _) (h2, _) -> compareHand h1 h2) his
part2 = part1

solve = do
    (Success p1cards) <- parseFromFile @String (many (pline (phand p1cards))) "input/Day7.in"
    (Success p2cards) <- parseFromFile @String (many (pline (phand p2cards))) "input/Day7.in"
    let p1ans = part1 p1cards
    let p2ans = part2 p2cards
    
    printDay 7 p1ans p2ans

