module Day19 where

import qualified Data.Map as M
import Utils
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Debug.Trace

data Cat = X | M | A | S deriving (Show, Ord, Enum, Eq)
data Part = P {
    x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
    } deriving (Show, Eq)

data Label = Accept | Reject | OW String deriving (Show, Eq, Ord)
data Rule = Cond Cat Ordering Int Label | Goto Label deriving Show
type Workflow = (Label,[Rule])
type System = M.Map Label [Rule]

-- Stored as [) form
type Range = (Int, Int)


size :: Range -> Int
size (low, high) = max 0 (high-low)

inRange :: Range -> Int -> Bool
inRange (low, high) v = low <= v && v < high

belowRange :: Range -> Int -> Bool
belowRange (low, high) v = v < low


-- Returns a pair of ranges
-- First is the range that satisfies the condition
-- Second is the range that doesn't 
-- (True, False)
applyCond :: Range -> Ordering -> Int -> (Range, Range)
applyCond r@(l, h) LT v
    | belowRange r v = ((0,0),r)
    | inRange r v = ((l, v),(v, h))
    | otherwise = (r, (0,0))

applyCond r@(l,h) GT v 
    | belowRange r v = (r, (0,0))
    | inRange r v =  ((v+1,h), (l, v+1))
    | otherwise = ((0,0), r)
    where
        fempty = v+1 >= h

pcat :: Parsec Cat
pcat = choice [
    X <$ char 'x',
    M <$ char 'm',
    A <$ char 'a',
    S <$ char 's'
    ]

pord :: Parsec Ordering
pord = choice [
    LT <$ char '<',
    GT <$ char '>'
    ]

plabel :: Parsec Label
plabel = Accept <$ char 'A' <|> Reject <$ char 'R' <|> (pure OW <*> some lower)

pcond :: Parsec (Label -> Rule)
pcond = pure Cond <*> pcat <*> pord <*> sdecimal <* char ':'

prule = ((atomic pcond) <|> pure Goto) <*> plabel

pworkflow :: Parsec Workflow
pworkflow = plabel <* char '{' <~> sepBy prule (char ',') <* char '}' <* whitespaces

ppart :: Parsec Part
ppart = do
    char '{'
    [x,m,a,s] <- sepBy (lower *> char '=' *> sdecimal) (char ',')
    char '}' <* whitespaces
    return (P x m a s)

instance MonadFail Parsec where
    fail _ = empty

----- PARSING DONE ------

extract :: Part -> Cat -> Int
extract part X = x part
extract part M = m part
extract part A = a part
extract part S = s part

partScore :: Part -> Int
partScore (P x m a s) = x+m+a+s

getLabel :: Rule -> Label
getLabel (Cond _ _ _ label) = label
getLabel (Goto label) = label

isGoto :: Rule -> Bool
isGoto (Goto _) = True
isGoto _ = False

ruleApplies :: Part -> Rule -> Bool
ruleApplies part (Goto _) = True
ruleApplies part (Cond category ord val _) = compare pval val == ord
    where
        pval = extract part category

isEndState :: Label -> Bool
isEndState label = label == Accept || label == Reject

processWorkflow :: System -> [Rule] -> Part -> Bool
processWorkflow sys (r:rs) part
    | not (ruleApplies part r) = processWorkflow sys rs part
    | isEndState newlabel = newlabel == Accept
    | Goto label <- r = processWorkflow sys newrules part
    | otherwise = processWorkflow sys newrules part
    where
        newlabel :: Label = getLabel r
        newrules :: [Rule] = sys M.! newlabel


part1 :: System -> [Part] -> Int
part1 sys ps = sum $ map partScore accepted
    where
        start = sys M.! (OW "in")
        accepted = filter (processWorkflow sys start) ps

--- PART 1 DONE ---- 


getRange :: [Range] -> Cat -> Range
getRange rs X = head rs
getRange rs M = rs !! 1
getRange rs A = rs !! 2
getRange rs S = rs !! 3

replaceRange :: [Range] -> Range -> Cat -> [Range]
replaceRange rs nr X = replaceInd rs 0 nr
replaceRange rs nr M = replaceInd rs 1 nr
replaceRange rs nr A = replaceInd rs 2 nr
replaceRange rs nr S = replaceInd rs 3 nr


scoreRanges :: [Range] -> Int
scoreRanges rs = product (map size rs)

countAccept :: System -> [Rule] -> [Range] -> Int
countAccept sys [] rgs = 0 -- Need this for the goto reject countIfFail case
countAccept sys (r:rs) rgs
    | newlabel == Accept = scoreRanges leftRange + countIfFail
    | newlabel == Reject = countIfFail
    | isGoto r = countAccept sys leftRules rgs
    | otherwise = countIfSat + countIfFail  
    where
        newlabel :: Label = getLabel r 
        leftRules :: [Rule] =  sys M.! newlabel
        
        (leftRange, rightRange) = validRange r rgs

        countIfFail = countAccept sys rs rightRange
        countIfSat = countAccept sys leftRules leftRange

validRange :: Rule -> [Range] -> ([Range], [Range])
validRange (Goto _) rs = (rs, replicate 4 (0,0))
validRange (Cond cat ord val _) rs = ( replaceRange rs v cat, replaceRange rs nv cat)
    where
        (v, nv) =  applyCond (getRange rs cat) ord val


sranges = replicate 4 (1,4001)

part2 :: System -> Int
part2 sys = countAccept sys (sys M.! (OW "in")) sranges

solve = do
    Success (wfs, ps) <- parseFromFile @String (some pworkflow <~> some ppart) "input/Day19.in"
    let mainsys = M.fromList wfs
    let p1ans = part1 mainsys ps
    let p2ans = part2 mainsys 

    printDay 19 p1ans p2ans
