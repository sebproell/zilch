module Zilch where

import Data.List ( sort, group )

import qualified Data.Map as Map

type Die = Int

diceValues :: [Die]
diceValues = [1..6]

data Action = Action {points :: Int, takeDice :: [Die]} deriving (Show, Eq, Ord)

instance Semigroup Action where
    Action p1 d1 <> Action p2 d2  = Action (p1 + p2) (d1 ++ d2)

instance Monoid Action where
    mempty = Action 0 []

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

unique :: Ord a => [a] -> [a]
unique = map head . group . sort

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

isStreet :: [Die] -> Bool
isStreet ds = diceValues == sort ds

isNothing :: [Die] -> Bool
isNothing ds
    | length ds /= 6 = False
    | isStreet ds = False
    | otherwise = all (\d -> tooFew d (count d ds)) diceValues
        where tooFew 1 c = c == 0
              tooFew 5 c = c == 0
              tooFew d c = c < 3

isThreePair :: [Die] -> Bool
isThreePair ds
    | length ds /=6 = False
    | otherwise = all (\xs -> length xs == 2 || length xs == 4 || length xs == 6) . group . sort $ ds


multiplyBase :: Die -> Int -> Int -> Action
multiplyBase d base n = Action (base*2^(n-3)) (replicate n d) 

scoreMultiples :: Die -> Int -> Action
scoreMultiples 1 n = if n < 3 then Action (n*100) (replicate n 1) else multiplyBase 1 1000 n
scoreMultiples 5 n = if n < 3 then Action (n*50) (replicate n 5) else multiplyBase 5 500 n
scoreMultiples d n = if n >= 3 then multiplyBase d (d*100) n else mempty


scoreSpecial :: [Die] -> Action
scoreSpecial ds 
    | isStreet ds = Action 1500 ds
    | isNothing ds = Action 500 ds
    | isThreePair ds = Action 1500 ds
    | otherwise = mempty

scoreSet' :: [Die] -> Action
scoreSet' ds = max (mconcat . map scoreDie $ [1,2,3,4,5,6]) (scoreSpecial ds)
    where scoreDie d = scoreMultiples d (count d ds)

-- Compute the Action that corresponds to taking a given subset
-- The whole subset needs to be taken, if this is not possible,
-- the Action will be mempty
scoreSet :: [Die] -> Action
scoreSet ds = onlyIfAllDiceTaken . scoreSet' $ ds
    where onlyIfAllDiceTaken a | length (takeDice a) == length ds = a
                               | otherwise = mempty

-- All the Actions that can be taken for a given list of dice
actions :: [Die] -> [Action]
actions = filterInvalid . unique . map scoreSet . subsets
    where filterInvalid [mempty] = [mempty]
          filterInvalid actions = filter (/= mempty) actions


data GameState = GameState {score :: Int, availableDice :: Int} deriving (Show, Eq, Ord)

data Transition = Transition {probability :: Double, newState :: GameState} deriving (Show)

allThrows :: Int -> [[Die]]
allThrows 0 = [[]]
allThrows n = [d:xs | d <- diceValues, xs <- allThrows (n-1)]

allActions :: Int -> [[Action]]
allActions =  map actions . allThrows

takeAction :: GameState -> Action -> GameState
takeAction g (Action 0 []) = GameState 0 0
takeAction (GameState s r) (Action p ds) = GameState (p+s) (newDiceCount $ r - length ds)
    where newDiceCount a = if a == 0 then 6 else a 

takeAllActions :: GameState -> [[GameState]]
takeAllActions g@(GameState s n) = (map . map) (takeAction g) $ allActions n

throwProbability :: Int -> Double
throwProbability n = 1.0/(6.0^n)

transitionMap :: GameState -> Map.Map GameState Integer
transitionMap = foldl (\m gs -> Map.insertWith (+) gs 1 m) Map.empty . concat . takeAllActions

probabilityMap :: GameState -> Map.Map GameState Double
probabilityMap g@(GameState s n) = fmap (\c -> fromIntegral c * throwProbability n) . transitionMap $ g

mapForScore :: Int -> Int -> Map.Map GameState Double
mapForScore n s = probabilityMap $ GameState s n
