module Zilch where

import Data.List ( sort, group )

import qualified Data.Map as Map

type Die = Int

diceValues :: [Die]
diceValues = [1..6]

data Action = Action {points :: Int, takeDice :: Int} deriving (Show, Eq, Ord)

instance Semigroup Action where
    Action p1 d1 <> Action p2 d2  = Action (p1 + p2) (d1 + d2)

instance Monoid Action where
    mempty = Action 0 0

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
multiplyBase d base n = Action (base*2^(n-3)) n

scoreMultiples :: Die -> Int -> Action
scoreMultiples 1 n = if n < 3 then Action (n*100) n else multiplyBase 1 1000 n
scoreMultiples 5 n = if n < 3 then Action (n*50) n else multiplyBase 5 500 n
scoreMultiples d n = if n >= 3 then multiplyBase d (d*100) n else mempty


scoreSpecial :: [Die] -> Action
scoreSpecial ds 
    | isStreet ds = Action 1500 6
    | isNothing ds = Action 500 6
    | isThreePair ds = Action 1500 6
    | otherwise = mempty

scoreSet' :: [Die] -> Action
scoreSet' ds = max (mconcat . map scoreDie $ [1,2,3,4,5,6]) (scoreSpecial ds)
    where scoreDie d = scoreMultiples d (count d ds)

-- Compute the Action that corresponds to taking a given subset
-- The whole subset needs to be taken, if this is not possible,
-- the Action will be mempty
scoreSet :: [Die] -> Action
scoreSet ds = onlyIfAllDiceTaken . scoreSet' $ ds
    where onlyIfAllDiceTaken a | takeDice a == length ds = a
                               | otherwise = mempty


-- All the Actions that can be taken for a given list of dice
actions :: [Die] -> [Action]
actions = filterInvalid . unique . map scoreSet . subsets
    where filterInvalid [mempty] = [mempty]
          filterInvalid actions = filter (/= mempty) actions


data GameState = GameState {score :: Double, availableDice :: Int} deriving (Show, Eq)

data DecisionTree = MultiNode [DecisionTree]
                  | Node GameState [DecisionTree] deriving Show

instance Ord GameState where
    a <= b = score a <= score b

allThrows :: Int -> [[Die]]
allThrows 0 = [[]]
allThrows n = [d:xs | d <- diceValues, xs <- allThrows (n-1)]

memoize :: (Int -> [[Action]]) -> (Int -> [[Action]])
memoize f = (map f [0 ..] !!)

allActions' :: Int -> [[Action]]
allActions' =  map actions . allThrows

allActions :: Int -> [[Action]]
allActions = memoize allActions'

takeAction :: GameState -> Action -> GameState
takeAction g (Action 0 0) = GameState 0 0
takeAction (GameState s r) (Action p ds) = GameState (fromIntegral p+s) (newDiceCount $ r - ds)
    where newDiceCount a = if a == 0 then 6 else a 

takeAllActions :: GameState -> [[GameState]]
takeAllActions g@(GameState s n) = (map . map) (takeAction g) $ allActions n

throwProbability :: Int -> Double
throwProbability n = 1.0/(fromIntegral (length diceValues)^n)

transitionMap :: GameState -> Map.Map GameState Integer
transitionMap = foldl (\m gs -> Map.insertWith (+) gs 1 m) Map.empty . concat . takeAllActions

probabilityMap :: GameState -> Map.Map GameState Double
probabilityMap g@(GameState s n) = fmap (\c -> fromIntegral c * throwProbability n) . transitionMap $ g

mapForScore :: Int -> Double -> Map.Map GameState Double
mapForScore n s = probabilityMap $ GameState s n



expectedValue :: Map.Map GameState Double -> Double
expectedValue = fst . Map.mapAccumWithKey f 0.0
    where f accum gs prob = (accum + prob * score gs, prob)


takeAllActionsWithProbability :: Double -> GameState -> [[GameState]]
takeAllActionsWithProbability branchProbability gs =  (map . map) (\x -> GameState (score x * branchProbability * throwProbability (availableDice gs)) (availableDice x)) . takeAllActions $ gs
    where prob as = 1.0 / fromIntegral (length as)

recurse :: GameState -> [[[[GameState]]]]
recurse gs = (map . map) (takeAllActionsWithProbability . throwProbability . availableDice $ gs) . takeAllActionsWithProbability 1.0 $ gs

selectBestOption :: [[GameState]] -> [GameState]
selectBestOption = map maximum

makeMultiNode :: Double -> [GameState] -> DecisionTree
makeMultiNode prob = MultiNode . map (toTree prob) 

toTree :: Double -> GameState -> DecisionTree
toTree prob initial@(GameState s n) = Node initial . map (makeMultiNode (throwProbability n)). takeAllActionsWithProbability prob $ initial

makeTree :: GameState -> DecisionTree
makeTree = toTree 1.0

expectedTree :: Int -> DecisionTree -> Double
expectedTree 0 (Node g _) = score g
expectedTree levels (Node g subtrees) = sum . map (expectedTree (levels-1)) $ subtrees
expectedTree levels (MultiNode trees) = maximum . map (expectedTree levels) $ trees


-- Graph-based implementation

groupSortActions :: Int -> [(Int, [Action])]
groupSortActions i = [ (length g, head g) | g <- group . sort . allActions $ i]

type Score = Double 
type DiceCount = Int

data Strategy = Reject | Take deriving Show

type AnnotatedScore = (Score, Strategy)

-- A node which corresponds to a given number of dice
data DecisionNode = DecisionNode DiceCount [MultiDecisionEdge] deriving (Show)
-- A collection of edges that are mutually exclusive. Also stores the multiplicity
data MultiDecisionEdge = MultiDecisionEdge Int [DecisionEdge] deriving (Show)
-- A single edge with an associated score, that connects two nodes
data DecisionEdge = DecisionEdge Score DecisionNode deriving (Show)

instance Eq DecisionNode where
    DecisionNode a _ == DecisionNode b _ = a == b

instance Ord DecisionNode where
    DecisionNode a _ <= DecisionNode b _ = a <= b

makeEdge :: DiceCount -> Action -> DecisionEdge
makeEdge n (Action 0 0) = DecisionEdge 0 (makeGraph 0)
makeEdge n a = DecisionEdge (fromIntegral . points $ a) (makeGraph . newDiceCount $ n - takeDice a)
    where newDiceCount a = if a == 0 then 6 else a

memoizeGraph :: (DiceCount -> DecisionNode) -> (DiceCount -> DecisionNode)
memoizeGraph f = (map f [0 ..] !!)

makeGraph' :: DiceCount -> DecisionNode
makeGraph' n = DecisionNode n . map makeMultiDecisionNode . groupSortActions $ n
    where makeMultiDecisionNode (multiplicity, actions) = MultiDecisionEdge multiplicity . map (makeEdge n) $ actions

makeGraph :: DiceCount -> DecisionNode
makeGraph = memoizeGraph makeGraph'

expectedNode :: Score -> Int -> DecisionNode -> AnnotatedScore
expectedNode accScore 0 _ = (accScore, Take)
expectedNode accScore level (DecisionNode n multiedges) =
    let s = throwProbability n * (sum . map (expectedMultiEdge accScore (level - 1))) multiedges in
        if s > accScore then (s, Take) else (accScore, Reject)

expectedMultiEdge :: Score -> Int -> MultiDecisionEdge -> Score
expectedMultiEdge accScore level (MultiDecisionEdge mult edges) = fromIntegral mult * (maximum . map (expectedEdge accScore level) ) edges

expectedEdge :: Score -> Int -> DecisionEdge -> Score
expectedEdge accScore level (DecisionEdge 0 _) = 0.0
expectedEdge accScore level (DecisionEdge s node) = fst . expectedNode (s+accScore) level $ node


