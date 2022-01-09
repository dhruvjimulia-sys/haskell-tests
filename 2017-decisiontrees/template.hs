-- All functions implemented

import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue |
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame xs
  = and $ zipWith (==) xs (tail xs)

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove k
  = filter (\p -> fst p /= k)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt a h r
  = r !! fromJust (elemIndex a (map fst h))

removeAtt :: AttName -> Header -> Row -> Row
removeAtt a h r
  = map snd $ filter (\(i, _) -> i /= fromJust (elemIndex a (map fst h))) (zip [0..] r)

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, v) m
  | x `elem` map fst m = (x, v : fromJust (lookup x m)) : remove x m
  | otherwise          = (x, [v]) : m

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (n, vs) (h, rs)
  = map (\(k, v) -> (k, length v)) (foldr addToMapping (zip vs (repeat [])) (zip (map (lookUpAtt n h) rs) [1..]))

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null
  = 0
nodes (Leaf _)
  = 1
nodes (Node _ ts)
  = 1 + sum (map (nodes . snd) ts)

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf v) _ _
  = v
evalTree (Node n ts) h r
  = evalTree (lookUp (lookUpAtt n h r) ts) h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (n, vs)
  = map (\v -> (v, (remove n h, map (removeAtt n h) (filter (\r -> lookUpAtt n h r == v) rs)))) vs

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree
buildTree (_, []) _ _
  = Null
buildTree d@(h, rs) ca s
  | allSame (map (lookUpAtt (fst ca) h) rs) = Leaf (lookUpAtt (fst ca) h (head rs))
  | otherwise             = Node pn (map (\(v, ds) -> (v, buildTree ds ca s)) (partitionData d pa))
  where pa@(pn, _) = s d ca

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy (_, []) a
  = 0
entropy ds@(_, rs) a
  = sum (map (\(_, i) -> - xlogx (fromIntegral i / fromIntegral n)) (buildFrequencyTable a ds))
  where n = length rs

gain :: DataSet -> Attribute -> Attribute -> Double
gain ds@(_, rs) a@(n, vs) ca
  = entropy ds ca - sum (zipWith (\v@(_, i) (_, p) -> fromIntegral i / fromIntegral n * entropy p ca) (buildFrequencyTable a ds) (partitionData ds a))
  where n = length rs
  
bestGainAtt :: AttSelector
bestGainAtt ds@(h, rs) ca@(n, vs)
  = snd $ maximum $ map (\a -> (gain ds a ca, a)) (delete ca h)

--------------------------------------------------------------------

outlook :: Attribute
outlook
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute
temp
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute
humidity
  = ("humidity", ["high", "normal"])

wind :: Attribute
wind
  = ("wind", ["windy", "calm"])

result :: Attribute
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header
  =  [outlook,    temp,   humidity, wind,    result]
table
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header'
  =  [outlook,    result, temp,   humidity, wind]
table'
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook"
         [("sunny", Node "temp"
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity"
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp"
                         [("hot", Null),
                          ("mild", Node "humidity"
                                        [("high",Node "wind"
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity"
                                        [("high", Null),
                                         ("normal", Node "wind"
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook"
         [("sunny", Node "humidity"
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind"
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
