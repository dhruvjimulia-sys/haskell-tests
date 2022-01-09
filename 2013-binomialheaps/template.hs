-- All functions implemented

import Data.Maybe

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node k _ _)
  = k

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ cs)
  = cs

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t@(Node k r cs) t'@(Node k' _ cs')
  | k < k'    = Node k (r + 1) (t':cs)
  | otherwise = Node k' (r + 1) (t:cs')

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin h
  = minimum $ map key h

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h
  = h
mergeHeaps h []
  = h
mergeHeaps h@(t@(Node _ r _):ts) h'@(t'@(Node _ r' _):ts')
  | r < r'    = t : mergeHeaps ts h'
  | r' < r    = t' : mergeHeaps ts' h
  | otherwise = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts')

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v
  = mergeHeaps [Node v 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps (filter (/= t) h) h'
  where m = extractMin h
        t = head [t | t@(Node k _ _) <- h, k == m]
        h' = (reverse . children) t

binSort :: Ord a => [a] -> [a]
binSort xs
  = flatten (foldr insert [] xs)
  where flatten :: Ord a => BinHeap a -> [a]
        flatten [] = []
        flatten h  = extractMin h : flatten (deleteMin h) 

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary xs
  = reverse $ toBinary' $ map (\(Node _ r _) -> r) xs
  where toBinary' :: [Int] -> [Int]
        toBinary' xs
          | all (<0) xs = []
          | 0 `elem` xs = 1 : toBinary' (map (\x -> x - 1) xs)
          | otherwise   = 0 : toBinary' (map (\x -> x - 1) xs)

binarySum :: [Int] -> [Int] -> [Int]
binarySum i i'
  = reverse $ binarySum' (reverse i) (reverse i') 0
  where binarySum' :: [Int] -> [Int] -> Int -> [Int]
        binarySum' (x:xs) (y:ys) ci
          = s : binarySum' xs ys co
          where (co, s) = quotRem (x + y + ci) 2
        binarySum' (x:xs) [] ci
          = s : binarySum' xs [] co
          where (co, s) = quotRem (x + ci) 2
        binarySum' [] (y:ys) ci
          = s : binarySum' [] ys co
          where (co, s) = quotRem (y + ci) 2
        binarySum' _ _ 1
          = [1]
        binarySum' _ _ _
          = [] 
        

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]