module Tries where

import Data.List hiding (insert)
import Data.Bits
import Data.Maybe
import Debug.Trace

-- import Types
-- import HashFunctions
-- import Examples
--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes i
  | bs == 0   = bitTable !! b
  | otherwise = bitTable !! b + countOnes (shiftR i 4)
  where (bs, b) = divMod i 16

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes (n .&. (bit i - 1))

getIndex :: Int -> Int -> Int -> Int
getIndex i r b
  = shiftR i (r * b) .&. (bit b - 1)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x:xs) v
  = v : xs
replace i (x:xs) v
  = x : replace (i - 1) xs v

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 v l
  = v : l
insertAt i v (x:xs)
  = x : insertAt (i - 1) v xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f f' (Node v ss)
  = sum (map sumNode ss)
  where sumNode :: SubNode -> Int
        sumNode (Term i)
          = f i
        sumNode (SubTrie t)
          = sumTrie f f' t
sumTrie _ f' (Leaf xs)
  = f' xs

--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize
  = sumTrie (const 1) length

binCount :: Trie -> Int
binCount
  = sumTrie (const 1) (const 1)

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member v h t b
  = member' 0 t
  where member' :: Int -> Trie -> Bool
        member' _ (Leaf vs)
          = v `elem` vs
        member' l (Node bv st)
          = testBit bv i && case st !! n of
                              Term v'     -> v' == v
                              SubTrie st' -> member' (l + 1) st'
          where i = getIndex h l b
                n = countOnesFrom i bv

{-
type BitVector = Int
data Trie = Leaf [Int] | Node BitVector [SubNode]
          deriving (Eq, Show)
data SubNode = Term Int | SubTrie Trie
             deriving (Eq, Show)
type Hash = Int
type HashFun = Int -> Hash
-}
--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert f d b v t
  = insert' 0 v t
  where insert' :: Int -> Int -> Trie -> Trie
        insert' l v (Leaf vs)
          | v `elem` vs = Leaf vs
          | otherwise   = Leaf (v : vs)
        insert' l v (Node bv ss)
          | l == d - 1         = Leaf [v]
          | not (testBit bv i) = Node (setBit bv i) (insertAt n (Term v) ss)
          | otherwise          = Node bv (replace n ss s)
          where i = getIndex (f v) l b
                n = countOnesFrom i bv
                s = case ss !! n of
                      SubTrie st -> SubTrie (insert' (l + 1) v st)
                      Term v'    -> if v == v' then Term v' else SubTrie (insert' (l + 1) v (insert' (l + 1) v' empty))

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie f d b
  = foldl' (flip (insert f d b)) empty


type BitVector = Int

data Trie = Leaf [Int] | Node BitVector [SubNode]
          deriving (Eq, Show)

data SubNode = Term Int | SubTrie Trie
             deriving (Eq, Show)

type Hash = Int

type HashFun = Int -> Hash

empty :: Trie
empty
  = Node 0 []

-----------------------------------------------------------------
-- Show function for trees

-- Only needed for displaying bit vectors...
maxDegree :: Int
maxDegree
  = 16

showBitVector :: Int -> Int -> String
showBitVector bv 0
  = ""
showBitVector bv n
  = showBitVector (bv `div` 2) (n - 1) ++ show (bv `mod` 2)

showT :: Trie -> IO ()
showT t
  = showT' t 0

showT' :: Trie -> Int -> IO ()
showT' (Leaf vs) indent
  = do
      putStr (replicate indent ' ')
      putStrLn ("  " ++ show vs)
showT' (Node bv ts) indent
  = do
      putStrLn (replicate indent ' ' ++ showBitVector bv maxDegree)
      mapM_ (`showT''` (indent + 2)) ts

showT'' (Term v) indent
  = putStrLn (replicate indent ' ' ++ "<" ++ show v ++ ">")
showT'' (SubTrie t) indent
  = showT' t indent


--
-- Figure 2 (left)
--
figure :: Trie
figure
  = Node 16960 [Term 1830,
                SubTrie (Node 8208 [Term 73,
                                    SubTrie (Leaf [729,2521])]),
                Term 206]

--
-- Figure 2 (right)
--
figureHashed :: Trie
figureHashed
  = Node 16481 [Term 2521,
                Term 206,
                Term 729,
                SubTrie (Node 48 [Term 73,
                                  Term 1830])]

--
-- Test tries for insert from the spec
--
insT1, insT2, insT3, insT4 :: Trie
insT1
  = Node 512 [Term 2521]
insT2
  = Node 576 [Term 1830,
              Term 2521]
insT3
  = Node 576 [Term 1830,
              SubTrie (Node 8192 [SubTrie (Leaf [729,2521])])]

insT4
  = Node 97 [Term 2521,
             Term 206,
             Term 729]

--
-- Test tries for build from the spec
--
buildT1, buildT2 :: Trie
buildT1
  = Node 14 [Term 1,
             Term 2,
             Term 3]
buildT2
  = Node 1 [SubTrie (Node 1 [SubTrie (Leaf [256,512,768,
                                            1024,1280])])]

hash :: HashFun
hash x
  = op x''
  where
    op x = xor (shiftR x 16) x
    x'  = op x * 73244475
    x'' = op x' * 73244475
