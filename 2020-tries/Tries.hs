module Tries where

import Data.List hiding (insert)
import Data.Bits
import Data.Maybe

import Types
import HashFunctions
import Examples
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
