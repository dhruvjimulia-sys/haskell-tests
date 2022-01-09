-- Unimplemented function: longestRepeatedSubstring

import Data.List
import Data.Maybe

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix
  = isPrefixOf

isPrefix' :: String -> String -> Bool
isPrefix'
  = (.) (and .) (zipWith (==))

isPrefix'' :: String -> String -> Bool
isPrefix'' p s
  = take (length p) s == p

removePrefix :: String -> String -> String
removePrefix p s
--Pre: s is a prefix of s'
  = fromJust (stripPrefix p s)

removePrefix' :: String -> String -> String
removePrefix' p
--Pre: s is a prefix of s'
  = drop (length p)

suffixes :: [a] -> [[a]]
suffixes
  = init . tails

suffixes' :: [a] -> [[a]]
suffixes' []
  = []
suffixes' xs
  = xs : suffixes' (tail xs)

isSubstring :: String -> String -> Bool
isSubstring s s'
  = any (isPrefix s) (suffixes s')

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = concatMap (\(sf, i) -> [i | isPrefix s sf]) (zip (suffixes s') [0..])

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf i)
  = [i]
getIndices (Node xs)
  = concatMap (getIndices . snd) xs

partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' w w'
  = (p, sw, drop l w')
  where l = length (takeWhile (True==) (zipWith (==) w w'))
        (p, sw) = splitAt l w

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf i)
  = [i]
findSubstrings' _ (Leaf _)
  = []
findSubstrings' s (Node as)
  = concatMap findSubstrings'' as
  where findSubstrings'' :: (String, SuffixTree) -> [Int]
        findSubstrings'' (a, st)
          | null rs = getIndices st
          | null ra = findSubstrings' rs st
          | otherwise    = []
          where (_, rs, ra) = partition' s a

------------------------------------------------------

insert' :: (String, Int) -> SuffixTree -> SuffixTree
insert' (s, n) (Node as)
  | as' == as = Node ((s, Leaf n): as)
  | otherwise = Node as'
  where as' = concatMap insert'' as
        insert'' :: (String, SuffixTree) -> [(String, SuffixTree)] 
        insert'' (a, t)
          | null p  = [(a, t)]
          | null ra = [(a, insert' (rs, n) t)]
          | otherwise = [(p, Node [(rs, Leaf n), (ra, t)])]
          where (p, rs, ra) = partition' s a

-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert') (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> Maybe String
longestRepeatedSubstring t
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]