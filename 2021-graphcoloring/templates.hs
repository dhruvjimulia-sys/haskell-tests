module Alloc where

import Data.Maybe
import Data.List
import Data.Tuple

import Types
import Examples
import Distribution.Compat.Lens (_1)
import Debug.Trace

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x
  = length . elemIndices x

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = map (\n -> (n, count n (map fst es) + count n (map snd es))) ns

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (ns, es)
  = [s | (s, e) <- es, e == n] ++ [e | (s, e) <- es, s == n]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (delete n ns, filter (\(s, e) -> s /= n && e /= n) es)

------------------------------------------------------
--
-- Part II
--

colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph m g
  | null cs   = (min, 0) : cmap
  | otherwise = (min, head cs) : cmap
  where min  = snd $ minimum $ map swap (degrees g)
        cmap = colourGraph m (removeNode min g)
        cs   = sort $ [1..m] \\ map (`lookUp` cmap) (neighbours min g)

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap c
  = ("return", "return"): map (\(v, n) -> if n == 0 then (v, v) else (v, 'R': show n)) c

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments is im
  = map (\i -> Assign (lookUp i im) (Var i)) is

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Apply o e e') im
  = Apply o (renameExp e im) (renameExp e' im)
renameExp (Var id) im
  = Var (lookUp id im)
renameExp c _
  = c

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock [] _
  = []
renameBlock (Assign i e: ss) im
  | renameExp (Var i) im == e' = ss'
  | otherwise                  = Assign (lookUp i im) e': ss'
  where e' = renameExp e im
        ss' = renameBlock ss im
renameBlock ((If e b b'): ss) im
  = If (renameExp e im) (renameBlock b im) (renameBlock b' im) : renameBlock ss im
renameBlock ((While e b): ss) im
  = While (renameExp e im) (renameBlock b im) : renameBlock ss im

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG xs
  = ((nub . concat) xs, (nub . concatMap (\vs -> [(x,y) | x <- vs, y <- vs, x < y])) xs)

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars c
  = converge (==) l
  where l = iterate (liveVars' 0) (replicate (length c) [])
        liveVars' :: Int -> [[Id]]-> [[Id]]
        liveVars' n li | trace (show n ++ " ") False = undefined
        liveVars' n li
          = [u] `union` [delete d (foldr union [] (map (\s -> li !! s) sc))]
          where ((d, u), sc) = c !! n

converge :: (a -> a -> Bool) -> [a] -> a
converge f xs
  = (fst . head) $ dropWhile (not . snd) (zipWith (\x y -> (x, f x y)) xs (tail xs))

{-
type Function = (Id, [Id], Block)
data Statement = Assign Id Exp |
                 If Exp Block Block |
                 While Exp Block
               deriving (Eq, Show)

type Block = [Statement]

-}

buildCFG :: Function -> CFG
buildCFG (_, as, b)
  = buildCFG' b 0

buildCFG' :: Block -> Int -> CFG
buildCFG' [] _
  = []
buildCFG' ((Assign "return" e): ss) i
  = (("return", vars e), []) : buildCFG' ss (i + 1)
buildCFG' ((Assign v e): ss) i
  = ((v, vars e), [i + 1]) : buildCFG' ss (i + 1)
buildCFG' ((While e b): ss) i
  = (("_", vars e), [i + 1, i']): modifyLastAssign (buildCFG' b (i + 1)) i ++ buildCFG' ss i'
  where i' = i + 1 + size b
buildCFG' ((If e b b'): ss) i
  = (("_", vars e), [i + 1, i']): modifyLastAssign (buildCFG' b (i + 1)) i'' ++ buildCFG' b' i' ++ buildCFG' ss i''
  where i'  = i + 1 + size b
        i'' = i' + size b'

vars :: Exp -> [Id]
vars (Var id)
  = [id]
vars (Apply _ e e')
  = vars e ++ vars e'
vars _
  = []

size :: Block -> Int
size []
  = 0
size ((Assign _ _): ss)
  = 1 + size ss
size ((If _ b b'): ss)
  = 1 + size b + size b' + size ss
size ((While _ b): ss)
  = 1 + size b + size ss

modifyLastAssign :: CFG -> Int -> CFG
modifyLastAssign c i
  = case succ of
      [_] -> reverse ((p, [i]):xs)
      _   -> c
  where ((p, succ):xs) = reverse c
