module SOL where

import Data.List
import Data.Maybe

import Types
import TestData
import Debug.Trace
import Data.Tuple

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp
  = (fromJust .) . lookup

-- 3 marks
vars :: Formula -> [Id]
vars f
  = (sort . nub) (vars' f)
  where vars' :: Formula -> [Id]
        vars' (Var i)
          = [i]
        vars' (Not f)
          = vars' f
        vars' (And f f')
          = vars' f ++ vars' f'
        vars' (Or f f')
          = vars' f ++ vars' f'

-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (And a b))
  = Or (toNNF (Not a)) (toNNF (Not b))
toNNF (Not (Or a b))
  = And (toNNF (Not a)) (toNNF (Not b))
toNNF (Not (Not a))
  = toNNF a
toNNF (And f f')
  = And (toNNF f) (toNNF f')
toNNF (Or f f')
  = Or (toNNF f) (toNNF f')
toNNF (Not f)
  = Not (toNNF f)
toNNF f
  = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' (toNNF f)
  where toCNF' :: NNF -> CNF
        toCNF' (And f f')
          = And (toCNF' f) (toCNF' f')
        toCNF' (Or f f')
          = distribute (toCNF f) (toCNF f')
        toCNF' f
          = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' f
  where m = idMap f
        flatten' :: CNF -> CNFRep
        flatten' (Var i)
          = [[lookUp i m]]
        flatten' (And f f')
          = flatten' f ++ flatten' f'
        flatten' (Or f f')
          = [head (flatten' f) ++ head (flatten' f')]
        flatten' (Not f)
          = map (map negate) (flatten' f)

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
-- propUnits cr | trace (show cr ++ " ") False = undefined
propUnits cr
  = case break (\l -> length l == 1) cr of
      (xs, [])     -> (cr, [])
      (xs, [y]:ys) -> (cr', y : vs)
        where (cr', vs) = propUnits $ filter (notElem y) (map (filter (/= - y)) xs ++ map (filter (/= - y)) ys)

-- 4 marks
dp :: CNFRep -> [[Int]]
-- dp cr | trace (show cr ++ " ") False = undefined
dp cr
  = case propUnits cr of
      ([], vs)          -> [vs]
      (l@(x:_), vs) -> if [] `elem` l then [] else map (vs ++) (dp ([head x] : l)) ++ map (vs ++) (dp ([- head x] : l))

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f
  = concatMap allSat' (dp $ flatten $ toCNF f)
  where allSat' :: [Int] -> [[(Id, Bool)]]
        allSat' l
          = allSat'' $ map (\v -> toBoolPair v (lookUp v (idMap f))) (vars f)
          where toBoolPair :: Id -> Int -> (Id, Maybe Bool)
                toBoolPair v i
                  | i `elem` l   = (v, Just True)
                  | - i `elem` l = (v, Just False)
                  | otherwise    = (v, Nothing)
                allSat'' :: [(Id, Maybe Bool)] -> [[(Id, Bool)]]
                allSat'' []
                  = [[]]
                allSat'' ((i, Just True) : xs)
                  = map ((i, True):) (allSat'' xs)
                allSat'' ((i, Just False) : xs)
                  = map ((i, False):) (allSat'' xs)
                allSat'' ((i, Nothing) : xs)
                  = map ((i, True):) (allSat'' xs) ++ map ((i, False):) (allSat'' xs)