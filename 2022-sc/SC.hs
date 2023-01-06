module SC where

import Data.List
import Data.Maybe

import Types
import Examples
import Distribution.Compat.Lens (_1, toDListOf)

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun _ _)
  = True
isFun _
  = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs
  = partition (isFun . snd)

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _)
  = length (filter (isFun . snd) bs)
topLevelFunctions _
  = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll
  = foldl' union []

freeVars :: Exp -> [Id]
freeVars (Const _)
  = []
freeVars (Var i)
  | i `elem` prims = []
  | otherwise      = [i]
freeVars (App e es)
  = unionAll (map freeVars es) `union` freeVars e
freeVars (Fun is e)
  = freeVars e \\ is
freeVars (Let bs e)
  = freeVars e `union` unionAll (map (freeVars . snd) bs) \\ map fst bs
---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Let bs e)
  = [(i, unionAll (map (freeVars  . snd) fbs) \\ map fst fbs) | (i, _) <- fbs]
      ++ concatMap (\(_, e) -> buildFVMap e) bs
        ++ buildFVMap e
  where
    fbs = (fst . splitDefs) bs
buildFVMap (App e es)
  = buildFVMap e ++ concatMap buildFVMap es
buildFVMap (Fun is e)
  = buildFVMap e 
buildFVMap _
  = []

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions m (Let bs e)
  = Let (vbs ++ [('$' : f, Fun (lookUp f m ++ as) (modifyFunctions m e))
          | (f, Fun as e) <- fbs]) (modifyFunctions m e)
    where
      (fbs, vbs) = splitDefs bs
modifyFunctions m (Var f)
  = case fvs of
      Just []   -> Var ('$' : f)
      Just jfvs -> App (Var ('$' : f)) (map Var jfvs)
      Nothing   -> Var f
  where 
    fvs = lookup f m
modifyFunctions m (App e es)
  = App (modifyFunctions m e) (map (modifyFunctions m) es)
modifyFunctions _ e
  = e

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp

lift
  = id
  
-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' (Let [] e)
  = (e, [])
lift' (Let bs e)
  = (Let vbs e', fbs ++ scs' ++ scs)
  where
    t          = map (\(i, e) -> (i, lift' e)) bs
    bs'        = map (\(i, (e, _)) -> (i, e)) t
    (fbs, vbs) = splitDefs bs'
    scs        = concatMap (snd . snd) t
    (e', scs') = lift' e
lift' e
  = (e, [])