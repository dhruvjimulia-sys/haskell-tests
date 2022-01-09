
-- Unimplemented functions: App rule and Cond rule

import Data.Maybe

data Expr = Number Int |
            Boolean Bool |
            Id String  |
            Prim String |
            Cond Expr Expr Expr |
            App Expr Expr |
            Fun String Expr
          deriving (Eq, Show)

data Type = TInt |
            TBool |
            TFun Type Type |
            TVar String |
            TErr 
          deriving (Eq, Show)

showT :: Type -> String
showT TInt  
  = "Int"
showT TBool 
  = "Bool"
showT (TFun t t') 
  = "(" ++ showT t ++ " -> " ++ showT t' ++ ")"
showT (TVar a) 
  = a
showT TErr  
  = "Type error"

type TypeTable = [(String, Type)]

type TEnv 
  = TypeTable    -- i.e. [(String, Type)]

type Sub 
  = TypeTable    -- i.e. [(String, Type)]  

-- Built-in function types...
primTypes :: TypeTable
primTypes 
  = [("+", TFun TInt (TFun TInt TInt)),
     (">", TFun TInt (TFun TInt TBool)),
     ("==", TFun TInt (TFun TInt TBool)),
     ("not", TFun TBool TBool)]

------------------------------------------------------
-- PART I

-- Pre: The search item is in the table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x
  = tryToLookUp x undefined

tryToLookUp :: Eq a => a -> b -> [(a, b)] -> b
tryToLookUp x d xs
  = fromMaybe d (lookup x xs)

-- Pre: The given value is in the table
reverseLookUp :: Eq b => b -> [(a, b)] -> [a]
reverseLookUp x xs
  = map fst $ filter (\(_, v) -> v == x) xs

occurs :: String -> Type -> Bool
occurs v (TVar v')
  = v == v'
occurs v (TFun t t')
  = occurs v t || occurs v t'
occurs _ _
  = False

------------------------------------------------------
-- PART II

-- Pre: There are no user-defined functions (constructor Fun)
-- Pre: All variables in the expression have a binding in the given 
--      type environment
inferType :: Expr -> TEnv -> Type
inferType (Number _)  _
  = TInt
inferType (Boolean _)  _
  = TBool
inferType (Id s) t
  = lookUp s t
inferType (Prim p) _
  = lookUp p primTypes
inferType (Cond c e e') t
  | inferType c t == TBool && inferType e t == inferType e' t = inferType e t
  | otherwise                                           = TErr
inferType (App e e') te
  = inferType' (inferType e te) (inferType e' te)
  where inferType' :: Type -> Type -> Type
        inferType' (TFun t t') _
          | t == inferType e' te = t'
        inferType' _ _
          = TErr
inferType (Fun _ _) _
  = undefined 
------------------------------------------------------
-- PART III
applySub :: Sub -> Type -> Type
applySub s (TVar v)
  = tryToLookUp v (TVar v) s
applySub s (TFun t t')
  = TFun (applySub s t) (applySub s t')
applySub _ t
  = t

unify :: Type -> Type -> Maybe Sub
unify t t'
  = unifyPairs [(t, t')] []


unifyPairs :: [(Type, Type)] -> Sub -> Maybe Sub
unifyPairs [] s
  = Just s
unifyPairs ((TInt, TInt):ts) s
  = unifyPairs ts s
unifyPairs ((TBool, TBool):ts) s
  = unifyPairs ts s
unifyPairs ((TVar v, TVar v'):ts) s
  | v == v'   = unifyPairs ts s
  | otherwise = Nothing
unifyPairs ((TVar v, t):ts) s
  | occurs v t = Nothing
  | otherwise  = unifyPairs (map (\(t', t'') -> (applySub [(v, t)] t', applySub [(v, t)] t'')) ts) ((v, t): s)
unifyPairs ((t, TVar v):ts) s
  | occurs v t = Nothing
  | otherwise  = unifyPairs (map (\(t', t'') -> (applySub [(v, t)] t', applySub [(v, t)] t'')) ts) ((v, t): s)
unifyPairs ((TFun t1 t2, TFun t1' t2'):ts) s
  = unifyPairs ((t1, t1'): (t2, t2'): ts) s
unifyPairs _ _
  = Nothing

------------------------------------------------------
-- PART IV

updateTEnv :: TEnv -> Sub -> TEnv
updateTEnv tenv tsub
  = map modify tenv
  where
    modify (v, t) = (v, applySub tsub t)

combine :: Sub -> Sub -> Sub
combine sNew sOld
  = sNew ++ updateTEnv sOld sNew

-- In combineSubs [s1, s2,..., sn], s1 should be the *most recent* substitution
-- and will be applied *last*
combineSubs :: [Sub] -> Sub
combineSubs 
  = foldr1 combine

inferPolyType :: Expr -> Type
inferPolyType e
  = t
  where (_, t, _) = inferPolyType' e [] (map (\c -> 'a':[c]) ['1'..])

inferPolyType' :: Expr -> TEnv -> [String] -> (Sub, Type, [String])
inferPolyType' (Fun x e) en (s:ss)
  | te == TErr = (en, TErr, ss')
  | otherwise  = (en, TFun (applySub sb (TVar s)) te, ss)
  where (sb, te, ss') = inferPolyType' e ((x, TVar s) : en) ss
inferPolyType' (App f e) en (s:ss)
  = maybe (en, TErr, ss'') (\us -> (combineSubs [us, sb', sb], applySub us (TVar s), ss'')) (unify tf (TFun te (TVar s)))
  where (sb, tf, ss') = inferPolyType' f en ss
        (sb', te, ss'') = inferPolyType' e (updateTEnv en sb) ss'
inferPolyType' e en ss
  = (en, inferType e en, ss)

------------------------------------------------------
-- Monomorphic type inference test cases from Table 1...

env :: TEnv
env = [("x",TInt),("y",TInt),("b",TBool),("c",TBool)]

ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8 :: Expr
type1, type2, type3, type4, type5, type6, type7, type8 :: Type

ex1 = Number 9
type1 = TInt

ex2 = Boolean False
type2 = TBool

ex3 = Prim "not"
type3 =  TFun TBool TBool

ex4 = App (Prim "not") (Boolean True)
type4 = TBool

ex5 = App (Prim ">") (Number 0)
type5 = TFun TInt TBool

ex6 = App (App (Prim "+") (Boolean True)) (Number 5)
type6 = TErr

ex7 = Cond (Boolean True) (Boolean False) (Id "c")
type7 = TBool

ex8 = Cond (App (Prim "==") (Number 4)) (Id "b") (Id "c")
type8 = TErr

------------------------------------------------------
-- Unification test cases from Table 2...

u1a, u1b, u2a, u2b, u3a, u3b, u4a, u4b, u5a, u5b, u6a, u6b :: Type
sub1, sub2, sub3, sub4, sub5, sub6 :: Maybe Sub

u1a = TFun (TVar "a") TInt
u1b = TVar "b"
sub1 = Just [("b",TFun (TVar "a") TInt)]

u2a = TFun TBool TBool
u2b = TFun TBool TBool
sub2 = Just []

u3a = TFun (TVar "a") TInt
u3b = TFun TBool TInt
sub3 = Just [("a",TBool)]

u4a = TBool
u4b = TFun TInt TBool
sub4 = Nothing

u5a = TFun (TVar "a") TInt
u5b = TFun TBool (TVar "b")
sub5 = Just [("b",TInt),("a",TBool)]

u6a = TFun (TVar "a") (TVar "a")
u6b = TVar "a"
sub6 = Nothing

------------------------------------------------------
-- Polymorphic type inference test cases from Table 3...

ex9, ex10, ex11, ex12, ex13, ex14 :: Expr
type9, type10, type11, type12, type13, type14 :: Type

ex9 = Fun "x" (Boolean True)
type9 = TFun (TVar "a1") TBool

ex10 = Fun "x" (Id "x")
type10 = TFun (TVar "a1") (TVar "a1")

ex11 = Fun "x" (App (Prim "not") (Id "x"))
type11 = TFun TBool TBool

ex12 = Fun "x" (Fun "y" (App (Id "y") (Id "x")))
type12 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TVar "a3")) (TVar "a3"))

ex13 = Fun "x" (Fun "y" (App (App (Id "y") (Id "x")) (Number 7)))
type13 = TFun (TVar "a1") (TFun (TFun (TVar "a1") (TFun TInt (TVar "a3"))) 
              (TVar "a3"))

ex14 = Fun "x" (Fun "y" (App (Id "x") (Prim "+"))) 
type14 = TFun (TFun (TFun TInt (TFun TInt TInt)) (TVar "a3")) 
              (TFun (TVar "a2") (TVar "a3"))