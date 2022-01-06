import Data.List
import Data.Maybe
import Debug.Trace
import Data.Tuple

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k
  = fromJust . lookup k

checkSat :: BDD -> Env -> Bool
checkSat (r, ns) en
  = checkSat' r
  where checkSat' :: NodeId -> Bool
        checkSat' ni
          | lookUp nid en = if r < 2 then toEnum r else checkSat' r
          | otherwise     = if l < 2 then toEnum l else checkSat' l
          where (nid, l, r) = lookUp ni ns

sat :: BDD -> [[(Index, Bool)]]
sat (r, ns)
  = sat' r
  where sat' :: NodeId -> [[(Index, Bool)]]
        -- sat' i | trace (show i ++ " ") False = undefined
        sat' ni
          | ni == 0   = []
          | ni == 1   = [[]]
          | otherwise = map ((nid, False):) (sat' l) ++ map ((nid, True):) (sat' r)
          where (nid, l, r) = lookUp ni ns
          
------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))
  = Prim (not b)
simplify (Or (Prim b) (Prim b'))
  = Prim (b || b')
simplify (And (Prim b) (Prim b'))
  = Prim (b && b')
simplify e
  = e

restrict :: BExp -> Index -> Bool -> BExp
restrict (Not e) i b 
  = simplify (Not (restrict e i b))
restrict (Or e e') i b
  = simplify (Or (restrict e i b) (restrict e' i b))
restrict (And e e') i b
  = simplify (And (restrict e i b) (restrict e' i b))
restrict (IdRef i') i b
  | i' == i   = Prim b
  | otherwise = IdRef i'
restrict b _ _ 
  = b

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e
  = buildBDD' e 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim False) _ []
  = (0, [])
buildBDD' (Prim True) _ []
  = (1, [])
buildBDD' e id (x:xs)
  = (id, (id, (x, l, r)):lns ++ rns)
  where (l, lns) = buildBDD' (restrict e x False) (2 * id) xs
        (r, rns) = buildBDD' (restrict e x True) (2 * id + 1) xs 

{-
type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]
type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])
-}
------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = buildROBDD' e 2 Nothing xs

buildROBDD' :: BExp -> NodeId -> Maybe NodeId -> [Index] -> BDD
-- buildROBDD' e nid pid [] xs | trace (show nid ++ " ") False = undefined
buildROBDD' (Prim False) _ _ []
  = (0, [])
buildROBDD' (Prim True) _ _ []
  = (1, [])
buildROBDD' e id Nothing (x:xs)
  = (id, (id, (x, l, r)):lns ++ rns)
    where (l, lns) = buildROBDD' (restrict e x False) (2 * id) (Just id) xs
          (r, rns) = buildROBDD' (restrict e x True) (2 * id + 1) (Just id) xs 
buildROBDD' e id (Just pid) (x:xs)
  | l == r    = (pid, replace l pid lns)
  | otherwise = (id, (id, (x, l, r)):lns ++ rns)
  where (l, lns) = buildROBDD' (restrict e x False) (2 * id) (Just id) xs
        (r, rns) = buildROBDD' (restrict e x True) (2 * id + 1) (Just id) xs 
buildROBDD' _ _ _ _
  = undefined

{-
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs
  = fst (buildROBDD' e 2 Nothing [] xs)

buildROBDD' :: BExp -> NodeId -> Maybe NodeId -> [BDD] -> [Index] -> (BDD, [BDD])
-- buildROBDD' e nid pid [] xs | trace (show nid ++ " ") False = undefined
buildROBDD' (Prim False) _ _ _ []
  = ((0, []), [])
buildROBDD' (Prim True) _ _ _ []
  = ((1, []), [])
buildROBDD' e id Nothing ts (x:xs)
  = ((id, (id, (x, l, r)):lns ++ rns), lt: rt: alts ++ arts)
    where (lt@(l, lns), alts) = buildROBDD' (restrict e x False) (2 * id) (Just id) ts xs
          (rt@(r, rns), arts) = buildROBDD' (restrict e x True) (2 * id + 1) (Just id) (alts ++ ts) xs 
buildROBDD' e id (Just pid) ts (x:xs)
  | l == r    = ((pid, replace l pid lns), lt: alts)
  | otherwise = case lookup lns (map swap ts) of
                                   Just nid -> ((pid, replace l pid (lookUp nid ts)), rt: arts)
                                   Nothing  -> case lookup rns (map swap ts) of
                                                 Just nid -> ((pid, replace r pid (lookUp nid ts)), lt: alts)
                                                 Nothing -> ((id, (id, (x, l, r)):lns ++ rns), lt: rt: alts ++ arts)
  where (lt@(l, lns), alts) = buildROBDD' (restrict e x False) (2 * id) (Just id) ts xs
        (rt@(r, rns), arts) = buildROBDD' (restrict e x True) (2 * id + 1) (Just id) (alts ++ ts) xs 
buildROBDD' _ _ _ _ _
  = undefined
-}
replace :: NodeId -> NodeId -> [BDDNode] -> [BDDNode]
replace oid nid
  = map (\(id, v) -> if id == oid then (nid, v) else (id, v))

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


