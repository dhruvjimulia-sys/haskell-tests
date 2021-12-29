import Data.Maybe
import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp k
  = fromJust . lookup k

simplify :: RE -> RE
simplify (Plus re)
  = Seq (simplify re) (Rep (simplify re))
simplify (Opt re)
  = Alt (simplify re) Null
simplify (Rep re)
  = Rep (simplify re)
simplify (Alt re re')
  = Alt (simplify re) (simplify re')
simplify (Seq re re')
  = Seq (simplify re) (simplify re')
simplify re
  = re

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (s, _, _)
  = s
terminalStates :: Automaton -> [State]
terminalStates (_, t, _)
  = t
transitions :: Automaton -> [Transition]
transitions (_, _, t)
  = t

isTerminal :: State -> Automaton -> Bool
isTerminal s a
  = s `elem` terminalStates a

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s a
  = [t | t@(s', _, _) <- transitions a, s == s']

labels :: [Transition] -> [Label]
labels ts
  = nub [l | (_, _, l) <- ts, l /= Eps]

accepts :: Automaton -> String -> Bool
accepts a s
  = accepts' (startState a) s
  where accepts' :: State -> String -> Bool
        accepts' s str
          | isTerminal s a && null str = True
          | otherwise                  = any (try str) (transitionsFrom s a)
        try :: String -> Transition -> Bool
        try str (_, t, Eps)
          = accepts' t str
        try [] (_, _, C _)
          = False
        try (c':cs) (_, t, C c)
          | c == c'   = accepts' t cs
          | otherwise = False

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k
  = ([(m, n, Eps)], k)
make (Term c) m n k
  = ([(m, n, C c)], k)
make (Seq re re') m n k
  = (t ++ (k, k + 1, Eps) : t', k'')
  where (t, k')  = make re m k (k + 2)
        (t', k'') = make re' (k + 1) n k'
make (Alt re re') m n k
  = (t ++ (m, k, Eps): (m, k + 2, Eps): (k + 1, n, Eps): (k + 3, n, Eps): t', k'')
  where (t, k') = make re k (k + 1) (k + 4)
        (t', k'') = make re' (k + 2) (k + 3) k'
make (Rep re) m n k
  = ((m, k, Eps): (m, n, Eps): (k + 1, k, Eps): t, k')
  where (t, k') = make re k (k + 1) (k + 2)

--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier s a
  = concatMap getFrontier' (transitionsFrom s a)
  where getFrontier' :: Transition -> [Transition]
        getFrontier' (_, t, Eps)
          | isTerminal t a = [(t, t, Eps)]
          | otherwise      = concatMap getFrontier' (transitionsFrom t a)
        getFrontier' t
          = [t]

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions ts
  = map (\l -> (l, [e | (_, e, l') <- ts, l' == l])) (labels ts)

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA a@(s, es, _)
  = (s, map (`lookUp` mp) (filter (any (`elem` es)) ms), [(lookUp ms' mp, lookUp ms'' mp, l) | (ms', ms'', l) <- ts])
  where (m, ms, ts) = makeDA' [s] [] []
        mp = zip ms [1..]
        makeDA' :: [State] -> [MetaState] -> [MetaTransition]
                -> (MetaState, [MetaState], [MetaTransition])
        makeDA' ss ms ts
          | m `elem` ms = (ss, ms, ts)
          | otherwise   = foldr (\(l, ss) (r, ms, ts) -> let (r, ms', ts') = makeDA' ss ms ts in (r, m : ms, (m, r, l) : ts')) (ss, ms, ts) (groupTransitions fs)
          where m = (sort . nub) [s' | (s', _, _) <- fs]
                fs = concatMap (`getFrontier` a) ss

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

