--proposition data structure

import Data.List

type Name = String

data Prop = Var Name
          | T 
          | F
          | Not Prop
          | Prop :&: Prop
          | Prop :|: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


showProp :: Prop -> String

showProp(Var x) = x
showProp T = "T"
showProp F = "F"
showProp (Not p) = par ("~" ++ showProp p)
showProp (p :|: q) = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par (showProp p ++ "&" ++ showProp q)

par :: String -> String
par s = "(" ++ s ++ ")"


names :: Prop -> Names
names T = []
names F = []
names (Var x) = [x]
names (Not p) = names p
names (p :|: q) = nub ((names p) ++ (names q))
names (p :&: q) = nub ((names p) ++ (names q))

--evaluator for boolean 
eval :: Env -> Prop -> Bool 
eval e (Var x) = lookUp e x
eval e F = False
eval e T = True
eval e (Not p) = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q

lookUp :: Eq a => [(a, b)] -> a -> b
lookUp xys x = the [y | (x',y) <- xys, x==x']
    where 
    the [x] = x

--generate all possible environments from variable names
envs :: Names -> [Env]
envs [] = [[]]
envs (x : xs) = [(x,b):e | b <- bs, e <- envs xs] 
    where 
    bs = [False, True]

--determines if a proposition is satisfiable
satisfiable :: Prop -> Bool 
satisfiable p = or [eval e p | e <- envs(names p)]

--determines if proposition is a tautology
tautology :: Prop -> Bool 
tautology p = and [eval e p | e <- envs(names p)]
