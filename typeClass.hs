--Equality Dictionary

import Data.Char

data EqDict a = EqD (a -> a -> Bool)

eqInt :: Int -> Int -> Bool
eqInt x y = x == y

eq :: EqDict a -> a -> a -> Bool
eq (EqD f) = f

--comprehension
elem :: EqDict a -> a -> [a] -> Bool
elem d x ys = or [eq d x y | y <- ys]

--recursion
elem1 :: EqDict a -> a -> [a] -> Bool
elem1 d x [] = False
elem1 d x (y : ys) = eq d x y || elem1 d x ys

--higher-order
elem2 :: EqDict a -> a -> [a] -> Bool
elem2 d x ys = foldr (||) False (map (eq d x) ys)

--work around if haskell type Classes did not exist
--dictionary of various types

--dictionary for integers
dInt :: EqDict Int
dInt = EqD eqInt

--dictionary for characters
dChar :: EqDict Char
dChar = EqD f
    where
    f :: Char -> Char -> Bool
    f x y = eq dInt (ord x) (ord y)

--dictionary for pairs
dPair :: (EqDict a, EqDict b) -> EqDict(a,b)
dPair (da, db) = EqD f
    where
    f(u,v) (x, y) = eq da u x && eq db v y


--dictionary for lists
dList :: (EqDict a) -> EqDict[a]
dList da = EqD f 
    where 
    f [] [] = True
    f (x : xs) [] = False
    f [] (y : ys) = False
    f (x : xs) (y : ys) = eq da x y && f xs ys
    --f (x : xs) (y : ys) = eq da x y && eq (dList da) xs ys

data Shape = Rectangle Int Int | Circle Int
    deriving (Eq, Ord, Show)





