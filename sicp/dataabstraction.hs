
module Scheme (Pair, cons, car, cdr) where

data Pair a b = Nil | Cons a b 
    deriving Show

cons :: a -> b -> Pair a b
cons a b = Cons a b

car :: Pair a b -> a 
car (Cons a b) = a

cdr :: Pair a b -> b
cdr (Cons a b) = b

data Tree a = Empty | Node a (Tree a)(Tree a) 

--enumerate a tree to form a list.
class Enumerable m where
    enumerate :: m a -> [a]

instance Enumerable Tree where
    enumerate Empty = []
    enumerate (Node n left right) = (enumerate left) ++ [n] ++ (enumerate right)


--generate all pairs of  form (a b) where a < b

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f as = foldr (++) []  (map f as)

genPairs :: Int -> Int -> [(Int, Int)]
genPairs start end = flatMap f [start..end] where 
        
    f :: Int -> [(Int, Int)]
    f elem = filter (\(x, y) -> x <=y) (map (\x -> (elem, x)) [start..end])


--generate permutations of list
permutations :: (Eq a) => [a] -> [[a]] 
permutations [] = [[]]
permutations as = flatMap f as where 
    f elem = map (elem:) (permutations (remove elem as))
    remove a [] = []
    remove a (b : bs) 
        | a == b = bs
        | otherwise = b : (remove a bs)
