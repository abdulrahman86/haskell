
--breaking invariant problem

module OrderedListUnabs
    (Set, nil, insert, set, element, equal) where

import Data.List(nub,sort)

--set as ordered lists without abstraction

type Set a = [a]

invariant :: Ord a => Set a -> Bool
invariant xs = 
    and [x < y | (x,y) <- zip xs (tail xs)]

nil :: Set a
nil = []


insert :: Ord a => a -> Set a -> Set a
insert x [] = [x]
insert x (y:ys) | x < y = x : y : ys
                | x == y = y : ys
                | x > y = y : x `insert` ys

set :: Ord a => [a] -> Set a
set xs = nub1 (sort xs)

element :: Eq a => a -> Set a -> Bool
x `element` xs = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys = xs `subset` ys && ys `subset` xs
    where 
    xs `subset` ys = and [x `elem` ys | x <- xs]

nub1 :: Ord a => Set a -> Set a
nub1 [] = []
nub1 [x] = [x]
nub1 (x1 : x2 : xs) | x1 == x2 = nub1 (x2 : xs)
                    | otherwise = x1 : (nub1 (x2 : xs)) 
