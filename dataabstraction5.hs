
--abstracted set using unordered list

module ListAbs
    (Set, nil, insert, set, element, equal) where 

--Sets as list without abstraction
newtype Set a = MkSet [a]


nil :: Set a
nil = MkSet []


insert :: a -> Set a -> Set a
insert x (MkSet xs) = MkSet (x:xs)

set :: [a] -> Set a
set xs = MkSet xs

element :: Eq a => a -> Set a -> Bool
x `element` (MkSet xs) = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
MkSet xs `equal` MkSet ys = xs `subset` ys && ys `subset` xs
    where 
    xs `subset` ys = and [x `elem` ys | x <- xs]
