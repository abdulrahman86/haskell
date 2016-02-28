--data  structures without abstraction. 


module ListUnabs
    (Set, nil, insert, set, element, equal) where 

--Sets as list without abstraction
type Set a = [a]


nil :: Set a
nil = []


insert :: a -> Set a -> Set a
insert x xs = x:xs

set :: [a] -> Set a
set xs = xs

element :: Eq a => a -> Set a -> Bool
x `element` xs = x `elem` xs

equal :: Eq a => Set a -> Set a -> Bool
xs `equal` ys = xs `subset` ys && ys `subset` xs
    where 
    xs `subset` ys = and [x `elem` ys | x <- xs]


