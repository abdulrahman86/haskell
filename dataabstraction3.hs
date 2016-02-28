
--broken invariants
--unbalancing problem

--unabstract set using trees
module SetTreeUnabs
    (Set(Nil,Node), nil, insert, set, element, equal) where

data Set a = Nil | Node (Set a) a (Set a)

nil :: Set a
nil = Nil

list :: Set a -> [a]
list Nil = []
list (Node l x r) = (list l) ++ [x] ++ (list r)

invariant :: Ord a => Set a -> Bool
invariant Nil = True
invariant (Node l x r) = 
    invariant l && invariant r &&
    and [y < x | y <- list l] && 
    and [y > x | y <-list r]

insert :: Ord a => a -> Set a -> Set a
insert x Nil = Node Nil x Nil
insert x (Node l y r) 
    | x == y = Node l y r
    | x > y = Node l y (insert x r)
    | x < y = Node (insert x l) y r

set :: Ord a => [a] -> Set a
set = foldr insert nil

element :: Ord a => a -> Set a -> Bool
x `element` Nil = False
x `element` (Node l y r) 
    | x == y = True
    | x < y = x `element` l
    | otherwise = x `element` r

equal :: Ord a => Set a -> Set a -> Bool
s `equal` t = list s == list t


