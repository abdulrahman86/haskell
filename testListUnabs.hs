module TestListUnabs where

import ListUnabs

test :: Int -> Bool 

test n = 
    s `equal` t
    where
    s = set[1, 2..n]
    t = set[n, n-1..1]

breakAbstraction :: Set a -> a
breakAbstraction = head