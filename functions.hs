(+++) :: Integer -> Integer -> Integer
---defininig an infix operator in haskell
x +++ y = x + y


odds :: [Integer] -> [Integer]
---example of guards in haskell
odds [] = []

odds (x:xs) | odd x = x : odds xs
            | otherwise = odds xs


max3 :: Int -> Int -> Int -> Int
--example of infix symbol operator
max3 x y z = x `max` y `max` z

sum1ToN :: Int -> Int
--Example of list comprehension
sum1ToN n = sum[x | x <- [1..n]]

factorialRec :: Int -> Int
--example of function with where clause
factorialRec n = fact 1 n 
    where 
    fact :: Int -> Int -> Int
    fact m n | m > n = 1 
             | m <= n = m * fact (m + 1) n

zipRec :: [a] -> [b] -> [(a,b)]
--zip function in haskell
zipRec xs [] = []
zipRec [] ys = []
zipRec (x:xs) (y:ys) = (x,y) : zip xs ys

dotProd :: Num a => [a] -> [a] -> a
dotProd xs ys = sum [x * y | (x, y) <- zip xs ys]

search :: Eq a => [a] -> a -> [Int]
--gives indexes of where element matches the list
search xs y = [i | (x,i) <- zip xs [0..], x ==y]

(!!!) :: [a] -> Int -> a
--returns element at specified index of the list
(x:xs) !!! i | (i == 0) = x
             | otherwise = xs !!! (i - 1) 

dropLst :: [a] -> Int -> [a]
--drops first y elements in the list
dropLst xs y = [x | (x, i) <- zip xs [0..], i >= y]
 

takeRec :: [a] -> Int -> [a]
--takes first i numbers from the list
takeRec xs 0 = []
takeRec [] i = []
takeRec (x:xs) i = x : takeRec xs (i-1) 


mapLst :: (a -> b) -> [a] -> [b]
--map for lists using list comprehension
mapLst f lst = [f a | a <- lst]

mapRec :: (a -> b) -> [a] -> [b]

mapRec f [] = []
mapRec f (x:xs) =  (f x) : mapRec f xs

sqLst :: Num a => [a] -> [a]
--squaring a list using a map funciton.
sqLst x = mapRec sq x
    where 
    sq :: Num a => a -> a
    sq x = x * x 

filterLst :: (a -> Bool) -> [a] -> [a]
--filter funciton using list comprehension
filterLst p xs = [ x | x <- xs, p x]

filterRec :: (a -> Bool) -> [a] -> [a]
--recursive filter function
filterRec p [] = []
filterRec p (x: xs) | p x = x : filterRec p xs
                    | otherwise = filterRec p xs

positive :: (Ord a, Num a) => [a] -> [a]
--positive number filter
positive xs = filterRec (>0) xs

foldRec :: (a -> a -> a) -> a -> [a] -> a
--recursive fold function
foldRec f a [] = a
foldRec f a (x : xs) = f x (foldRec f a xs) 

sumLst :: (Num a) => [a] -> a
sumLst xs = foldRec (+) 0 xs 

concatLst :: [[a]] -> [a]
--concat function using fold
concatLst xs = foldRec (++) [] xs  