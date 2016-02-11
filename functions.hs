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
