(+++) :: Integer -> Integer -> Integer
---defininig an operator in haskell
x +++ y = x + y


odds :: [Integer] -> [Integer]
---example of guards in haskell
odds [] = []

odds (x:xs) | odd x = x : odds xs
            | otherwise = odds xs
