 --Lazy Stream ds and functions

data Stream a = Elem a (() -> Stream a) 
              | Nil

makeStreamFromSingleton :: a -> Stream a
makeStreamFromSingleton a = Elem a (\_ -> Nil)

makeStream :: a -> Stream a -> Stream a
makeStream a Nil = makeStreamFromSingleton a
makeStream a1 (Elem a2 f) = Elem a1 (\_ -> Elem a2 f)

headStream :: Stream a -> a
headStream Nil = error "Invalid operation head of Nil stream"
headStream (Elem a f) = a

tailStream :: Stream a -> Stream a
tailStream Nil = error "Invalid operation head of Nil stream"
tailStream (Elem _ f) = f ()

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p Nil = Nil
filterStream p (Elem a f) = if (p a)
                               then Elem a (\_ -> filterStream p (f()))
                               else filterStream p (tailStream (Elem a f))

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream g Nil = Nil
mapStream g (Elem a f) = Elem (g a) (\_ -> mapStream g (f())) 

appendStream :: Stream a -> Stream a -> Stream a
appendStream Nil y = y
appendStream (Elem a f) y = Elem a (\_ -> appendStream (f()) y)


takeStream :: Int -> Stream a -> Stream a
takeStream 0 _ = Nil
takeStream n Nil =  Nil
takeStream n (Elem a f) = Elem a (\_ -> takeStream (n-1) (f()))

--stream to list is a terminal function
streamToList :: Stream a -> [a]
streamToList Nil = []
streamToList (Elem a f) = a : streamToList (f())


--findFirst is a terminal function
findFirst :: a -> (a -> Bool) -> Stream a -> a
findFirst a _ Nil = a
findFirst a p (Elem b f) | p b = b
                         | otherwise = findFirst a p (f())


--stream processing test
s1 = (makeStream 5 (makeStream 4 (makeStream 3 (makeStream 2(makeStreamFromSingleton 1)))))
s2 = filterStream (\x -> x > 2) s1
s3 = mapStream (\ a -> a + 1) s1
s4 = mapStream (\ a -> a + 1) s2
s5 = s4 `appendStream` s1




flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten(xs)

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f = flatten.(map f)

collect2 ::  (a -> b -> r) -> [a] -> [b] -> (a -> b -> Bool) -> [r]
collect2 result as bs p = flatMap (\a ->
                            flatMap (\b ->
                                            if (p a b)
                                                then [result a b]
                                                else []) bs
                            ) as

collect3 :: (a -> b -> c -> r) -> [a] -> [b] -> [c] -> (a -> b -> c -> Bool) -> [r]
collect3 result as bs cs p = flatMap (\a ->
                                    flatMap (\b ->
                                                   flatMap (\c ->
                                                                 if p a b c
                                                                    then [result a b c]
                                                                    else []
                                                            ) cs
                                        ) bs
                              )as

--all the pairs (a,b) st. a and b are between 1 and 3 and a < b
testCollect2 = collect2 (\x y -> (x,y))[1..3][1..3] (\i j -> i < j) 
testCollect3 = collect3  (\ i j k -> (i, j, k)) [1..3][1..3][1..3] (\ i j k -> i + j + k < 5)
