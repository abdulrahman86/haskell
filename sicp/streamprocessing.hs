data Stream a = Elem a (() -> Stream a) 
              | Nil

data Tree a = NilTree | Node a | Branch (Tree a) (Tree a)

instance Show a => Show (Stream a) where 
  show Nil = "Nil"
  show (Elem a f) = show a ++ "..."


enumerateTreeStream :: Tree a -> Stream a
enumerateTreeStream NilTree = Nil
enumerateTreeStream (Node a) = makeStreamFromSingleton a
enumerateTreeStream (Branch left right) = enumerateTreeStream left `appendStream` (delay (enumerateTreeStream right))
  where 
    appendStream :: Stream a -> (() -> Stream a)-> Stream a
    appendStream Nil y = y()
    appendStream (Elem a f) y = Elem a (delay (f() `appendStream` y))



makeStreamFromSingleton :: a -> Stream a
makeStreamFromSingleton a = Elem a (\_ -> Nil)

makeStream :: a -> Stream a -> Stream a
makeStream a Nil = makeStreamFromSingleton a
makeStream a1 (Elem a2 f) = Elem a1 (\_ -> Elem a2 f)

delay :: a -> (() -> a)
delay exp = \_ -> exp

force :: (() -> a) -> a
force f = f ()

headStream :: Stream a -> a
headStream Nil = error "Invalid operation head of Nil stream"
headStream (Elem a f) = a

tailStream :: Stream a -> Stream a
tailStream Nil = error "Invalid operation head of Nil stream"
tailStream (Elem _ f) = force f

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream p Nil = Nil
filterStream p (Elem a f) = if (p a)
                               then Elem a (delay (filterStream p (force f)))
                               else filterStream p (tailStream (Elem a f))

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream g Nil = Nil
mapStream g (Elem a f) = Elem (g a) (delay (mapStream g (force f)))

appendStream :: Stream a -> Stream a -> Stream a
appendStream Nil y = y
appendStream (Elem a f) y = Elem a (delay (appendStream (force f) y))


accumulate :: (a -> b -> b) -> Stream a -> b -> b
accumulate combiner stream initVal = case stream of Nil -> initVal
                                                    (Elem a f) -> combiner a (accumulate combiner (force f) initVal)


takeStream :: Int -> Stream a -> Stream a
takeStream 0 _ = Nil
takeStream n Nil =  Nil
takeStream n (Elem a f) = Elem a (delay (takeStream (n-1) (force f)))

--stream to list is a terminal function
streamToList :: Stream a -> [a]
streamToList Nil = []
streamToList (Elem a f) = a : streamToList (force f)

enumerateRange :: Int -> Int -> Stream Int
enumerateRange start end = if start > end 
                        then Nil 
                        else Elem start (delay (enumerateRange (start + 1) end))

enumerateList :: [a] -> Stream a
enumerateList list = case list of [] -> Nil
                                  (a : as) -> Elem a (\_ -> enumerateList as)


integersFrom :: Integer -> Stream Integer
integersFrom a = Elem a (delay (integersFrom (a+1)))



flattenStream :: Stream (Stream a) -> Stream a
flattenStream as = case as of Nil -> Nil
                              (Elem a f) -> a `appendStream` flattenStream (force f)


flatMapStream :: (a -> Stream b) -> Stream a -> Stream b
flatMapStream f = flattenStream.(mapStream f) 


collect2Stream ::  (a -> b -> r) -> Stream a -> Stream b -> (a -> b -> Bool) -> Stream r
collect2Stream result as bs p = flatMapStream (\a ->
                            flatMapStream (\b ->
                                            if (p a b)
                                                then makeStreamFromSingleton(result a b)
                                                else Nil) bs
                            ) as

collect3Stream :: (a -> b -> c -> r) -> Stream a -> Stream b -> Stream c -> (a -> b -> c -> Bool) -> Stream r
collect3Stream result as bs cs p = flatMapStream (\a ->
                                    flatMapStream (\b ->
                                                   flatMapStream (\c ->
                                                                 if p a b c
                                                                    then makeStreamFromSingleton(result a b c)
                                                                    else Nil
                                                            ) cs
                                        ) bs
                              )as


--findFirst is a terminal function
findFirst :: a -> (a -> Bool) -> Stream a -> a
findFirst a _ Nil = a
findFirst a p (Elem b f) | p b = b
                         | otherwise = findFirst a p (force f)



--n-th elem of stream

nthElem ::  a -> Int -> Stream a -> a

nthElem a _ Nil = a
nthElem _ 0 (Elem a f) = a
nthElem a n (Elem b f) = nthElem a (n-1) (force f) 


--sieve of eratosthenes

sieve :: (Num a, Eq a, Integral a) => Stream a -> Stream a

sieve Nil = Nil

sieve (Elem a f) = Elem a (delay (sieve (filterStream (\ x -> x `mod` a /= 0) (force f))))

fibs :: Stream Integer
fibs = Elem 0 (delay (Elem 1 (delay (addStreams fibs (tailStream fibs)))))


addStreams :: (Num a) => Stream a -> Stream a -> Stream a
addStreams Nil s = s
addStreams f Nil = f
addStreams (Elem a f1) (Elem b f2) = Elem (a+b) (delay (addStreams (force f1) (force f2)))


scaleStream :: (Integral a) => a -> Stream a -> Stream a
scaleStream c = mapStream (\x -> c * x)




--stream processing test
s1 = (makeStream 5 (makeStream 4 (makeStream 3 (makeStream 2(makeStreamFromSingleton 1)))))
s2 = filterStream (\x -> x > 2) s1
s3 = mapStream (\ a -> a + 1) s1
s4 = mapStream (\ a -> a + 1) s2
s5 = s4 `appendStream` s1


--some useful streams
ones = Elem 1 (\_ -> ones)

integers = Elem 1 (\_ -> addStreams ones integers)






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
