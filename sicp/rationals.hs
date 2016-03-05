
module Rationals (Rat, makeRat, numer, denom) where 

import Scheme

type Rat = Pair Int Int

makeRat :: Int -> Int -> Rat
makeRat numer denom = cons numer denom

numer :: Rat -> Int
numer rat = car rat

denom :: Rat -> Int
denom rat = cdr rat

-- denom :: Rat -> Int 
-- denom (Cons x y) = y