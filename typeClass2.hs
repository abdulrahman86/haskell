module Natural(Nat) where

--naturals from integers
 --preservation of invariants. 
 --difference between newtype and data. 
 
newtype Nat = MkNat Integer

invariant :: Nat -> Bool
invariant (MkNat x) = x >= 0

instance Eq Nat where 
    MkNat x == MkNat y = x == y

instance Ord Nat where
    MkNat x <= MkNat y = x <= y

instance Show Nat where
    show (MkNat x) = show x

instance Num Nat where
    MkNat x + MkNat y = MkNat (x + y)
    MkNat x - MkNat y 
        | x >= y = MkNat (x - y)
        | otherwise = error (show (x - y) ++ "i s negative")
    MkNat x * MkNat y = MkNat (x * y)
    fromInteger x
        | x >= 0 = MkNat (x)
        | otherwise = error (show x ++ " is negative")
    negate = undefined 

