data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

test = 
    let 
        add = \x -> \y -> case x of
                            Zero -> y
                            Succ u -> Succ (add u y)
    in
    
    let 
        two = Succ (Succ Zero)
        in 
        add two two