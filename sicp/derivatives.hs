data Exp = Const Int
         | Var String
         | Exp :*: Exp
         | Exp :+: Exp

instance Show Exp where
    show (Const x) = show x
    show (Var x) = show x

    show (Const x :+: Const y)  = show (x + y)
    show (Const 0 :+: x)  = show x
    show (x :+: Const 0)  = show x
    show (x :+: y) = "(" ++ show x ++ "+" ++ show y ++ ")"

    show (Const 0 :*: _)  = show 0
    show (_ :*: Const 0)  = show 0
    show (Const 1 :*: x)  = show x
    show (x :*: Const 1)  = show x
    show (Const x :*: Const y)  = show (x * y)
    show (x :*: y) = "(" ++ show x ++ "*" ++ show y ++ ")"

derivative :: Exp -> Exp -> Exp
derivative (Const a) (Var x) = Const 0
derivative (Var y) (Var x)
                            | y == x  = Const 1
                            | otherwise = Const 0

derivative (x :+: y) (Var z) = simplify ((derivative x (Var z)) :+: (derivative y (Var z)))
derivative (x :*: y) (Var z) = simplify(simplify((x :*: (derivative y (Var z)))) :+: simplify((y :*: (derivative x (Var z)))))


simplify :: Exp -> Exp 

simplify (Const x) = Const x
simplify (Var x) = Var x
simplify (Const x :+: Const y) = Const (x + y)
simplify (Const 0 :+: x) = x
simplify (x :+: Const 0) = x
simplify (Const 0 :*: x) = Const 0
simplify (x :*: Const 0) = Const 0
simplify (Const 1 :*: x) = x
simplify (x :*: Const 1) = x
simplify (Const x :*: Const y)  = Const (x * y)
simplify x = x




