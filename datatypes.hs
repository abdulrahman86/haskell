
--data type for natural numbers
data Nat = Zero 
         | Suc Nat
 
--addition function for natural number data type
add :: Nat -> Nat -> Nat
x `add` Zero = x 
x `add` Suc y = Suc (x `add` y)

--product function for natural number data type.
prod :: Nat -> Nat -> Nat
x `prod` Zero = Zero
x `prod` Suc y = x `add` (x `prod` y)

--power function for natural numbers
power  :: Float -> Nat -> Float
x `power` Zero = 1.0
x `power` Suc n = x * (x `power` n)

--data type for lists
data List a = Nil
            | Cons a  (List a)

append :: List a -> List a -> List a

append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)
--shape 

--type aliases
type Radius = Float
type Width = Float
type Height = Float


data Shape = Circle Radius | Rect Width Height


area :: Shape -> Float

area (Circle r) = pi * r^2
area (Rect w h) = w * h


--Expression Trees

data Exp = Lit Int
         | Exp :+: Exp
         | Exp :*: Exp

eval :: Exp -> Int

--function to evaluate integer expressions
eval (Lit n) = n
eval (x :+: y) = (eval x) + (eval y)
eval (x :*: y) = (eval x) * (eval y)