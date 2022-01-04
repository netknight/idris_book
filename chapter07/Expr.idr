data Expr num = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

eval: (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger -- dot (.) - serves here for function composition

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = "( " ++ show x ++ " + " ++ show y ++ " )"
  show (Sub x y) = "( " ++ show x ++ " - " ++ show y ++ " )"
  show (Mul x y) = "( " ++ show x ++ " * " ++ show y ++ " )"
  show (Div x y) = "( " ++ show x ++ " / " ++ show y ++ " )"
  show (Abs x) = "| " ++ show x ++ " |"

(Integral ty, Neg ty, Abs ty, Eq ty) => Eq (Expr ty) where
  a == b = eval a == eval b

--https://github.com/idris-lang/Idris-dev/issues/3479
Cast ty ty where
      cast = id

(Integral ty, Neg ty, Abs ty, Cast ty num) => Cast (Expr ty) num where
  cast expr = cast (eval expr)

--

Functor Expr where
  map f (Val x) = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x) = Abs (map f x)

-- map (*2) (the (Expr _) (1 + 2 +3))
